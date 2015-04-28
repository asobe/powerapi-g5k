/*
 * This software is licensed under the GNU Affero General Public License, quoted below.
 *
 * This file is a part of PowerAPI.
 *
 * Copyright (C) 2011-2014 Inria, University of Lille 1.
 *
 * PowerAPI is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * PowerAPI is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with PowerAPI.
 *
 * If not, please consult http://www.gnu.org/licenses/agpl-3.0.html.
 */
package org.powerapi.hpc.analysis

import java.io.{PrintWriter, FileOutputStream, File}
import java.util.concurrent.TimeUnit
import akka.actor.{Props, ActorSystem, ActorLogging, Actor}
import com.typesafe.config.Config
import org.apache.logging.log4j.LogManager
import org.apache.commons.math.stat.correlation.PearsonsCorrelation
import org.powerapi.PowerMeter
import org.powerapi.core.target.All
import org.powerapi.core.power.MEAN
import org.powerapi.core.{ConfigValue, Configuration}
import org.powerapi.module.PowerChannel.AggregatePowerReport
import org.powerapi.module.libpfm.{LibpfmHelper, LibpfmCoreSensorModule}
import org.powerapi.module.libpfm.PerformanceCounterChannel.{PCReport, subscribePCReport}
import org.powerapi.module.powerspy.PowerSpyModule
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.sys.process.ProcessLogger
import scala.util.Sorting
import scalax.file.Path
import scalax.file.PathMatcher.IsDirectory
import scala.collection.JavaConversions._
import scala.concurrent.duration.{DurationInt, DurationLong}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.sys.process.stringSeqToProcess
import scala.sys

object RENEW

/**
 * Define specific kinds of reporters to be sure that all data are written inside files.
 */
class PowersDisplay(filepath: String) extends org.powerapi.core.APIComponent {
  var output = new PrintWriter(new FileOutputStream(new File(filepath), true))

  override def postStop(): Unit = {
    output.close()
    super.postStop()
  }

  def receive: Actor.Receive = {
    case msg: AggregatePowerReport => report(msg)
    case msg: String => append(msg)
    case RENEW => {
      output.close()
      Path(s"$filepath", '/').delete(true)
      output = new PrintWriter(new FileOutputStream(new File(filepath), true))
    }
  }

  def report(msg: AggregatePowerReport): Unit = {
    output.append(s"${msg.power.toWatts}\n")
    output.flush()
  }

  def append(msg: String): Unit = {
    output.append(s"$msg\n")
    output.flush()
  }
}

class CountersDisplay(mapping: Map[String, String], events: List[String]) extends Actor with ActorLogging  {
  var outputs = (for(event <- events) yield {
    event -> new PrintWriter(new FileOutputStream(new File(mapping(event)), true))
  }).toMap

  override def postStop(): Unit = {
    outputs.foreach {
      case (_, writer) => writer.close()
    }
    super.postStop()
  }

  def receive: Actor.Receive = {
    case msg: PCReport => report(msg)
    case msg: String => append(msg)
    case RENEW => {
      outputs.foreach {
        case (event, writer) => {
          writer.close()
          Path(mapping(event), '/').delete(true)
        }
      }

      outputs = (for(event <- events) yield {
        event -> new PrintWriter(new FileOutputStream(new File(mapping(event)), true))
      }).toMap
    }
  }

  def report(msg: PCReport): Unit = {
    for((event, wrappers) <- msg.wrappers.groupBy(_.event)) {
      val future = Future.sequence(wrappers.foldLeft(List[Future[Long]]())((acc, elt) => acc ++ elt.values))

      future onSuccess {
        case values: List[Long] => {
          val counter = values.foldLeft(0l)((acc, value) => acc + value)
          outputs(event).append(s"$counter\n")
          outputs(event).flush()
        }
      }

      future onFailure {
        case ex: Throwable => {
          log.warning("An error occurred: {}", ex.getMessage)
        }
      }
    }
  }

  def append(msg: String): Unit = {
    outputs.values.foreach(output => {
      output.append(s"$msg\n")
      output.flush()
    })
  }
}

case class Workload(path: String, benchmarks: List[String], scriptPhase1: String, scriptPhase2: String)

object Application extends App with Configuration {

  @volatile var powerapi: Option[PowerMeter] = None
  @volatile var externalPMeter: Option[PowerMeter] = None

  val shutdownHookThread = scala.sys.ShutdownHookThread {
    println("It's the time for sleeping! ...")
    powerapi match {
      case Some(papi) => papi.shutdown()
      case _ => {}
    }
    externalPMeter match {
      case Some(exPMeter) => exPMeter.shutdown()
      case _ => {}
    }
    org.powerapi.module.libpfm.LibpfmHelper.deinit()
  }

  lazy val topology: Map[Int, Set[Int]] = load { conf =>
    (for (item: Config <- conf.getConfigList("powerapi.cpu.topology"))
      yield (item.getInt("core"), item.getDoubleList("indexes").map(_.toInt).toSet)).toMap
  } match {
    case ConfigValue(values) => values
    case _ => Map()
  }

  lazy val interval: FiniteDuration = load { _.getDuration("powerapi.hpc-analysis.interval", TimeUnit.NANOSECONDS) } match {
    case ConfigValue(value) => value.nanoseconds
    case _ => 1.seconds
  }

  lazy val workloads: List[Workload] = load { conf =>
    (for (item: Config <- conf.getConfigList("powerapi.hpc-analysis.workloads"))
      yield Workload(item.getString("path"), item.getStringList("benchmarks").toList, item.getString("script-phase1"), item.getString("script-phase2"))).toList
  } match {
    case ConfigValue(values) => values
    case _ => List()
  }

  LibpfmHelper.init()
  Seq("bash", "-c", "chmod +x scripts/*").!

  val trash = ProcessLogger(out => {}, err => {})
  private val log = LogManager.getLogger
  val PSFormat = """\s*([\d]+)\s.*""".r

  val begin = System.nanoTime
  var allEvents = LibpfmHelper.detectedEvents().toList
  val nbEventPerSubset = 4
  var subEvents = Seq[String]()
  var beg, end, lastPid, iSubset = 0

  val separator = "="
  val outputPowers = "output-powers.dat"
  val baseOutputCounter = "output-"

  val eventsMapping = (for(event <- allEvents) yield event -> s"$baseOutputCounter${event.toLowerCase().replace('_', '-').replace(':', '-')}.dat").toMap

  var hpcsWhitelistPhase1 = Map[String, Array[(String, Double)]]()
  var hpcsBlacklistPhase1 = Map[String, Array[String]]()
  var hpcsWhitelistPhase2 = Map[String, Array[(String, Double)]]()
  var hpcsBlacklistPhase2 = Map[String, Array[String]]()

  (Path(".") * "*.dat").foreach(path => path.delete(force = true))

  /**
   * We keep the connexion opened.
   */
  externalPMeter = Some(PowerMeter.loadModule(PowerSpyModule()))

  /**
   * Special actor system and writers.
   */
  val writersSys = ActorSystem("writers")

  /**
   * Phase 1 - Remove several counters that are not relevant
   */
  Path("phase1", '/').deleteRecursively(force = true)

  for(workload <- workloads) {
    log.info("Phase 1")

    for(benchmark <- workload.benchmarks) {
      log.info("benchmark: {}", benchmark)

      beg = 0
      end = beg + nbEventPerSubset
      subEvents = allEvents.slice(beg, end)

      while(subEvents.nonEmpty) {
        powerapi = Some(PowerMeter.loadModule(LibpfmCoreSensorModule(subEvents.toSet)))
        val powerapiDisplay = writersSys.actorOf(Props(classOf[CountersDisplay], eventsMapping, subEvents), "output-cpu")
        val externalPMeterDisplay = writersSys.actorOf(Props(classOf[PowersDisplay], s"$outputPowers"), "output-powers")

        var allExPMeter = externalPMeter.get.monitor(interval)(All)(MEAN).to(externalPMeterDisplay)
        var allPapi = powerapi.get.monitor(interval)(All)(MEAN).to(powerapiDisplay, subscribePCReport)
        Thread.sleep(5.seconds.toMillis)
        allExPMeter.cancel()
        allPapi.cancel()
        Thread.sleep(2.seconds.toMillis)

        allExPMeter = externalPMeter.get.monitor(interval)(All)(MEAN).to(externalPMeterDisplay)
        allPapi = powerapi.get.monitor(interval)(All)(MEAN).to(powerapiDisplay, subscribePCReport)
        Seq(s"./${workload.scriptPhase1}", s"${workload.path}", benchmark).!
        allExPMeter.cancel()
        allPapi.cancel()

        writersSys.stop(externalPMeterDisplay)
        writersSys.stop(powerapiDisplay)
        powerapi.get.shutdown()
        powerapi = None

        // Move all the files associated to this subset.
        iSubset = beg / nbEventPerSubset

        Path(s"phase1/$benchmark/subset-$iSubset", '/').createDirectory()
        (Path(".", '/') * "*.dat").foreach(path => {
          path.moveTo(Path(s"phase1/$benchmark/subset-$iSubset/${path.name}", '/'), true)
        })

        beg += nbEventPerSubset
        end += nbEventPerSubset
        subEvents = allEvents.slice(beg, end)
        Thread.sleep(5.seconds.toMillis)
      }

      /**
       * Remove a counter when the pearson coefficient is smaller than a threshold.
       */
      val pearsonUtil = new PearsonsCorrelation()
      for(subsetPath <- Path(s"phase1/$benchmark", '/') ** IsDirectory) {
        val hpcFilepaths = (subsetPath * "*.dat").filter(_.name != outputPowers)
        val powersData = (subsetPath / outputPowers).lines().filter(_ != "\n").map(_.toDouble)

        for(hpcFilepath <- hpcFilepaths) {
          val hpcData = hpcFilepath.lines().filter(_ != "\n").map(_.toDouble)
          val minSize = math.min(powersData.size, hpcData.size)
          val pearson = pearsonUtil.correlation(hpcData.slice(0, minSize).toArray, powersData.slice(0, minSize).toArray)

          if(pearson > 0.5) {
            hpcsWhitelistPhase1 += benchmark -> (hpcsWhitelistPhase1.getOrElse(benchmark, Array[(String, Double)]()) :+ (eventsMapping.filter(_._2 == hpcFilepath.name).head._1, pearson))
          }
          else hpcsBlacklistPhase1 += benchmark -> (hpcsBlacklistPhase1.getOrElse(benchmark, Array[String]()) :+ eventsMapping.filter(_._2 == hpcFilepath.name).head._1)
        }
      }

      Sorting.quickSort(hpcsWhitelistPhase1(benchmark))(Ordering.by[(String, Double), Double](_._2).reverse)
      log.info("Number of selected counters for {}: {} in the first phase", benchmark, s"${hpcsWhitelistPhase1(benchmark).size}")
      log.info("Number of removed counters for {}: {} in the first phase", benchmark, s"${hpcsBlacklistPhase1(benchmark).size}")
      log.info("Removed counters after the first phase: {}", hpcsBlacklistPhase1(benchmark).mkString(","))
      log.info("Remaining counters after the first phase: {}", hpcsWhitelistPhase1(benchmark).mkString(","))
      log.info("*******")
    }
  }

  /**
   * Phase 2 - Try to delete more counters with an extended experiment.
   */
  (Path(".") * "*.dat").foreach(path => path.delete(force = true))
  Path("phase2", '/').deleteRecursively(force = true)

  for(workload <- workloads) {
    log.info("Phase 2")

    for(benchmark <- workload.benchmarks) {
      log.info("benchmark: {}", benchmark)

      beg = 0
      end = beg + nbEventPerSubset

      allEvents = (for(tuple <- hpcsWhitelistPhase1(benchmark)) yield tuple._1).toList
      subEvents = allEvents.slice(beg, end)

      while(subEvents.nonEmpty) {
        powerapi = Some(PowerMeter.loadModule(LibpfmCoreSensorModule(subEvents.toSet)))
        val powerapiDisplay = writersSys.actorOf(Props(classOf[CountersDisplay], eventsMapping, subEvents), "output-cpu")
        val externalPMeterDisplay = writersSys.actorOf(Props(classOf[PowersDisplay], s"$outputPowers"), "output-powers")

        var allExPMeter = externalPMeter.get.monitor(interval)(All)(MEAN).to(externalPMeterDisplay)
        var allPapi = powerapi.get.monitor(interval)(All)(MEAN).to(powerapiDisplay, subscribePCReport)
        Thread.sleep(5.seconds.toMillis)
        allExPMeter.cancel()
        allPapi.cancel()
        Thread.sleep(2.seconds.toMillis)

        allExPMeter = externalPMeter.get.monitor(interval)(All)(MEAN).to(externalPMeterDisplay)
        allPapi = powerapi.get.monitor(interval)(All)(MEAN).to(powerapiDisplay, subscribePCReport)
        Seq(s"./${workload.scriptPhase2}", s"${workload.path}", benchmark).!
        allExPMeter.cancel()
        allPapi.cancel()

        writersSys.stop(externalPMeterDisplay)
        writersSys.stop(powerapiDisplay)
        powerapi.get.shutdown()
        powerapi = None

        // Move all the files associated to this subset.
        iSubset = beg / nbEventPerSubset

        Path(s"phase2/$benchmark/subset-$iSubset", '/').createDirectory()
        (Path(".", '/') * "*.dat").foreach(path => {
          path.moveTo(Path(s"phase2/$benchmark/subset-$iSubset/${path.name}", '/'), true)
        })

        beg += nbEventPerSubset
        end += nbEventPerSubset
        subEvents = allEvents.slice(beg, end)
        Thread.sleep(5.seconds.toMillis)
      }

      /**
       * Remove a counter when the pearson coefficient is smaller than a threshold.
       */
      val pearsonUtil = new PearsonsCorrelation()
      for(subsetPath <- Path(s"phase2/$benchmark", '/') ** IsDirectory) {
        val hpcFilepaths = (subsetPath * "*.dat").filter(_.name != outputPowers)
        val powersData = (subsetPath / outputPowers).lines().filter(_ != "\n").map(_.toDouble)

        for(hpcFilepath <- hpcFilepaths) {
          val hpcData = hpcFilepath.lines().filter(_ != "\n").map(_.toDouble)
          val minSize = math.min(powersData.size, hpcData.size)
          val pearson = pearsonUtil.correlation(hpcData.slice(0, minSize).toArray, powersData.slice(0, minSize).toArray)

          if(pearson > 0.5) {
            hpcsWhitelistPhase2 += benchmark -> (hpcsWhitelistPhase2.getOrElse(benchmark, Array[(String, Double)]()) :+ (eventsMapping.filter(_._2 == hpcFilepath.name).head._1, pearson))
          }
          else hpcsBlacklistPhase2 += benchmark -> (hpcsBlacklistPhase2.getOrElse(benchmark, Array[String]()) :+ eventsMapping.filter(_._2 == hpcFilepath.name).head._1)
        }
      }

      Sorting.quickSort(hpcsWhitelistPhase2(benchmark))(Ordering.by[(String, Double), Double](_._2).reverse)
      log.info("Number of selected counters for {}: {} in the second phase", benchmark, s"${hpcsWhitelistPhase2(benchmark).size}")
      log.info("Number of removed counters for {}: {} in the second phase", benchmark, s"${hpcsBlacklistPhase2(benchmark).size}")
      log.info("Removed counters after the second phase: {}", hpcsBlacklistPhase2(benchmark).mkString(","))
      log.info("Remaining counters after the second phase: {}", hpcsWhitelistPhase2(benchmark).mkString(","))
      log.info("*******")
    }
  }

  shutdownHookThread.start()
  shutdownHookThread.join()
  shutdownHookThread.remove()
  sys.exit(0)
}
