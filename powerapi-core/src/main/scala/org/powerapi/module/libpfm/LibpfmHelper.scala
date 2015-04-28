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
package org.powerapi.module.libpfm

import org.apache.logging.log4j.LogManager
import org.bridj.Pointer.{allocateCLongs, pointerTo, pointerToCString}
import perfmon2.libpfm.{pfm_pmu_info_t, perf_event_attr, pfm_perf_encode_arg_t, LibpfmLibrary, pfm_event_info_t, pfm_event_attr_info_t}
import perfmon2.libpfm.LibpfmLibrary.{pfm_attr_t, pfm_pmu_t, pfm_os_t}
import scala.collection.BitSet

/**
 * Internal wrappers
 *
 * @author <a href="mailto:maxime.colmant@gmail.com">Maxime Colmant</a>
 */
trait Identifier
case class TID(identifier: Int) extends Identifier
case class CID(core: Int) extends Identifier
case class TCID(identifier: Int, core: Int) extends Identifier

/**
 * This object allows us to interact with the Libpfm library (C Library).
 * We use jnaerator and bridj to create the binding.
 *
 * @see https://github.com/ochafik/nativelibs4java
 * @see http://www.man7.org/linux/man-pages/man2/perf_event_open.2.html.
 *
 * @author <a href="mailto:maxime.colmant@gmail.com">Maxime Colmant</a>
 */
object LibpfmHelper {
  private val format = LibpfmLibrary.perf_event_read_format.PERF_FORMAT_TOTAL_TIME_ENABLED.value().toInt | LibpfmLibrary.perf_event_read_format.PERF_FORMAT_TOTAL_TIME_RUNNING.value.toInt
  private var initialized = false

  private val log = LogManager.getLogger

  /**
   * Implicit conversion BitSet to Long
   */
  implicit def BitSet2Long(value: BitSet): Long = {
    // We limit the size of the bitset (see the documentation on perf_event.h, only 23 bits for the config.)
    // The other 41 bits are reserved.
    value.range(0, 23).foldLeft(0l)((acc, index) => acc + (1L << index))
  }

  /**
   * Init. libpfm
   */
  def init(): Boolean = {
    if(!initialized) {
      val ret = LibpfmLibrary.pfm_initialize()

      if(ret == LibpfmLibrary.PFM_SUCCESS) {
        initialized = true
        true
      }
      else {
        initialized = false
        throw new RuntimeException("Libpfm cannot be initialized.")
      }
    }
    else {
      log.debug("Libpfm is already initialized.")
      true
    }
  }

  /**
   * Deinit. libpfm
   */
  def deinit(): Unit = {
    if(initialized) {
      LibpfmLibrary.pfm_terminate()
      initialized = false
    }
  }

  /**
   * Open a file descriptor with the given configuration options.
   *
   * @param identifier: identifier used to open the counter
   * @param configuration: bits configuration
   * @param name: name of the performance counter to open
   */
  def configurePC(identifier: Identifier, configuration: BitSet, name: String): Option[Int] = {
    val cName = pointerToCString(name)
    val argEncoded = new pfm_perf_encode_arg_t
    val argEncodedPointer = pointerTo(argEncoded)
    val eventAttr = new perf_event_attr
    val eventAttrPointer = pointerTo(eventAttr)

    argEncoded.attr(eventAttrPointer)

    // Get the specific event encoding for the OS.
    // PFM_PLM3: measure at user level (including PFM_PLM2, PFM_PLM1).
    // PFM_PLM0: measure at kernel level.
    // PFM_PLMH: measure at hypervisor level.
    // PFM_OS_PERF_EVENT_EXT is used to extend the default perf_event library with libpfm.
    val ret = LibpfmLibrary.pfm_get_os_event_encoding(cName, LibpfmLibrary.PFM_PLM0|LibpfmLibrary.PFM_PLM3|LibpfmLibrary.PFM_PLMH, pfm_os_t.PFM_OS_PERF_EVENT, argEncodedPointer)

    if(ret == LibpfmLibrary.PFM_SUCCESS) {
      // Set the bits in the structure.
      eventAttr.read_format(format)
      eventAttr.bits_config(configuration: Long)

      // Open the file descriptor.
      val fd = identifier match {
        case TID(tid) => CUtils.perf_event_open(eventAttrPointer, tid, -1, -1, 0)
        case CID(cid) => CUtils.perf_event_open(eventAttrPointer, -1, cid, -1, 0)
        case TCID(tid, cid) => CUtils.perf_event_open(eventAttrPointer, tid, cid, -1, 0)
        case _ => {
          log.error("The type of the first parameter is unknown.")
          -1
        }
      }

      if(fd > 0) {
        Some(fd)
      }

      else {
        log.warn("Libpfm is not able to open a counter for the event {}.", name)
        None
      }
    }

    else {
      log.warn("Libpfm cannot initialize the structure for this event.")
      None
    }
  }

  /**
   * Reset the performance counter represented by a file descriptor.
   */
  def resetPC(fd: Int): Boolean = {
    CUtils.ioctl(fd, LibpfmLibrary.PERF_EVENT_IOC_RESET) == 0
  }

  /**
   * Enable the performance counter represented by a file descriptor.
   */
  def enablePC(fd: Int): Boolean = {
    CUtils.ioctl(fd, LibpfmLibrary.PERF_EVENT_IOC_ENABLE) == 0
  }

  /**
   * Disable the performance counter represented by a file descriptor.
   */
  def disablePC(fd: Int): Boolean = {
    CUtils.ioctl(fd, LibpfmLibrary.PERF_EVENT_IOC_DISABLE) == 0
  }

  /**
   * Close the performance counter represented by a file descriptor.
   */
  def closePC(fd: Int): Boolean = {
    CUtils.close(fd) == 0
  }

  /**
   * Read the values from the performance counter represented by a file descriptor.
   */
  def readPC(fd: Int): Array[Long] = {
    val counts = allocateCLongs(3)
    // 8 bytes * 3 longs
    if(CUtils.read(fd, counts, 8 * 3) > -1) {
      counts.getCLongs
    }
    else Array(0L, 0L, 0L)
  }

  /**
   * Allows to scale the values read from a performance counter by applying a ratio between the enabled/running times.
   */
  def scale(now: Array[Long], old: Array[Long]): Option[Long] = {
    /* [0] = raw count
     * [1] = TIME_ENABLED
     * [2] = TIME_RUNNING
     */
    if(now(2) == 0 && now(1) == 0 && now(0) != 0) {
      log.warn("time_running = 0 = time_enabled, raw count not zero.")
      None
    }

    else if(now(2) > now(1)) {
      log.warn("time_running > time_enabled.")
      None
    }

    else if(now(2) - old(2) > 0) {
      Some(((now(0) - old(0)) * ((now(1) - old(1)) / (now(2) - old(2))).toDouble).round)
    }

    else None
  }

  /**
   * Get the list of cpu events.
   */
  def detectedEvents(): Set[String] = {
    lazy val detectedPmus = pmus()
    (for(pmu <- detectedPmus) yield events(pmu)).flatten.toSet
  }

  /**
   * PMUs detected on the processor.
   * All the generic PMUs are removed because they used the specifics ones for the encoding.
   */
  private def pmus(): Seq[pfm_pmu_info_t] = {
    // See the outputs of ./check_events from the examples folder in the libpfm library.
    val generics = Array(pfm_pmu_t.PFM_PMU_INTEL_X86_ARCH, pfm_pmu_t.PFM_PMU_PERF_EVENT, pfm_pmu_t.PFM_PMU_PERF_EVENT_RAW)
    val allSupportedPMUS = pfm_pmu_t.values().to[scala.collection.mutable.ArrayBuffer] -- generics
    var activePMUS = Seq[pfm_pmu_info_t]()

    // The bit is_present is checked to know if a PMU is available or not. A shift is done because of a jnaerator/bridj limitation with bit fields struct.
    for(pmu <- allSupportedPMUS) {
      val pinfo = new pfm_pmu_info_t
      val pinfoPointer = pointerTo(pinfo)
      val ret = LibpfmLibrary.pfm_get_pmu_info(pmu, pinfoPointer)

      if(ret == LibpfmLibrary.PFM_SUCCESS && ((pinfo.bits_def >> 32) & 1) == 1) {
        activePMUS :+= pinfo
      }
    }

    activePMUS
  }

  /**
   * Events attached to a PMU (with different UMASK).
   */
  private def events(pmu: pfm_pmu_info_t): Seq[String] = {
    val einfo = new pfm_event_info_t
    val einfoPointer = pointerTo(einfo)
    var index = pmu.first_event
    var events = Seq[String]()

    while(index != -1) {
      if(LibpfmLibrary.pfm_get_event_info(index, pfm_os_t.PFM_OS_PERF_EVENT, einfoPointer) == LibpfmLibrary.PFM_SUCCESS) {
        // If there is no equivalent event, we can keep the event.
        if(einfo.equiv == null) {
          val eventName = einfo.name.getCString

          // We keep only the events with at least one UMASK. The event with only modifiers are shortcuts.
          for(i <- 0 until einfo.nattrs) {
            val ainfo = new pfm_event_attr_info_t
            val ainfoPointer = pointerTo(ainfo)
            val ret = LibpfmLibrary.pfm_get_event_attr_info(einfo.idx, i, pfm_os_t.PFM_OS_PERF_EVENT, ainfoPointer)

            if(ret == LibpfmLibrary.PFM_SUCCESS) {
              // Filter on the type because it could be also a MODIFIER.
              if(ainfo.`type`.value == pfm_attr_t.PFM_ATTR_UMASK.value) {
                events :+= eventName + ":" + ainfo.name.getCString
              }
            }
          }
        }

        index = LibpfmLibrary.pfm_get_event_next(index)
      }
    }

    events
  }
}
