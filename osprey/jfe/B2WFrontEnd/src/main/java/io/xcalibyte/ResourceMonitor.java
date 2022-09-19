/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.lang.management.ManagementFactory;
import java.lang.management.MemoryMXBean;
import java.lang.management.MemoryUsage;
import java.util.ArrayList;

class ResourceMonitor {
  static ArrayList<Resource> resourceList;
  static boolean enabled;
  private static Logger logger = LogManager.getLogger(ResourceMonitor.class);

  /* Class Resource */
  static class Resource {
    long   startTime;
    long   accmuTime;
    long   startMemory;	/* Memory used (bytes) */
    long   accmuMemory;
    String name;
    long   parentIdx;
    Resource() {
      startTime = 0;
      accmuTime = 0;
      startMemory = 0;
      accmuMemory = 0;
      name = null;
      parentIdx = -1;
    }
    Resource(String resName, long resParentIdx) {
      startTime = 0;
      accmuTime = 0;
      startMemory = 0;
      accmuMemory = 0;
      name = resName;
      parentIdx = resParentIdx;
    }

    void dump() {
      System.out.println(name + ":");
      System.out.println("    Elapsed Time:" + accmuTime +
                              "ms\t\tMemoryUsed:" + accmuMemory/1024 + "KB\t\tparentIdx:[" + parentIdx + "]" );

    }
  }


  /* Enum MonitorPhase */
  public static enum MonitorPhase {
    MONITOR_PHASE_MIN(0),
    MONITOR_PHASE_ALL(0),
    MONITOR_PHASE_SOOT(1),
    MONITOR_PHASE_GEN_WHIRL(2),
    MONITOR_PHASE_GEN_LIBRARY(3),
    MONITOR_PHASE_GEN_EH(4),
    MONITOR_PHASE_DUMP_BEFORE(5),
    MONITOR_PHASE_DUMP_AFTER(6),
    MONITOR_PAHSE_DAVA(7),
    MONITOR_PHASE_VERIFY(8),
    MONITOR_PHASE_VISIT_BODY(9),
    MONITOR_PHASE_VISIT_INVOKE(10),
    MONITOR_PHASE_MAX(10);

    private final int id;

    MonitorPhase(int id) {
      this.id = id;
    }

    public int toInt() {
      return id;
    }

    public String toString() {
      return String.valueOf(id);
    }
  }


  static void initialize(boolean enable) {
    enabled = enable;
    if(enable) {
      if(resourceList != null) {
        resourceList.clear();
      } else {
        resourceList = new ArrayList<Resource>();
      }

      Resource res = new Resource("Overall Time Memory", -1);
      resourceList.add(MonitorPhase.MONITOR_PHASE_ALL.toInt(), res);


      res = new Resource("Soot", MonitorPhase.MONITOR_PHASE_ALL.toInt());
      resourceList.add(MonitorPhase.MONITOR_PHASE_SOOT.toInt(), res);

      res = new Resource("Whirl Generation", MonitorPhase.MONITOR_PHASE_ALL.toInt());
      resourceList.add(MonitorPhase.MONITOR_PHASE_GEN_WHIRL.toInt(), res);

      res = new Resource("EH Generation", MonitorPhase.MONITOR_PHASE_GEN_WHIRL.toInt());
      resourceList.add(MonitorPhase.MONITOR_PHASE_GEN_EH.toInt(), res);

      res = new Resource("Jimple Dump Before", MonitorPhase.MONITOR_PHASE_GEN_WHIRL.toInt());
      resourceList.add(MonitorPhase.MONITOR_PHASE_DUMP_BEFORE.toInt(), res);

      res = new Resource("Jimple Dump After", MonitorPhase.MONITOR_PHASE_GEN_WHIRL.toInt());
      resourceList.add(MonitorPhase.MONITOR_PHASE_DUMP_AFTER.toInt(), res);

      res = new Resource("Dava Generation", MonitorPhase.MONITOR_PHASE_GEN_WHIRL.toInt());
      resourceList.add(MonitorPhase.MONITOR_PAHSE_DAVA.toInt(), res);

      res = new Resource("Body Verify", MonitorPhase.MONITOR_PHASE_GEN_WHIRL.toInt());
      resourceList.add(MonitorPhase.MONITOR_PHASE_VERIFY.toInt(), res);

      res = new Resource("Body Visit", MonitorPhase.MONITOR_PHASE_GEN_WHIRL.toInt());
      resourceList.add(MonitorPhase.MONITOR_PHASE_VISIT_BODY.toInt(), res);

      res = new Resource("Invoke Stmt Visit", MonitorPhase.MONITOR_PHASE_VISIT_BODY.toInt());
      resourceList.add(MonitorPhase.MONITOR_PHASE_VISIT_INVOKE.toInt(), res);
    }
  }

  static void start(MonitorPhase phase) {
    if(enabled) {
      Resource res = resourceList.get(phase.toInt());
      res.startTime = System.currentTimeMillis();

      MemoryMXBean memoryMXBean = ManagementFactory.getMemoryMXBean();
      MemoryUsage memoryUsage = memoryMXBean.getHeapMemoryUsage();
      res.startMemory = memoryUsage.getUsed();
    }
  }

  static void end(MonitorPhase phase) {
    if(enabled) {
      Resource res = resourceList.get(phase.toInt());
      res.accmuTime = res.accmuTime + (System.currentTimeMillis() - res.startTime);

      MemoryMXBean memoryMXBean = ManagementFactory.getMemoryMXBean();
      MemoryUsage memoryUsage = memoryMXBean.getHeapMemoryUsage();
      // memory may be GCed
      if(memoryUsage.getUsed() > res.startMemory) {
        res.accmuMemory = res.accmuMemory + memoryUsage.getUsed() - res.startMemory;
      }
    }
  }

  static long getTimeUsage(MonitorPhase phase) {
    if(enabled) {
      Resource res = resourceList.get(phase.toInt());
      return res.accmuTime;
    } else {
      return -1;
    }
  }

  static long getMemoryUsage(MonitorPhase phase) {
    if(enabled) {
      Resource res = resourceList.get(phase.toInt());
      return res.accmuMemory;
    } else {
      return -1;
    }
  }

  static void dumpUsage(MonitorPhase phase) {
    if(enabled) {
      Resource res = resourceList.get(phase.toInt());
      System.out.print("[" + phase.toInt() + "] ");
      res.dump();
    } else {
      logger.debug("Resource Monitor not Enabled");
    }
  }

  static void dumpAllUsage() {
    if(enabled) {
      for (int resIdx = MonitorPhase.MONITOR_PHASE_MIN.toInt(); resIdx <= MonitorPhase.MONITOR_PHASE_MAX.toInt(); resIdx++) {
        Resource res = resourceList.get(resIdx);
        System.out.print("[" + resIdx + "] ");
        res.dump();
      }
      long totalTime = resourceList.get(MonitorPhase.MONITOR_PHASE_ALL.toInt()).accmuTime;
      long sootTime = resourceList.get(MonitorPhase.MONITOR_PHASE_SOOT.toInt()).accmuTime;
      float percentage = sootTime * 1.0f / totalTime;
      System.out.println(String.format(
        "[#] Soot usage percentage : %.2f%%", percentage * 100)
      );
    } else {
      logger.debug("Resource Monitor not Enabled");
    }
  }

}

