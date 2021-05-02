package framework.plans

import org.scalatest.FunSuite
import framework.common._
import framework.examples.tpch._
import framework.examples.genomic._
import framework.nrc._
import framework.plans.{Equals => CEquals, Project => CProject}
import scala.collection.mutable.HashMap

class TestStatsCollector extends TestBase {

  val run = false

  test("standard comilation route"){

    val seBuilder = SEBuilder(sprogs)
    seBuilder.updateSubexprs()

    val subs = seBuilder.sharedSubs()

    val ces = CEBuilder.buildCoverMap(subs)
    
    val statsCollector = new StatsCollector(progs)
    // val stats = statsCollector.getCost(subs, ces)

    // assert(stats.size == 26)

  }

  test("shred comilation route"){

    val seBuilder = SEBuilder(sprogs)
    seBuilder.updateSubexprs()

    val subs = seBuilder.sharedSubs()
    // printSE(subs)

    val ces = CEBuilder.buildCoverMap(subs)
    printCE(ces)
    
    val statCollector = new StatsCollector(sprogs)
    // val stats = statCollector.getCost(subs, ces)
    // assert(stats.size == 24)

  }

}