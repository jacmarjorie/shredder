package framework.plans

import org.scalatest.FunSuite
import framework.common._
import framework.examples.tpch._
import framework.examples.genomic._
import framework.nrc._
import scala.collection.immutable.{Map => IMap}
import scala.collection.mutable.{HashMap, Map}
import framework.plans.{Equals => CEquals, Project => CProject}

class TestCachePlanners extends TestBase {

  val compileCost = false
  
  test("standard comilation route"){

    println("STANDARD COMPILATION ROUTE!!!")

    val seBuilder = SEBuilder(sprogs)
    seBuilder.updateSubexprs()

    val subs = seBuilder.sharedSubs()

    val ces = CEBuilder.buildCoverMap(subs)
    
    // printCE(ces)

    val statsCollector = new StatsCollector(progs)
    val stats = statsCollector.getCost(subs, ces)

    val cost = new Cost(stats)

    // println("FLEXIBILITY 0")
    val selected0 = cost.selectCovers(ces, subs)

    // selected0.foreach{ s =>
    //   println(s._1)
    //   println(s._2.profit)
    //   println(Printer.quote(s._2.plan))
    // }

    val totalSize = selected0.map(s => s._2.est.outSize).reduce(_+_)
    // println(totalSize)
    // total size is 4502.043333333335 (4/5)
    val planner0 = new GreedyCachePlanner(selected0, 5000)
    planner0.solve()

    val candidates0 = planner0.knapsack
    println("these were put in the knapsack from most strict - greedy")
    candidates0.foreach{ c => 
      println(Printer.quote(c._2))
    }

    val planner1 = new DynamicCachePlanner(selected0)
    val results1 = planner1.solve(5000)

    val candidates1 = results1.selected
    println("these were put in the knapsack from most strict - dynamic")
    candidates1.foreach{ c => 
      println(Printer.quote(c._2))
    }


  }

  test("shred comilation route"){

    println("SHREDDED COMPILATION ROUTE!!!")
    
    val seBuilder = SEBuilder(sprogs)
    seBuilder.updateSubexprs()

    val subs = seBuilder.sharedSubs()
    // printSE(subs)

    val ces = CEBuilder.buildCoverMap(subs)
    // printCE(ces)
    
    val statCollector = new StatsCollector(sprogs)
    val stats = statCollector.getCost(subs, ces)

    val cost = new Cost(stats)
    val selected0 = cost.selectCovers(ces, subs)

    // selected0.foreach{ s =>
    //   println(s._1)
    //   println(s._2.profit)
    //   println(Printer.quote(s._2.plan))
    // }

    val totalSize = selected0.map(s => s._2.est.outSize).reduce(_+_)
    // println(totalSize)
    // total size is 4502.043333333335 (4/5)
    val planner0 = new GreedyCachePlanner(selected0, 5000)
    planner0.solve()

    val candidates0 = planner0.knapsack
    println("these were put in the knapsack from most strict - greedy")
    candidates0.foreach{ c => 
      println(Printer.quote(c._2))
    }

    val planner1 = new DynamicCachePlanner(selected0)
    val results1 = planner1.solve(5000)

    val candidates1 = results1.selected
    println("these were put in the knapsack from most strict - dynamic")
    candidates1.foreach{ c => 
      println(Printer.quote(c._2))
    }

  }

}