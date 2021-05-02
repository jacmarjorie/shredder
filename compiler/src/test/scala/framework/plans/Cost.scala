package framework.plans

import org.scalatest.FunSuite
import framework.common._
import framework.examples.tpch._
import framework.examples.genomic._
import framework.nrc._
import scala.collection.immutable.{Map => IMap}
import scala.collection.mutable.{HashMap, Map}
import framework.plans.{Equals => CEquals, Project => CProject}

class TestCost extends TestBase {

  test("standard comilation route"){

    val seBuilder = SEBuilder(progs)
    seBuilder.updateSubexprs()

    val subs = seBuilder.sharedSubs()

    printSE(subs)
    val ces = CEBuilder.buildCoverMap(subs)
    
    val statsCollector = new StatsCollector(progs)
    val stats = statsCollector.getCost(subs, ces)

    val cost = new Cost(stats)
    val selectedCovers = cost.selectCovers(ces, subs)

    selectedCovers.foreach{ s =>
      println(s._1)
      println(s._2.profit)
      println(Printer.quote(s._2.plan))
    }

    // cost.printEstimateAndStat(ces, subs)

    // assert(stats.size == 26)

  }

  test("shred comilation route"){

    val seBuilder = SEBuilder(sprogs)
    seBuilder.updateSubexprs()

    val subs = seBuilder.sharedSubs()
    // printSE(subs)

    val ces = CEBuilder.buildCoverMap(subs)
    // printCE(ces)
    
    val statCollector = new StatsCollector(sprogs)
    val stats = statCollector.getCost(subs, ces)

    val cost = new Cost(stats)
    val selectedCovers = cost.selectCovers(ces, subs)

    selectedCovers.foreach{ s =>
      println(s._1)
      println(s._2.profit)
      println(Printer.quote(s._2.plan))
    }

  }

  test("nest test"){
    
    val joinQuery1 = parser.parse(
      """
        for c in Customer union
          if (c.c_name = "test1")
          then for o in Order union 
            if (c.c_custkey = o.o_custkey)
            then {( cname := c.c_name, orderkey := o.o_orderkey )}
      """, parser.term).get
    val joinPlan1 = getPlan(joinQuery1.asInstanceOf[Expr]).asInstanceOf[CExpr]
    
    val joinQuery2 = parser.parse(
      """
        for c in Customer union 
          if (c.c_name = "test2")
          then for o in Order union
            if (c.c_custkey = o.o_custkey)
            then {( custkey := c.c_custkey, otherkey := o.o_custkey )}
      """, parser.term).get
    val joinPlan2 = getPlan(joinQuery2.asInstanceOf[Expr]).asInstanceOf[CExpr]

    val nestQuery1 = parser.parse(
      """
        for c in Customer union 
          if (c.c_name = "test1")
          then {(cname := c.c_name, c_orders := for o in Order union
            if (c.c_custkey = o.o_custkey)
            then {( orderkey := o.o_orderkey )})}
      """, parser.term).get
    val nestPlan1 = getPlan(nestQuery1.asInstanceOf[Expr]).asInstanceOf[CExpr]
    
    val nestQuery2 = parser.parse(
      """
        for c in Customer union 
          if (c.c_name = "test2")
          then {(custkey := c.c_custkey, n_orders := for o in Order union
            if (c.c_custkey = o.o_custkey)
            then {( otherkey := o.o_custkey )})}
      """, parser.term).get
    val nestPlan2 = getPlan(nestQuery2.asInstanceOf[Expr]).asInstanceOf[CExpr]

    // first get the fingerprint map
    val plans = Vector(joinPlan1, nestPlan1, joinPlan2, nestPlan2).zipWithIndex

    val seBuilder = SEBuilder(plans)
    seBuilder.updateSubexprs()

    val subs = seBuilder.sharedSubs()
    // println(subs.size)
    val covers = CEBuilder.buildCoverMap(subs)
    // println(covers.size)

    // this will take covers and subs to generate statistics
    val stats = Map.empty[String, Statistics]

    // val cost = new Cost(stats)
    // val selectedCovers = cost.selectCovers(covers, subs)

    // empty stats will always default to true
    // assert(covers.size == selectedCovers.size)

  }

}