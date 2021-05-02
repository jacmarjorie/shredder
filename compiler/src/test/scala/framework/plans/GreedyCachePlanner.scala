package framework.plans

import org.scalatest.FunSuite
import framework.common._
import framework.examples.tpch._
import framework.examples.genomic._
import framework.nrc._
import scala.collection.immutable.{Map => IMap}
import scala.collection.mutable.{HashMap, Map}
import framework.plans.{Equals => CEquals, Project => CProject}

class TestGreedyCachePlanner extends TestBase {

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
    // println("these were put in the knapsack from most strict")
    // candidates0.foreach{ c => 
    //   println(Printer.quote(c._2))
    // }

    // println("FLEXIBILITY 1")

    // val selected1 = cost.selectCovers(ces, subs, 1)

    // selected1.foreach{ s =>
    //   println(s._1)
    //   println(s._2.profit)
    //   println(Printer.quote(s._2.plan))
    // }

    // val planner1 = new GreedyCachePlanner(selected1, 5000)
    // planner1.solve()

    // val candidates1 = planner1.knapsack
    // println("these were put in the knapsack from less strict")
    // candidates1.foreach{ c => 
    //   println(Printer.quote(c._2))
    // }

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


    val planner = new GreedyCachePlanner(selectedCovers, 5000)
    planner.solve()

    val candidates = planner.knapsack
    println("these were put in the knapsack from most strict")
    candidates.foreach{ c => 
      println(Printer.quote(c._2))
    }

  }

  // test("base aggregates"){

  //   val query1str = 
  //     s"""
  //       hybridScore1 <= 
  //           for o in occurrences union
  //             {( oid := o.oid, sid := o.donorId, cands := 
  //               ( for t in o.transcript_consequences union
  //                   for c in copynumber union
  //                     if (t.gene_id = c.cn_gene_id) then
  //                       {( gene := t.gene_id, score1 := (c.cn_copy_number + 0.01) * if (t.impact = "HIGH") then 0.80 
  //                           else if (t.impact = "MODERATE") then 0.50
  //                           else if (t.impact = "LOW") then 0.30
  //                           else 0.01 )}).sumBy({gene}, {score1}) )}
  //     """
  //   val query1 = parser.parse(query1str).get
  //   val plan1 = getProgPlan(query1.asInstanceOf[Program])

  //   val query2str = 
  //     s"""
  //       hybridScore2 <= 
  //         for o in occurrences union
  //           {( oid := o.oid, sid := o.donorId, cands := 
  //             ( for t in o.transcript_consequences union
  //                 for c in copynumber union
  //                   if (t.gene_id = c.cn_gene_id) then
  //                     {( gene := t.gene_id, score2 := (c.cn_copy_number + 0.01) * t.polyphen_score )}).sumBy({gene}, {score2}) )}
  //     """
  //   val query2 = parser.parse(query2str).get
  //   val plan2 = getProgPlan(query2.asInstanceOf[Program])

  //   val plans = Vector(plan1, plan2).zipWithIndex

  //   val subexprs = HashMap.empty[(CExpr, Int), Integer]
  //   plans.foreach(p => SEBuilder.equivSig(p)(subexprs))
  //   val subs = SEBuilder.sharedSubs(plans, subexprs, false)

  //   val ces = CEBuilder.buildCoverMap(subs)
    
  //   // val stats = StatsCollector.getCost(subs, ces)
  //   val stats = Map.empty[String, Statistics]

  //   val cost = new Cost(stats)
  //   val selected0 = cost.selectCovers(ces, subs)
  //   // println("these were selected from most strict")
  //   // selected0.foreach{ s => 
  //   //   println("profit: "+s._2.profit)
  //   //   println("weight: "+s._2.est.outSize)
  //   //   println("plan: "+Printer.quote(s._2.plan)+"\n") 
  //   // }

  //   val totalSize0 = selected0.map(s => s._2.est.outSize).reduce(_+_)
  //   // println(totalSize0)

  //   val planner0 = new GreedyCachePlanner(selected0, 500000)
  //   planner0.solve()

  //   val candidates0 = planner0.knapsack
  //   // println("these were put in the knapsack from most strict")
  //   // candidates0.foreach{ c => 
  //   //   println(Printer.quote(c._2))
  //   // }
  //   assert(candidates0.size == 1)

  //   val planner1 = new GreedyCachePlanner(selected0, 50000)
  //   planner1.solve()

  //   val candidates1 = planner1.knapsack
  //   assert(candidates1.size == 0)

  //   // less strict 
  //   val selected2 = cost.selectCovers(ces, subs, 1)
  //   // println("these were selected from flexibility 1")
  //   // selected2.foreach{ s => 
  //   //   println("profit: "+s._2.profit)
  //   //   println("weight: "+s._2.est.outSize)
  //   //   println("plan: "+Printer.quote(s._2.plan)+"\n") 
  //   // }

  //   val totalSize2 = selected2.map(s => s._2.est.outSize).reduce(_+_)
  //   // println(totalSize2)

  //   val planner2 = new GreedyCachePlanner(selected2, 500000)
  //   planner2.solve()

  //   // since there are no overlaps this should be 
  //   // the same as most strict
  //   // TODO write another plan
  //   val candidates2 = planner2.knapsack
  //   // println("these were put in the knapsack from flexibility 1")
  //   // candidates2.foreach{ c => 
  //   //   println(Printer.quote(c._2))
  //   // }
  //   assert(candidates0.size == candidates2.size)

  //   val planner3 = new GreedyCachePlanner(selected2, 50000)
  //   planner3.solve()

  //   val candidates3 = planner3.knapsack
  //   assert(candidates3.size == 0)

  //   // least strict
  //   val selected4 = cost.selectCovers(ces, subs, 2)
  //   // println("these were selected from flexibility 2")
  //   // selected4.foreach{ s => 
  //   //   println("profit: "+s._2.profit)
  //   //   println("weight: "+s._2.est.outSize)
  //   //   println("plan: "+Printer.quote(s._2.plan)+"\n") 
  //   // }

  //   val totalSize4 = selected4.map(s => s._2.est.outSize).reduce(_+_)
  //   // println(totalSize4)

  //   val planner4 = new GreedyCachePlanner(selected4, 500000)
  //   planner4.solve()

  //   // since there are no overlaps this should be 
  //   // the same as most strict
  //   val candidates4 = planner4.knapsack
  //   // println("these were put in the knapsack from flexibility 4")
  //   // candidates4.foreach{ c => 
  //   //   println(Printer.quote(c._2))
  //   // }
  //   assert(candidates4.size == 3)

  //   val planner5 = new GreedyCachePlanner(selected4, 50000)
  //   planner5.solve()

  //   val candidates5 = planner5.knapsack
  //   assert(candidates5.size == 2)


  // }

  // test("Expression below reduce shared more"){
    
  //   val query0str = 
  //     s"""
  //       hybridJoin1 <= 
  //           for o in occurrences union
  //             {( oid := o.oid, sid := o.donorId, cands := 
  //                for t in o.transcript_consequences union
  //                   for c in copynumber union
  //                     if (t.gene_id = c.cn_gene_id) then
  //                       {( gene := t.gene_id, score1 := (c.cn_copy_number + 0.01) * if (t.impact = "HIGH") then 0.80 
  //                           else if (t.impact = "MODERATE") then 0.50
  //                           else if (t.impact = "LOW") then 0.30
  //                           else 0.01 )} )}
  //     """
  //   val query0 = parser.parse(query0str).get
  //   val plan0 = getProgPlan(query0.asInstanceOf[Program])

  //   val query1str = 
  //     s"""
  //       hybridScore1 <= 
  //           for o in occurrences union
  //             {( oid := o.oid, sid := o.donorId, cands := 
  //               ( for t in o.transcript_consequences union
  //                   for c in copynumber union
  //                     if (t.gene_id = c.cn_gene_id) then
  //                       {( gene := t.gene_id, score1 := (c.cn_copy_number + 0.01) * if (t.impact = "HIGH") then 0.80 
  //                           else if (t.impact = "MODERATE") then 0.50
  //                           else if (t.impact = "LOW") then 0.30
  //                           else 0.01 )}).sumBy({gene}, {score1}) )}
  //     """
  //   val query1 = parser.parse(query1str).get
  //   val plan1 = getProgPlan(query1.asInstanceOf[Program])

  //   val query2str = 
  //     s"""
  //       hybridScore2 <= 
  //         for o in occurrences union
  //           {( oid := o.oid, sid := o.donorId, cands := 
  //             ( for t in o.transcript_consequences union
  //                 for c in copynumber union
  //                   if (t.gene_id = c.cn_gene_id) then
  //                     {( gene := t.gene_id, score2 := (c.cn_copy_number + 0.01) * t.polyphen_score )}).sumBy({gene}, {score2}) )}
  //     """
  //   val query2 = parser.parse(query2str).get
  //   val plan2 = getProgPlan(query2.asInstanceOf[Program])
    
  //   val query3str = 
  //     s"""
  //       joinScore2 <= 
  //         for o in occurrences union
  //           {( oid := o.oid, sid := o.donorId, cands := 
  //             for t in o.transcript_consequences union
  //                 for c in copynumber union
  //                   if (t.gene_id = c.cn_gene_id) then
  //                     {( gene := t.gene_id, score2 := (c.cn_copy_number + 0.01) * t.polyphen_score )} )}
  //     """
  //   val query3 = parser.parse(query3str).get
  //   val plan3 = getProgPlan(query3.asInstanceOf[Program])

  //   val plans = Vector(plan0, plan1, plan2, plan3).zipWithIndex

  //   val subexprs = HashMap.empty[(CExpr, Int), Integer]
  //   plans.foreach(p => SEBuilder.equivSig(p)(subexprs))
  //   val subs = SEBuilder.sharedSubs(plans, subexprs, false)

  //   val ces = CEBuilder.buildCoverMap(subs)
    
  //   // val stats = StatsCollector.getCost(subs, ces)
  //   val stats = Map.empty[String, Statistics]

  //   val cost = new Cost(stats)
  //   val selected0 = cost.selectCovers(ces, subs, 1)
  //   // println("these were selected from less strict")
  //   // selected0.foreach{ s => 
  //   //   println("profit: "+s._2.profit)
  //   //   println("weight: "+s._2.est.outSize)
  //   //   println("plan: "+Printer.quote(s._2.plan)+"\n") 
  //   // }

  //   val totalSize0 = selected0.map(s => s._2.est.outSize).reduce(_+_)
  //   // println(totalSize0)

  //   val planner0 = new GreedyCachePlanner(selected0, 500000)
  //   planner0.solve()

  //   val candidates0 = planner0.knapsack
  //   // println("these were put in the knapsack from less strict")
  //   // candidates0.foreach{ c => 
  //   //   println(Printer.quote(c._2))
  //   // }

  // }

  // test("nest test"){
    
  //   val joinQuery1 = parser.parse(
  //     """
  //       for c in Customer union
  //         if (c.c_name = "test1")
  //         then for o in Order union 
  //           if (c.c_custkey = o.o_custkey)
  //           then {( cname := c.c_name, orderkey := o.o_orderkey )}
  //     """, parser.term).get
  //   val joinPlan1 = getPlan(joinQuery1.asInstanceOf[Expr]).asInstanceOf[CExpr]
    
  //   val joinQuery2 = parser.parse(
  //     """
  //       for c in Customer union 
  //         if (c.c_name = "test2")
  //         then for o in Order union
  //           if (c.c_custkey = o.o_custkey)
  //           then {( custkey := c.c_custkey, otherkey := o.o_custkey )}
  //     """, parser.term).get
  //   val joinPlan2 = getPlan(joinQuery2.asInstanceOf[Expr]).asInstanceOf[CExpr]

  //   val nestQuery1 = parser.parse(
  //     """
  //       for c in Customer union 
  //         if (c.c_name = "test1")
  //         then {(cname := c.c_name, c_orders := for o in Order union
  //           if (c.c_custkey = o.o_custkey)
  //           then {( orderkey := o.o_orderkey )})}
  //     """, parser.term).get
  //   val nestPlan1 = getPlan(nestQuery1.asInstanceOf[Expr]).asInstanceOf[CExpr]
    
  //   val nestQuery2 = parser.parse(
  //     """
  //       for c in Customer union 
  //         if (c.c_name = "test2")
  //         then {(custkey := c.c_custkey, n_orders := for o in Order union
  //           if (c.c_custkey = o.o_custkey)
  //           then {( otherkey := o.o_custkey )})}
  //     """, parser.term).get
  //   val nestPlan2 = getPlan(nestQuery2.asInstanceOf[Expr]).asInstanceOf[CExpr]

  //   // first get the fingerprint map
  //   val plans = Vector(joinPlan1, nestPlan1, joinPlan2, nestPlan2).zipWithIndex

  //   val subexprs = HashMap.empty[(CExpr, Int), Integer]
  //   plans.foreach(p => SEBuilder.equivSig(p)(subexprs))
    
  //   val subs = SEBuilder.sharedSubs(plans, subexprs)
  //   val covers = CEBuilder.buildCoverMap(subs)

  //   // this will take covers and subs to generate statistics
  //   val stats = Map.empty[String, Statistics]

  //   val cost = new Cost(stats)
  //   val selectedCovers = cost.selectCovers(covers, subs, 3)

  //   // empty stats will always default to true
  //   val planner = new GreedyCachePlanner(selectedCovers, 1.0)
  //   val cache = planner.solve()
  //   assert(planner.knapsack.size == 1)

  //   val planner2 = new GreedyCachePlanner(selectedCovers, capacity = 2.0)
  //   val cache2 = planner2.solve()
  //   assert(planner2.knapsack.size == 2)

  //   // TODO recheck this based on flexibility updates
  //   // val planner5 = new GreedyCachePlanner(selectedCovers, capacity = 5.0)
  //   // val cache5 = planner5.solve()
  //   // assert(planner5.knapsack.size == 5)

  //   // val plannerFract = new GreedyCachePlanner(selectedCovers, capacity = 3.0)
  //   // val cacheFract = plannerFract.solve(fraction = .75)
  //   // assert(plannerFract.knapsack.size == 3)

  // }

}