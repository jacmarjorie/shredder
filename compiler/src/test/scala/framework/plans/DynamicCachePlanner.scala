package framework.plans

import org.scalatest.FunSuite
import framework.common._
import framework.examples.tpch._
import framework.examples.genomic._
import framework.nrc._
import scala.collection.immutable.{Map => IMap}
import scala.collection.mutable.{HashMap, Map}
import framework.plans.{Equals => CEquals, Project => CProject}

class TestDynamicCachePlanner extends FunSuite with Materialization with 
  MaterializeNRC with NRCTranslator  with Shredding{

  val compileCost = false

  val occur = new Occurrence{}
  val cnum = new CopyNumber{}
  val samps = new Biospecimen{}
  val tbls = IMap("Customer" -> TPCHSchema.customertype,
                 "Order" -> TPCHSchema.orderstype,
                 "Lineitem" -> TPCHSchema.lineittype,
                 "Part" -> TPCHSchema.parttype, 
                 "occurrences" -> BagType(occur.occurmid_type), 
                 "copynumber" -> BagType(cnum.copyNumberType), 
                 "samples" -> BagType(samps.biospecType))

  val parser = Parser(tbls)
  val normalizer = new Finalizer(new BaseNormalizer{})
  val optimizer = new Optimizer()

  def getPlan(query: Program, shred: Boolean = true): LinearCSet = {

    val program = if (shred){

      val (shredded, shreddedCtx) = shredCtx(query)
      val optShredded = optimize(shredded)
      val materialized = materialize(optShredded, eliminateDomains = true)
      materialized.program

    }else query

    val ncalc = normalizer.finalize(translate(program)).asInstanceOf[CExpr]
    optimizer.applyPush(Unnester.unnest(ncalc)(IMap(), IMap(), None, "_2")).asInstanceOf[LinearCSet]
  
  }


  test("aggregates"){

    val query1str = 
      s"""
        hybridScore1 <= 
            for o in occurrences union
              {( oid := o.oid, sid := o.donorId, cands := 
                ( for t in o.transcript_consequences union
                    for c in copynumber union
                      if (t.gene_id = c.cn_gene_id) then
                        {( gene := t.gene_id, score1 := (c.cn_copy_number + 0.01) * if (t.impact = "HIGH") then 0.80 
                            else if (t.impact = "MODERATE") then 0.50
                            else if (t.impact = "LOW") then 0.30
                            else 0.01 )}).sumBy({gene}, {score1}) )}
      """
    val query1 = parser.parse(query1str).get
    val plan1 = getPlan(query1.asInstanceOf[Program])

    val query2str = 
      s"""
        hybridScore2 <= 
          for o in occurrences union
            {( oid := o.oid, sid := o.donorId, cands := 
              ( for t in o.transcript_consequences union
                  for c in copynumber union
                    if (t.gene_id = c.cn_gene_id) then
                      {( gene := t.gene_id, score2 := (c.cn_copy_number + 0.01) * t.polyphen_score )}).sumBy({gene}, {score2}) )}
      """
    val query2 = parser.parse(query2str).get
    val plan2 = getPlan(query2.asInstanceOf[Program])
    val splan2 = getPlan(query2.asInstanceOf[Program], true)
    println(Printer.quote(splan2))

    // val progs = Vector(plan1, plan2).zipWithIndex
    // val plans = progs.flatMap{ case (prog, id) => 
    //   prog.exprs.map(e => e match {
    //     case CNamed(name, p) => (p, id)
    //     case _ => (e, id)
    // })}

    // val subexprs = HashMap.empty[(CExpr, Int), Integer]
    // plans.foreach(p => SEBuilder.equivSig(p)(subexprs))
    // val subs = SEBuilder.sharedSubs(plans, subexprs, false)

    // val ces = CEBuilder.buildCoverMap(subs)
    
    // val stats = StatsCollector.getCost(subs, ces)

    // val cost = new Cost(stats)

    // cost.printEstimateAndStat(ces, subs)

    // this should select all with profit not 0
    // val selected0 = cost.selectCovers(ces, subs, 3)
    // println("these were selected from all covers "+selected0.size)
    // selected0.foreach{ s => 
    //   println("profit: "+s._2.profit)
    //   println("weight: "+s._2.est.outSize)
    //   println("plan: "+Printer.quote(s._2.plan)+"\n") 
    // }


    // val planner0 = new DynamicCachePlanner(selected0)
    // val result0 = planner0.solve(100000)
    // println("this is the result of dynamic selection")
    // println("profit: "+result0.profit)
    // println("plans: "+result0.selected.size)
    // result0.selected.foreach{
    //   s => println(Printer.quote(s._2)+"\n")
    // }


    // val planner1 = new DynamicCachePlanner(selected0)
    // val result1 = planner1.solve(50000)
    // println("this is the result of dynamic selection")
    // println("profit: "+result1.profit)
    // println("plans: "+result1.selected.size)
    // result1.selected.foreach{
    //   s => println(Printer.quote(s._2)+"\n")
    // }

    // val selected1 = cost.selectCovers(ces, subs, 2)
    // println("these were selected from all covers "+selected1.size)
    // selected1.foreach{ s => 
    //   println("profit: "+s._2.profit)
    //   println("weight: "+s._2.est.outSize)
    //   println("plan: "+Printer.quote(s._2.plan)+"\n") 
    // }


    // val planner2 = new DynamicCachePlanner(selected1)
    // val result2 = planner2.solve(500000)
    // println("this is the result of dynamic selection")
    // println("profit: "+result2.profit)
    // println("plans: "+result2.selected.size)
    // result2.selected.foreach{
    //   s => println(Printer.quote(s._2)+"\n")
    // }
    // assert(result2 == result0)

  }

}