package framework.plans

import org.scalatest.FunSuite
import framework.common._
import framework.examples.tpch._
import framework.examples.genomic._
import framework.nrc._
import framework.plans.{Equals => CEquals, Project => CProject}
import java.util.UUID.randomUUID
import scala.collection.mutable.HashMap

class TestQueryRewriter extends FunSuite with MaterializeNRC with NRCTranslator {

  val occur = new Occurrence{}
  val copynumber = new CopyNumber{}
  val tbls = Map("Customer" -> TPCHSchema.customertype,
                 "Order" -> TPCHSchema.orderstype,
                 "Lineitem" -> TPCHSchema.lineittype,
                 "Part" -> TPCHSchema.parttype, 
                 "Occur" -> BagType(occur.occurmid_type),
                 "CopyNumber" -> BagType(copynumber.copyNumberType))

  val parser = Parser(tbls)
  val normalizer = new Finalizer(new BaseNormalizer{})
  val optimizer = new Optimizer()

  def getPlan(query: Expr): CExpr = {
    val ncalc = normalizer.finalize(translate(query)).asInstanceOf[CExpr]
    optimizer.applyPush(Unnester.unnest(ncalc)(Map(), Map(), None, "_2"))
  }

  // TODO test with apply all
  def getPlan(query: Program): LinearCSet = {
    val ncalc = normalizer.finalize(translate(query)).asInstanceOf[CExpr]
    optimizer.applyPush(Unnester.unnest(ncalc)(Map(), Map(), None, "_2")).asInstanceOf[LinearCSet]
  }


  test("select covering"){
    val c1 = Variable.fresh(TPCHSchema.customertype.tp)
    val c2 = Variable.fresh(TPCHSchema.customertype.tp)

    val cust1 = Select(InputRef("Customer", TPCHSchema.customertype), c1, Gt(CProject(c1, "custkey"), Constant(20)), c2)
    val cust2 = Select(InputRef("Customer", TPCHSchema.customertype), c1, Lt(CProject(c2, "custkey"), Constant(2)), c2)

    // first get the fingerprint map
    val plans = Vector(cust1, cust2).zipWithIndex

    val subexprs = HashMap.empty[(CExpr, Int), Integer]
    plans.foreach(p => SEBuilder.equivSig(p)(subexprs))

    // only generate subs for things that are cache friendly
    val subs = SEBuilder.sharedSubs(plans, subexprs, true)

    val ces = CEBuilder.buildCoverMap(subs)

    val rewriter = QueryRewriter(subexprs)
    val newplans = rewriter.rewritePlans(plans, ces)

    // expected results
    val cover = ces.head._2
    val v = Variable.freshFromBag(cover.tp)
    val expected1 = Select(cover, v, Gt(CProject(v, "custkey"), Constant(20)), v)
    val expected2 = Select(cover, v, Lt(CProject(v, "custkey"), Constant(2)), v)

    assert(newplans(0).vstr == expected1.vstr)
    assert(newplans(1).vstr == expected2.vstr)

  }

  test("project covering"){
    val c1 = Variable.fresh(TPCHSchema.customertype.tp)
    val c2 = Variable.fresh(TPCHSchema.customertype.tp)

    val cust1 = parser.parse("for c in Customer union { (cname := c.c_name ) }", parser.term).get
    val custPlan1 = getPlan(cust1.asInstanceOf[Expr]).asInstanceOf[Projection]

    val cust2 = parser.parse("for c in Customer union { (ckey := c.c_custkey ) }", parser.term).get
    val custPlan2 = getPlan(cust2.asInstanceOf[Expr]).asInstanceOf[Projection]

    // first get the fingerprint map
    val plans = Vector(custPlan1, custPlan2).zipWithIndex

    val subexprs = HashMap.empty[(CExpr, Int), Integer]
    plans.foreach(p => SEBuilder.equivSig(p)(subexprs))

    // only generate subs for things that are cache friendly
    val subs = SEBuilder.sharedSubs(plans, subexprs, true)

    val ces = CEBuilder.buildCoverMap(subs)

    val rewriter = QueryRewriter(subexprs)
    val newplans = rewriter.rewritePlans(plans, ces)

    // expected results
    val cover = ces.head._2
    val v = Variable.freshFromBag(cover.tp)

    val expected1 = Projection(cover, v, custPlan1.filter, List("cname"))
    val expected2 = Projection(cover, v, custPlan2.filter, List("ckey"))
    assert(newplans(0).vstr == expected1.vstr)
    assert(newplans(1).vstr == expected2.vstr)

  }

  test("flat aggregates"){

    val query1str = 
      s"""
        Query1 <=
        (for c in CopyNumber union
          {(sid := c.cn_aliquot_uuid, 
            gid := c.cn_gene_id, 
            cnum := c.cn_copy_number)}).sumBy({sid, gid}, {cnum})
      """

    val query1 = parser.parse(query1str).get
    val plan1 = getPlan(query1.asInstanceOf[Program])

    val query2str = 
      s"""
        Query2 <=
        (for c in CopyNumber union
          {(sid := c.cn_aliquot_uuid, 
            gid := c.cn_gene_id, 
            cmax := c.max_copy_number)}).sumBy({sid, gid}, {cmax})
      """

    val query2 = parser.parse(query2str).get
    val plan2 = getPlan(query2.asInstanceOf[Program])

    val query3str = 
      s"""
        Query3 <=
        (for c in CopyNumber union
          {(sid := c.cn_aliquot_uuid, 
            cmax := c.max_copy_number)}).sumBy({sid}, {cmax})
      """

    val query3 = parser.parse(query3str).get
    val plan3 = getPlan(query3.asInstanceOf[Program])

    // equivsig -> {SE}
    // val subs = SEBuilder.sharedSubsFromProgram(Vector(plan1, plan2, plan3))

    // val ces = CEBuilder.buildCovers(subs)

    // val newplans = ces.map{
    //   case c => QueryRewriter.rewritePlans(c)
    // }

    // for(c <- newplans){
    //   println("cover")
    //   println(Printer.quote(c.cover))
    //   for (s <- c.ses){
    //     println("and sub")
    //     println(Printer.quote(s.subplan))
    //   }
    // }
  }

  test("unnest rewrites"){
    val unnestQuery1 = parser.parse(
      """
        Query1 <= 
        for o in Occur union
          if (o.donorId = "fakeTest")
          then for t in o.transcript_consequences union
            if (t.gene_id = "geneA") 
            then {( oid := o.oid, impact := t.impact )}
      """).get
    val plan1 = getPlan(unnestQuery1.asInstanceOf[Program])
    
    val unnestQuery2 = parser.parse(
      """
        Query2 <= 
        for o in Occur union
          if (o.oid = "test")
          then for t in o.transcript_consequences union 
            if (t.sift_score > 0.01)
            then {( sid := o.donorId, poly := t.polyphen_score )}
      """).get
    val plan2 = getPlan(unnestQuery2.asInstanceOf[Program])

    // val subs = SEBuilder.sharedSubsFromProgram(Vector(plan1, plan2))

    // val ces = CEBuilder.buildCovers(subs)

    // val newplans = ces.map{
    //   case c => QueryRewriter.rewritePlans(c)
    // }

    // for(c <- newplans){
    //   println("cover")
    //   println(Printer.quote(c.cover))
    //   for (s <- c.ses){
    //     println("and sub")
    //     println(Printer.quote(s.subplan))
    //   }
    // }

  }

  test("nest test"){
    println("\nNEST TEST\n")
    
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

    val subexprs = HashMap.empty[(CExpr, Int), Integer]
    plans.foreach(p => SEBuilder.equivSig(p)(subexprs))

    // only generate subs for things that are cache friendly
    val subs = SEBuilder.sharedSubs(plans, subexprs)
    // subs.foreach{ s =>
    //   println("this fingerprint "+s._1)
    //   s._2.foreach(p => println(Printer.quote(p.subplan)))
    // }

    val ces = CEBuilder.buildCoverMap(subs)
    ces.foreach{c =>
      println("fingerprint "+c._1)
      println(Printer.quote(c._2)+"\n")
    }

    val rewriter = QueryRewriter(subexprs)
    val newplans = rewriter.rewritePlans(plans, ces)
    newplans.foreach(p => println(Printer.quote(p)+"\n"))

    // val codegen = new framework.generator.spark.SparkDatasetGenerator(false, false)
    // println(codegen.generate(newplans(0)))

  }

}
