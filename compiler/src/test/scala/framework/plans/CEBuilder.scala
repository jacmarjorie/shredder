package framework.plans

import org.scalatest.FunSuite
import framework.common._
import framework.examples.tpch._
import framework.examples.genomic._
import framework.nrc._
import framework.plans.{Equals => CEquals, Project => CProject}
import scala.collection.mutable.HashMap

class TestCEBuilder extends TestBase {

  test("standard program compilation"){

    val seBuilder = SEBuilder(progs)
    seBuilder.updateSubexprs()

    val subs = seBuilder.sharedSubs()
    // printSE(subs)

    val ces = CEBuilder.buildCoverMap(subs)
    printCE(ces)
    // assert(ces.size == 8) //5

  }

  // test("shredded compilation"){
    
  //   val seBuilder = SEBuilder(sprogs)
  //   seBuilder.updateSubexprs()

  //   val subs = seBuilder.sharedSubs()

  //   val nmap = seBuilder.getNameMap

  //   printSE(subs)
  //   val ces = CEBuilder.buildCoverMap(subs, nmap)
  //   // printCE(ces)
  //   assert(ces.size == 10)

  // }

  // test("input SEs"){
  //   val cust1 = InputRef("Customer", TPCHSchema.customertype)
  //   val cust2 = InputRef("Customer", TPCHSchema.customertype)

  //   val ce1 = CEBuilder.buildCover(cust1, cust2)
  //   assert(ce1 == cust1)

  //   val ce2 = CEBuilder.buildCovers(List(cust1, cust2))
  //   assert(ce1 == ce2)

  // }

  // test("indexed SEs"){
  //   val cust1 = AddIndex(InputRef("Customer", TPCHSchema.customertype), "cust")
  //   val cust2 = AddIndex(InputRef("Customer", TPCHSchema.customertype), "cust")

  //   val ce1 = CEBuilder.buildCover(cust1, cust2)
  //   assert(ce1 == cust1)

  //   val ce2 = CEBuilder.buildCovers(List(cust1, cust2))
  //   assert(ce1 == ce2)

  // }

  // test("select covering"){
  //   val c1 = Variable.fresh(TPCHSchema.customertype.tp)
  //   val c2 = Variable.fresh(TPCHSchema.customertype.tp)

  //   val cust1 = Select(InputRef("Customer", TPCHSchema.customertype), c1, Gt(CProject(c1, "custkey"), Constant(20)))
  //   val cust2 = Select(InputRef("Customer", TPCHSchema.customertype), c1, Lt(CProject(c2, "custkey"), Constant(2)))
    
  //   val ce1 = CEBuilder.buildCover(cust1, cust2).asInstanceOf[Select]
  //   assert(ce1.p.vstr == "custkey>20||custkey<2")

  //   val ce2 = CEBuilder.buildCovers(List(cust1, cust2))
  //   assert(ce1.vstr == ce2.vstr)

  // }

  // test("project covering"){

  //   val cust1 = parser.parse("for c in Customer union { (cname := c.c_name ) }", parser.term).get
  //   val custPlan1 = getPlan(cust1.asInstanceOf[Expr]).asInstanceOf[Projection]

  //   val cust2 = parser.parse("for c in Customer union { (custkey := c.c_custkey ) }", parser.term).get
  //   val custPlan2 = getPlan(cust2.asInstanceOf[Expr]).asInstanceOf[Projection]

  //   assert(custPlan1.fields.toSet == Set("c_name"))
  //   assert(custPlan2.fields.toSet == Set("c_custkey"))

  //   val ce1 = CEBuilder.buildCover(custPlan1, custPlan2).asInstanceOf[Projection]
  //   assert(ce1.fields.toSet == Set("c_custkey", "c_name"))

  //   val ce2 = CEBuilder.buildCovers(List(custPlan1, custPlan2))
  //   assert(ce1.vstr == ce2.vstr)

  // }

  // test("join covering"){

  //   // with CE below
  //   val joinQuery1 = parser.parse(
  //     """
  //       for o in Order union
  //         if (o.o_orderkey > 10)
  //         then for c in Customer union
  //           if (c.c_custkey = o.o_custkey)
  //           then {(cname := c.c_name, odate := o.o_orderdate )}
  //     """, parser.term).get
  //   val joinPlan1 = getPlan(joinQuery1.asInstanceOf[Expr])
    
  //   val joinQuery2 = parser.parse(
  //     """
  //       for o in Order union
  //         if (o.o_orderkey > 15)
  //         then for c in Customer union 
  //           if (c.c_custkey = o.o_custkey)
  //           then {(custkey := c.c_custkey, orderkey := o.o_orderkey )}
  //     """, parser.term).get
  //   val joinPlan2 = getPlan(joinQuery2.asInstanceOf[Expr])

  //   // need to look at this case more
  //   val ce = CEBuilder.buildCover(joinPlan1, joinPlan2)
  //     .asInstanceOf[Projection].in.asInstanceOf[JoinOp]

  //   val left = ce.left.asInstanceOf[Projection].in.asInstanceOf[Select].p.vstr
  //   assert(left == "o_orderkey>10||o_orderkey>15")

  //   // val ce2 = CEBuilder.buildCover(List(joinPlan1.in, joinPlan2.in))
  //   // assert(ce.vstr == ce2.vstr)

  //   // need this once selections work better
  //   // val joinQuery1 = parser.parse(
  //   //   """
  //   //     for o in Order union
  //   //       for c in Customer union
  //   //         if (c.c_custkey = o.o_custkey)
  //   //         then {(cname := c.c_name, odate := o.o_orderdate )}
  //   //   """, parser.term).get
  //   // val joinPlan1 = getPlan(joinQuery1.asInstanceOf[Expr]).asInstanceOf[Projection]
    
  //   // val joinQuery2 = parser.parse(
  //   //   """
  //   //     for o in Order union
  //   //       for c in Customer union 
  //   //         if (c.c_custkey = o.o_custkey)
  //   //         then {(custkey := c.c_custkey, orderkey := o.o_orderkey )}
  //   //   """, parser.term).get
  //   // val joinPlan2 = getPlan(joinQuery2.asInstanceOf[Expr]).asInstanceOf[Projection]

  // }

  // test("unnest covers"){
  //   val unnestQuery1 = parser.parse(
  //     """
  //       for o in occurrences union
  //         if (o.donorId = "fakeTest")
  //         then for t in o.transcript_consequences union
  //           if (t.gene_id = "geneA") 
  //           then {( oid := o.oid, impact := t.impact )}
  //     """, parser.term).get
  //   val unnestPlan1 = getPlan(unnestQuery1.asInstanceOf[Expr]).asInstanceOf[CExpr]
    
  //   val unnestQuery2 = parser.parse(
  //     """
  //       for o in occurrences union
  //         if (o.oid = "test")
  //         then for t in o.transcript_consequences union 
  //           if (t.sift_score > 0.01)
  //           then {( sid := o.donorId, poly := t.polyphen_score )}
  //     """, parser.term).get
  //   val unnestPlan2 = getPlan(unnestQuery2.asInstanceOf[Expr]).asInstanceOf[CExpr]

  //   val ce = CEBuilder.buildCover(unnestPlan1, unnestPlan2).asInstanceOf[Projection]
  //   val un = ce.in.asInstanceOf[UnnestOp]
  //   assert(un.fields.toSet == Set("oid", "impact", "donorId", "polyphen_score"))
  //   assert(un.path == "transcript_consequences")
  //   assert(un.filter.vstr == "gene_id=geneA||sift_score>0.01")

  // }

  // test("reduce covers"){
  //   val reduceQuery1 = parser.parse(
  //     """
  //       (for c in Customer union 
  //         for o in Order union
  //           if (c.c_custkey = o.o_custkey)
  //           then {(cname := c.c_name, orderkey := o.o_orderkey )}).sumBy({cname}, {orderkey})
  //     """, parser.term).get
  //   val reducePlan1 = getPlan(reduceQuery1.asInstanceOf[Expr]).asInstanceOf[CExpr]
      
  //   val reduceQuery2 = parser.parse(
  //     """
  //       (for o in Order union
  //         for c in Customer union 
  //           if (c.c_custkey = o.o_custkey)
  //           then {(custkey := c.c_custkey, otherkey := o.o_custkey )}).sumBy({custkey}, {otherkey})
  //     """, parser.term).get
  //   val reducePlan2 = getPlan(reduceQuery2.asInstanceOf[Expr]).asInstanceOf[CExpr]
  //   val ce = CEBuilder.buildCover(reducePlan1, reducePlan2).asInstanceOf[Reduce]
  //   assert(ce.keys.toSet == Set("c_name", "c_custkey"))
  //   assert(ce.values.toSet == Set("o_orderkey", "o_custkey"))
    
  // }

  // test("covers from SEs"){
  //   // with CE below
  //   val joinQuery1 = parser.parse(
  //     """
  //       for o in Order union
  //         if (o.o_orderkey > 10)
  //         then for c in Customer union
  //           if (c.c_custkey = o.o_custkey)
  //           then {(cname := c.c_name, odate := o.o_orderdate )}
  //     """, parser.term).get
  //   val joinPlan1 = getPlan(joinQuery1.asInstanceOf[Expr])
    
  //   val joinQuery2 = parser.parse(
  //     """
  //       for o in Order union
  //         if (o.o_orderkey > 15)
  //         then for c in Customer union 
  //           if (c.c_custkey = o.o_custkey)
  //           then {(custkey := c.c_custkey, orderkey := o.o_orderkey )}
  //     """, parser.term).get
  //   val joinPlan2 = getPlan(joinQuery2.asInstanceOf[Expr])

  //   val plans = Vector(joinPlan1, joinPlan2).zipWithIndex
  //   val seBuilder = SEBuilder(plans)
  //   seBuilder.updateSubexprs()

  //   val subs = seBuilder.sharedSubs()

  //   val covers = CEBuilder.buildCoverMap(subs)

  // }

}