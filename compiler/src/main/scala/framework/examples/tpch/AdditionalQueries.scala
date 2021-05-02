package framework.examples.tpch

import framework.common._
import framework.utils.Utils.Symbol
import framework.nrc.Parser

/** This document contains additional TPC-H queries 
  * that have been used throughout the development 
  * and testing of the framework. 
  * 
  * This also includes queries from Slender (previously SlenderQueries.scala).
  */

  object SimpleTest extends TPCHBase {
  
  override def loadTables(shred: Boolean = false, skew: Boolean = false): String = ""

  val name = "SimpleTest"
  
  val tbls = Set()
  val tbls2 = Map("Customer" -> TPCHSchema.customertype,
                 "Order" -> TPCHSchema.orderstype)

  // all samples that have a TP53 mutation with non-high impact
  val thisQuery = 
    s"""
      Query1 <=
        for c in Customer union 
          if (c.c_name = "test1")
          then {(cname := c.c_name, c_orders := for o in Order union
            if (c.c_custkey = o.o_custkey)
            then {( orderkey := o.o_orderkey )})}
    """

    val parser = Parser(tbls2)
    val program: Program = parser.parse(thisQuery).get.asInstanceOf[Program]

}



/**
  * // this is query1_ljp
  * Let ljp = For l in L Union
  *            For p in P Union
  *              If (l.l_partkey = p.p_partkey) // skew join
  *              Then Sng((l_orderkey := l_orderkey, p_name := p.p_name, l_qty := l.l_quantity))
  *
  * // this is query1
  * For c in C Union
  *  Sng((c_name := c.c_name, c_orders := 
  *    For o in O Union
  *      If (o.o_custkey = c.c_custkey)
  *      Then Sng((o_orderdate := o.o_orderdate, o_parts := 
  *        For l in ljp Union
  *           If (l.l_orderkey = o.o_orderkey)
  *           Then Sng((p_name := p.p_name, l_qty := l.l_quantity))))))
  */

object Query1 extends TPCHBase {
  val name = "Query1"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val query1_ljp = ForeachUnion(lr, relL,
                     ForeachUnion(pr, relP,
                       IfThenElse(Cmp(OpEq, lr("l_partkey"), pr("p_partkey")), 
                         Singleton(Tuple("l_orderkey" -> lr("l_orderkey"), 
                                         "p_name" -> pr("p_name"), 
                                         "l_qty" -> lr("l_quantity"))))))
  
  val (ljpr, lpr) = varset("ljp", "lp", query1_ljp)

  val query1 = ForeachUnion(cr, relC,
                Singleton(Tuple("c_name" -> cr("c_name"), "c_orders" -> ForeachUnion(or, relO,
                  IfThenElse(Cmp(OpEq, or("o_custkey"), cr("c_custkey")), 
                    Singleton(Tuple("o_orderdate" -> or("o_orderdate"), "o_parts" -> 
                      ForeachUnion(lpr, ljpr,
                        IfThenElse(Cmp(OpEq, lpr("l_orderkey"), or("o_orderkey")),
                          Singleton(Tuple("p_name" -> lpr("p_name"), "l_qty" -> lpr("l_qty"))))))))))))

  val program = Program(Assignment(ljpr.name, query1_ljp), Assignment(name, query1))
}

object Query1Full extends TPCHBase {
  val name = "Query1"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val query1 = ForeachUnion(cr, relC,
                Singleton(Tuple("c_name" -> cr("c_name"), "c_orders" -> ForeachUnion(or, relO,
                  IfThenElse(Cmp(OpEq, or("o_custkey"), cr("c_custkey")), 
                    Singleton(Tuple("o_orderdate" -> or("o_orderdate"), "o_parts" -> 
                      ForeachUnion(lr, relL,
                        IfThenElse(Cmp(OpEq, lr("l_orderkey"), or("o_orderkey")),
                       ForeachUnion(pr, relP,
                         IfThenElse(Cmp(OpEq, lr("l_partkey"), pr("p_partkey")), 
                          Singleton(Tuple("p_name" -> pr("p_name"), "l_qty" -> lr("l_quantity"))))))))))))))

  val program = Program(Assignment(name, query1))
}

object Query1BU extends TPCHBase {
  val name = "Query1"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val lquery = ForeachUnion(lr, relL, 
    ForeachUnion(pr, relP, 
      IfThenElse(Cmp(OpEq, lr("l_partkey"), pr("p_partkey")),
        Singleton(Tuple("l_orderkey" -> lr("l_orderkey"), "p_name" -> pr("p_name"), "l_qty" -> lr("l_quantity"))))))
  
  val (parts, partRef) = varset("parts", "part", lquery)
  val oquery = 
    ForeachUnion(or, relO,
      Singleton(Tuple("o_custkey" -> or("o_custkey"), "o_orderdate" -> or("o_orderdate"), "o_parts" -> 
        ForeachUnion(partRef, parts,
          IfThenElse(Cmp(OpEq, or("o_orderkey"), partRef("l_orderkey")),
            Singleton(Tuple("p_name" -> partRef("p_name"), "l_qty" -> partRef("l_qty"))))))))
  
  val (orders, orderRef) = varset("orders", "order", oquery)
  val query = 
    ForeachUnion(cr, relC,
      Singleton(Tuple("c_name" -> cr("c_name"), "c_orders" -> 
        ForeachUnion(orderRef, orders,
          IfThenElse(Cmp(OpEq, cr("c_custkey"), orderRef("o_custkey")),
            Singleton(Tuple("o_orderdate" -> orderRef("o_orderdate"), 
              "o_parts" -> orderRef("o_parts"))))))))

  val program = Program(Assignment(parts.name, lquery), Assignment(orders.name, oquery), Assignment(name, query))
}


object Query1Filter extends TPCHBase {
  val name = "Query1Filter"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val query1_ljp = ForeachUnion(lr, relL,
                     ForeachUnion(pr, relP,
                       IfThenElse(Cmp(OpEq, lr("l_partkey"), pr("p_partkey")), 
                         Singleton(Tuple("l_orderkey" -> lr("l_orderkey"), 
                                         "p_name" -> pr("p_name"), 
                                         "l_qty" -> lr("l_quantity"))))))
  
  val (ljpr, lpr) = varset("ljp", "lp", query1_ljp)

  val query1 = ForeachUnion(cr, relC,
                IfThenElse(Cmp(OpGe, Const(1500000, IntType), cr("c_custkey")), 
                Singleton(Tuple("c_name" -> cr("c_name"), "c_orders" -> ForeachUnion(or, relO,
                  IfThenElse(And(Cmp(OpEq, or("o_custkey"), cr("c_custkey")), 
                    Cmp(OpGe, Const(150000000, IntType), or("o_orderkey"))), 
                    Singleton(Tuple("o_orderdate" -> or("o_orderdate"), "o_parts" -> 
                      ForeachUnion(lpr, ljpr,
                        IfThenElse(Cmp(OpEq, lpr("l_orderkey"), or("o_orderkey")),
                          Singleton(Tuple("p_name" -> lpr("p_name"), "l_qty" -> lpr("l_qty")))))))))))))

  val program = Program(Assignment(ljpr.name, query1_ljp), Assignment(name, query1))
}

/**
For c2 in Query1 Union
  Sng((c_name := c2.c_name, totals := (
    For o2 in c2.c_orders Union
      For p2 in o2.o_parts Union
        Sng((orderdate := o2.o_orderdate, pname := p2.p_name, qty := p2.l_qty))
    ).groupBy+((orderdate := x1.orderdate, pname := x1.pname)), x1.qty)))
**/
object Query2 extends TPCHBase {
  val name = "Query2"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val (q1r, cor) = varset(Query1.name, "c2",
    Query1.program(Query1.name).varRef.asInstanceOf[BagExpr])

  val orders = BagProject(cor, "c_orders")
  val co2r = TupleVarRef("o2", orders.tp.tp)

  val parts = BagProject(co2r, "o_parts")
  val co3r = TupleVarRef("p2", parts.tp.tp)

  val query2 =
    ForeachUnion(cor, q1r,
      Singleton(Tuple("c_name" -> cor("c_name"), "totals" ->
        ReduceByKey(
          ForeachUnion(co2r, orders,
            ForeachUnion(co3r, parts,
              Singleton(Tuple("orderdate" -> co2r("o_orderdate"),
                "pname" -> co3r("p_name"), "qty" -> co3r("l_qty"))))),
          List("orderdate", "pname"),
          List("qty")
        ))))

  val program =
    Query1.program.asInstanceOf[Program] ++
      Program(Assignment(name, query2))
}

/**
(For c2 in Query1 Union
  For o2 in c2.c_orders Union
    For p2 in o2.o_parts Union
      For p in P Union
        If (p.p_name = p2.p_name)
        Then Sng((c_name := c2.c_name, p_name := p.p_name, total := p2.l_qty*p.p_retailprice))
  ).groupBy+((c_name := x1.c_name, p_name := x1.p_name), x1.total)
**/
object Query3 extends TPCHBase {
  val name = "Query3"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val (q1r, cor) = varset(Query1.name, "c2",
    Query1.program(Query1.name).varRef.asInstanceOf[BagExpr])

  val orders = BagProject(cor, "c_orders")
  val co2r = TupleVarRef("o2", orders.tp.tp)

  val parts = BagProject(co2r, "o_parts")
  val co3r = TupleVarRef("p2", parts.tp.tp)

  val query3 =
    ReduceByKey(
      ForeachUnion(cor, q1r,
        ForeachUnion(co2r, orders,
          ForeachUnion(co3r, parts,
            ForeachUnion(pr, relP,
              IfThenElse(Cmp(OpEq, pr("p_name"), co3r("p_name")),
                Singleton(Tuple("c_name" -> cor("c_name"), "p_name" -> pr("p_name"), 
                                "total" ->  co3r("l_qty").asNumeric * pr("p_retailprice").asNumeric))))))),
      List("c_name", "p_name"),
      List("total")
    )

  val program =
    Query1.program.asInstanceOf[Program] ++
      Program(Assignment(name, query3))
}

/**
For c2 in Query1 Union
  Sng((c_name := c2.c_name, c_orders := 
    For o2 in c2.c_orders Union
      Sng((o_orderdate := o2.o_orderdate, o_parts := 
        (For p2 in o2.o_parts Union
          For p in P Union
            If (p.p_name = p2.p_name)
            Then Sng((p_name := p2.p_name, total := p2.l_qty*p.p_retailprice))
    ).groupBy+((p_name := x1.p_name), x1.total)
**/
object Query4 extends TPCHBase {
  val name = "Query4"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val (q1r, cor) = varset(Test2.name, "c2",
    Test2.program(Test2.name).varRef.asInstanceOf[BagExpr])

  val orders = BagProject(cor, "c_orders")
  val co2r = TupleVarRef("o2", orders.tp.tp)

  val parts = BagProject(co2r, "o_parts")
  val co3r = TupleVarRef("p2", parts.tp.tp)

  val query4 =
    ForeachUnion(cor, q1r,
      Singleton(Tuple("c_name" -> cor("c_name"), "c_orders" ->
        ForeachUnion(co2r, orders,
          Singleton(Tuple("o_orderdate" -> co2r("o_orderdate"), "o_parts" ->
            ReduceByKey(
              ForeachUnion(co3r, parts,
                ForeachUnion(pr, relP,
                  IfThenElse(Cmp(OpEq, pr("p_partkey"), co3r("l_partkey")),
                    Singleton(Tuple("p_name" -> pr("p_name"), "total" -> 
                      co3r("l_quantity").asNumeric * pr("p_retailprice").asNumeric))))),
              List("p_name"),
              List("total")
            )))))))

  val program = Program(Assignment(name, query4))
}

/** 
This is just an extended version of Query 1 
which is passed to Query4Filter* to 
make the application of filters easier.
**/
object Query1Extended extends TPCHBase {
  val name = "Query1Extended"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val query1_ljp = ForeachUnion(lr, relL,
                     ForeachUnion(pr, relP,
                       IfThenElse(Cmp(OpEq, lr("l_partkey"), pr("p_partkey")), 
                         Singleton(Tuple("l_orderkey" -> lr("l_orderkey"), 
                                         "p_name" -> pr("p_name"), 
                                         "l_qty" -> lr("l_quantity"))))))
  
  val (ljpr, lpr) = varset("ljp", "lp", query1_ljp)

  val query1 = ForeachUnion(cr, relC,
                Singleton(Tuple("c_custkey" -> cr("c_custkey"), "c_name" -> cr("c_name"), "c_orders" -> ForeachUnion(or, relO,
                  IfThenElse(Cmp(OpEq, or("o_custkey"), cr("c_custkey")), 
                    Singleton(Tuple("o_orderkey" -> or("o_orderkey"), "o_orderdate" -> or("o_orderdate"), "o_parts" -> 
                      ForeachUnion(lpr, ljpr,
                        IfThenElse(Cmp(OpEq, lpr("l_orderkey"), or("o_orderkey")),
                          Singleton(Tuple("p_name" -> lpr("p_name"), "l_qty" -> lpr("l_qty"))))))))))))

  val program = Program(Assignment(ljpr.name, query1_ljp), Assignment(name, query1))
}

object Query4Filter1 extends TPCHBase {
  val name = "Query4Filter1"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val (q1r, cor) = varset(Query1Extended.name, "c2",
    Query1Extended.program(Query1Extended.name).varRef.asInstanceOf[BagExpr])

  val orders = BagProject(cor, "c_orders")
  val co2r = TupleVarRef("o2", orders.tp.tp)

  val parts = BagProject(co2r, "o_parts")
  val co3r = TupleVarRef("p2", parts.tp.tp)

  val query4 =
    ForeachUnion(cor, q1r,
      IfThenElse(Cmp(OpGe, Const(1500000, IntType), cr("c_custkey")),
        Singleton(Tuple("c_name" -> cor("c_name"), "c_orders" ->
          ForeachUnion(co2r, orders,
            Singleton(Tuple("o_orderdate" -> co2r("o_orderdate"), "o_parts" ->
              ReduceByKey(
                ForeachUnion(co3r, parts,
                  ForeachUnion(pr, relP,
                    IfThenElse(Cmp(OpEq, pr("p_name"), co3r("p_name")),
                        Singleton(Tuple("p_name" -> pr("p_name"), "total" -> 
                          co3r("l_qty").asNumeric * pr("p_retailprice").asNumeric))))),
                List("p_name"),
                List("total")
              ))))))))

  val program =
    Query1Extended.program.asInstanceOf[Program] ++
      Program(Assignment(name, query4))
}

object Query4Filter2 extends TPCHBase {
  val name = "Query4Filter2"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val (q1r, cor) = varset(Query1Extended.name, "c2",
    Query1Extended.program(Query1Extended.name).varRef.asInstanceOf[BagExpr])

  val orders = BagProject(cor, "c_orders")
  val co2r = TupleVarRef("o2", orders.tp.tp)

  val parts = BagProject(co2r, "o_parts")
  val co3r = TupleVarRef("p2", parts.tp.tp)

  val query4 =
    ForeachUnion(cor, q1r,
      IfThenElse(Cmp(OpGe, Const(1500000, IntType), cr("c_custkey")),
        Singleton(Tuple("c_name" -> cor("c_name"), "c_orders" ->
          ForeachUnion(co2r, orders,
            IfThenElse(Cmp(OpGe, Const(150000000, IntType), or("o_orderkey")),
            Singleton(Tuple("o_orderdate" -> co2r("o_orderdate"), "o_parts" ->
              ReduceByKey(
                ForeachUnion(co3r, parts,
                  ForeachUnion(pr, relP,
                    IfThenElse(Cmp(OpEq, pr("p_name"), co3r("p_name")),
                        Singleton(Tuple("p_name" -> pr("p_name"), "total" -> 
                          co3r("l_qty").asNumeric * pr("p_retailprice").asNumeric))))),
                List("p_name"),
                List("total")
              )))))))))

  val program = Program(Assignment(name, query4))
}

/**
Let resultInner := For c in C Union
  For o in O Union
    If (c.c_custkey = o.o_custkey) // skew join
    Then Sng((o_orderkey := o.o_orderkey, c_name := c.c_name))

For s in S Union
  Sng((s_name := s.s_name, customers2 := For l in L Union
    If (s.s_suppkey = l.l_suppkey)
    Then For co in resultInner Union
      If (co.o_orderkey = l.l_orderkey)
      Then Sng((c_name2 := co.c_name))))
**/

object Query5 extends TPCHBase {
  val name = "Query5"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val custs = 
      ForeachUnion(or, relO,
          ForeachUnion(cr, relC,
            IfThenElse(Cmp(OpEq, cr("c_custkey"), or("o_custkey")),
              Singleton(Tuple("c_orderkey" -> or("o_orderkey"),
                 "c_name" -> cr("c_name"), "c_nationkey" -> cr("c_nationkey"))))))
  val (customers, customersRef) = varset("customers", "co", custs)

  val custsKeyed = ForeachUnion(customersRef, customers, 
    ForeachUnion(lr, relL,
      IfThenElse(Cmp(OpEq, lr("l_orderkey"), customersRef("c_orderkey")),
        projectTuple(customersRef, "c_suppkey" -> lr("l_suppkey"), List("c_orderkey")))))
  val (csuppkeys, csuppRef) = varset("csupps", "cs", custsKeyed)
                       
  val query5 = ForeachUnion(sr, relS,
                Singleton(Tuple("s_name" -> sr("s_name"), "s_nationkey" -> sr("s_nationkey"), "customers2" -> 
                  ForeachUnion(csuppRef, csuppkeys,
                    IfThenElse(Cmp(OpEq, csuppRef("c_suppkey"), sr("s_suppkey")),
                      projectBaseTuple(csuppRef, List("c_suppkey"))))))) 

  val program = Program(Assignment(customers.name, custs), 
    Assignment(csuppkeys.name, custsKeyed), Assignment(name, query5))
}

/**

This is Query6 without specifying Let's, which is more 
optimal for the version of the query that does 
not apply the optimization.

For c in C Union
  Sng((c_name := c.c_name, suppliers := For co in Query2 Union
    For co2 in co.customers2 Union
      If (co2.c_name2 = c.c_name)
      Then Sng((s_name := co.s_name))))
**/
object Query6Full extends TPCHBase {
  val name = "Query6Full"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")
 
  val (q2r, cor) = varset(Query5.name, "co",
    Query5.program(Query5.name).varRef.asInstanceOf[BagExpr])

  val cust = BagProject(cor, "customers2")
  val co2r = TupleVarRef("co2", cust.tp.tp)

  val query6 = ForeachUnion(cr, relC,
                Singleton(Tuple("c_name" -> cr("c_name"), "suppliers" -> 
                  ForeachUnion(cor, q2r,
                    ForeachUnion(co2r, cust,
                      IfThenElse(Cmp(OpEq, co2r("c_name"), cr("c_name")),
                        Singleton(Tuple("s_name" -> cor("s_name")))))))))

  val program = Program(Assignment(name, query6))
}

object Query6GBK extends TPCHBase {
  val name = "Query6GBK"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")
 
  val (q2r, cor) = varset(Query5.name, "co",
    Query5.program(Query5.name).varRef.asInstanceOf[BagExpr])

  val cust = BagProject(cor, "customers2")
  val co2r = TupleVarRef("co2", cust.tp.tp)

  val cflat = ForeachUnion(cor, q2r,
                    ForeachUnion(co2r, cust,
                      Singleton(Tuple("c_name" -> co2r("c_name"), "s_name" -> cor("s_name")))))
  val (cfr, c2r) = varset("cflat", "c2", cflat)
  val query6 = GroupByKey(cfr, List("c_name"), List("s_name"))

  val program = Program(Assignment(cfr.name, cflat), Assignment(name, query6))
}

/**

This is Query 6 with the Let, which helps with 
join order in the version with the optimization.

cflat := For co in Query5 Union
 For co2 in co.customers2 Union
   Sng((c_name := co2.c_name2, s_name := co.s_name))
For c in C Union
 Sng((c_name := c.c_name, suppliers := For cf in cflat Union
   If (cf.c_name = c.c_name)
   Then Sng((s_name := cf.s_name))))
**/
object Query6 extends TPCHBase {
  val name = "Query6"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")
 
  val (q5r, cor) = varset(Query5.name, "co",
    Query5.program(Query5.name).varRef.asInstanceOf[BagExpr])

  val cust = BagProject(cor, "customers2")
  val co2r = TupleVarRef("co2", cust.tp.tp)
  
  val flat = ForeachUnion(cor, q5r,
              ForeachUnion(co2r, cust,
                Singleton(Tuple("c_name" -> co2r("c_name"), "s_name" -> cor("s_name"), 
                  "s_nationkey" -> cor("s_nationkey")))))
  val (cflatr, cfr) = varset("cflat", "cf", flat.asInstanceOf[BagExpr])

  val query6 = ForeachUnion(cr, relC,
                Singleton(Tuple("c_name" -> cr("c_name"), "c_nationkey" -> cr("c_nationkey"), "suppliers" -> 
                  ForeachUnion(cfr, cflatr,
                    IfThenElse(Cmp(OpEq, cfr("c_name"), cr("c_name")),
                      projectBaseTuple(cfr, List("s_suppkey")))))))

  val program = Program(Assignment(cflatr.name, flat), Assignment(name, query6))
}

/**
cflat := For co in Query5 Union
  For co2 in co.customers2 Union
    Sng((c_name := co2.c_name2, s_name := co.s_name))

For c in C Union
  Sng((c_name := c.c_name, suppliers := Total(For cf in cflat Union
    If (cf.c_name = c.c_name)
    Then Sng((s_name := cf.s_name)))))
**/
object Query7 extends TPCHBase {
  val name = "Query7"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val (q5r, cor) = varset(Query5.name, "co",
    Query5.program(Query5.name).varRef.asInstanceOf[BagExpr])

  val cust = BagProject(cor, "customers2")
  val co2r = TupleVarRef("co2", cust.tp.tp)
  
  val flat = ForeachUnion(cor, q5r,
              ForeachUnion(co2r, cust,
                Singleton(Tuple("c_name" -> co2r("c_name"), "s_name" -> cor("s_name")))))
  val (cflatr, cfr) = varset("cflat", "cf", flat.asInstanceOf[BagExpr])
  val query7 = ForeachUnion(cr, relC,
                Singleton(Tuple("c_name" -> cr("c_name"), "suppliers" -> 
                  Count(ForeachUnion(cfr, cflatr,
                    IfThenElse(Cmp(OpEq, cfr("c_name"), cr("c_name")),
                      Singleton(Tuple("s_name" -> cfr("s_name")))))))))

  val program =
    Query5.program.asInstanceOf[Program] ++
      Program(Assignment(cflatr.name, flat), Assignment(name, query7))
}



/**
  
  This below contains the queries from slender. Queries will have "Full" suffix if they are described 
  as a single monolith query. Queries without the "Full" suffix are written as a sequence 
  (currently upstream queries in the sequence have to be flat), that enables the specification 
  of join order. 

**/
  
object TPCHQuery1Full extends TPCHBase {
  val name = "Query1Full"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val query1 = ForeachUnion(cr, relC,
    Singleton(Tuple("c_name" -> cr("c_name"), "c_orders" -> ForeachUnion(or, relO,
      IfThenElse(Cmp(OpEq, or("o_custkey"), cr("c_custkey")),
        Singleton(Tuple("o_orderdate" -> or("o_orderdate"), "o_parts" -> ForeachUnion(lr, relL,
          IfThenElse(
            Cmp(OpEq, lr("l_orderkey"), or("o_orderkey")),
            ForeachUnion(pr, relP, IfThenElse(
              Cmp(OpEq, lr("l_partkey"), pr("p_partkey")),
              Singleton(Tuple("p_name" -> pr("p_name"), "l_qty" -> lr("l_quantity"))))))))))))))

  val program = Program(Assignment(name, query1))
}

/**
  * // this is query1_ljp
  * Let ljp = For l in L Union
  *            For p in P Union
  *              If (l.l_partkey = p.p_partkey) // skew join
  *              Then Sng((l_orderkey := l_orderkey, p_name := p.p_name, l_qty := l.l_quantity))
  *
  * // this is query1
  * For c in C Union
  *  Sng((c_name := c.c_name, c_orders := 
  *    For o in O Union
  *      If (o.o_custkey = c.c_custkey)
  *      Then Sng((o_orderdate := o.o_orderdate, o_parts := 
  *        For l in ljp Union
  *           If (l.l_orderkey = o.o_orderkey)
  *           Then Sng((p_name := p.p_name, l_qty := l.l_quantity))))))
  */

object TPCHQuery1 extends TPCHBase {
  val name = "Query1"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val query1_ljp = ForeachUnion(lr, relL,
                     ForeachUnion(pr, relP,
                       IfThenElse(Cmp(OpEq, lr("l_partkey"), pr("p_partkey")), 
                         Singleton(Tuple("l_orderkey" -> lr("l_orderkey"), 
                                         "p_name" -> pr("p_name"), 
                                         "l_qty" -> lr("l_quantity"))))))
  
  val (ljpr, lpr) = varset("ljp", "lp", query1_ljp)

  val query1 = ForeachUnion(cr, relC,
                Singleton(Tuple("c_name" -> cr("c_name"), "c_orders" -> ForeachUnion(or, relO,
                  IfThenElse(Cmp(OpEq, or("o_custkey"), cr("c_custkey")), 
                    Singleton(Tuple("o_orderdate" -> or("o_orderdate"), "o_parts" -> 
                      ForeachUnion(lpr, ljpr,
                        IfThenElse(Cmp(OpEq, lpr("l_orderkey"), or("o_orderkey")),
                          Singleton(Tuple("p_name" -> lpr("p_name"), "l_qty" -> lpr("l_qty"))))))))))))

  val program = Program(Assignment(ljpr.name, query1_ljp), Assignment(name, query1))
}

object TPCHQuery1WK extends TPCHBase {
  val name = "Query1WK"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val query1_ljp = ForeachUnion(lr, relL,
                     ForeachUnion(pr, relP,
                       IfThenElse(Cmp(OpEq, lr("l_partkey"), pr("p_partkey")), 
                         Singleton(Tuple("l_orderkey" -> lr("l_orderkey"), 
                                         "p_name" -> pr("p_name"), 
                                         "l_qty" -> lr("l_quantity"))))))
  
  val (ljpr, lpr) = varset("ljp", "lp", query1_ljp)

  val query1 = ForeachUnion(cr, relC,
                Singleton(Tuple("c_custkey" -> cr("c_custkey"), "c_name" -> cr("c_name"), "c_orders" -> ForeachUnion(or, relO,
                  IfThenElse(Cmp(OpEq, or("o_custkey"), cr("c_custkey")), 
                    Singleton(Tuple("o_orderkey" -> or("o_orderkey"), "o_orderdate" -> or("o_orderdate"), "o_parts" -> 
                      ForeachUnion(lpr, ljpr,
                        IfThenElse(Cmp(OpEq, lpr("l_orderkey"), or("o_orderkey")),
                          Singleton(Tuple("p_name" -> lpr("p_name"), "l_qty" -> lpr("l_qty"))))))))))))

  val program = Program(Assignment(ljpr.name, query1_ljp), Assignment(name, query1))
}

object TPCHQuery1Filter extends TPCHBase {
  val name = "Query1Filter"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val query1_ljp = ForeachUnion(lr, relL,
                     ForeachUnion(pr, relP,
                       IfThenElse(Cmp(OpEq, lr("l_partkey"), pr("p_partkey")), 
                         Singleton(Tuple("l_orderkey" -> lr("l_orderkey"), 
                                         "p_name" -> pr("p_name"), 
                                         "l_qty" -> lr("l_quantity"))))))
  
  val (ljpr, lpr) = varset("ljp", "lp", query1_ljp)

  val query1 = ForeachUnion(cr, relC,
                IfThenElse(Cmp(OpGe, Const(1500000, IntType), cr("c_custkey")), 
                Singleton(Tuple("c_name" -> cr("c_name"), "c_orders" -> ForeachUnion(or, relO,
                  IfThenElse(And(Cmp(OpEq, or("o_custkey"), cr("c_custkey")), 
                    Cmp(OpGe, Const(150000000, IntType), or("o_orderkey"))), 
                    Singleton(Tuple("o_orderdate" -> or("o_orderdate"), "o_parts" -> 
                      ForeachUnion(lpr, ljpr,
                        IfThenElse(Cmp(OpEq, lpr("l_orderkey"), or("o_orderkey")),
                          Singleton(Tuple("p_name" -> lpr("p_name"), "l_qty" -> lpr("l_qty")))))))))))))

  val program = Program(Assignment(ljpr.name, query1_ljp), Assignment(name, query1))
}

object TPCHQuery2Full extends TPCHBase {
  val name = "Query2Full"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val query2 = ForeachUnion(sr, relS,
            Singleton(Tuple("s_name" -> sr("s_name"), "customers2" -> ForeachUnion(lr, relL,
              IfThenElse(Cmp(OpEq, sr("s_suppkey"), lr("l_suppkey")),
                ForeachUnion(or, relO,
                  IfThenElse(Cmp(OpEq, or("o_orderkey"), lr("l_orderkey")),
                    ForeachUnion(cr, relC,
                      IfThenElse(Cmp(OpEq, cr("c_custkey"), or("o_custkey")),
                        Singleton(Tuple("c_name2" -> cr("c_name"))))))))))))

  val program = Program(Assignment(name, query2))
}

/**
Let resultInner := For c in C Union
  For o in O Union
    If (c.c_custkey = o.o_custkey) // skew join
    Then Sng((o_orderkey := o.o_orderkey, c_name := c.c_name))

For s in S Union
  Sng((s_name := s.s_name, customers2 := For l in L Union
    If (s.s_suppkey = l.l_suppkey)
    Then For co in resultInner Union
      If (co.o_orderkey = l.l_orderkey)
      Then Sng((c_name2 := co.c_name))))
**/

object TPCHQuery2 extends TPCHBase {
  val name = "Query2"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val resultInner = 
    //ForeachUnion(lr, relL,
      ForeachUnion(or, relO,
        //IfThenElse(Cmp(OpEq, lr("l_orderkey"), or("o_orderkey")),
          ForeachUnion(cr, relC, // skew join
            IfThenElse(Cmp(OpEq, cr("c_custkey"), or("o_custkey")),
              Singleton(Tuple("o_orderkey" -> or("o_orderkey"), "c_name" -> cr("c_name"))))))//))
  val (rir, cor) = varset("resultInner", "co", resultInner)
                       
  val query2 = ForeachUnion(sr, relS,
                Singleton(Tuple("s_name" -> sr("s_name"), "customers2" -> 
                  ForeachUnion(lr, relL,
                    IfThenElse(Cmp(OpEq, lr("l_suppkey"), sr("s_suppkey")),
                      ForeachUnion(cor, rir,
                        IfThenElse(Cmp(OpEq, lr("l_orderkey"), cor("o_orderkey")),
                          Singleton(Tuple("c_name2" -> cor("c_name"))))))))))

  val program = Program(Assignment(rir.name, resultInner), Assignment(name, query2))
}


object TPCHQuery3Full extends TPCHBase{
  val name = "Query3Full"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val query3 = ForeachUnion(pr, relP,
                Singleton(Tuple("p_name" -> pr("p_name"), "suppliers" -> ForeachUnion(psr, relPS,
                  IfThenElse(Cmp(OpEq, psr("ps_partkey"), pr("p_partkey")),
                    ForeachUnion(sr, relS,
                      IfThenElse(Cmp(OpEq, sr("s_suppkey"), psr("ps_suppkey")),
                        Singleton(Tuple("s_name" -> sr("s_name"), "s_nationkey" -> sr("s_nationkey"))))))),
                  "customers" -> ForeachUnion(lr, relL,
                    IfThenElse(Cmp(OpEq, lr("l_partkey"), pr("p_partkey")),
                      ForeachUnion(or, relO,
                        IfThenElse(Cmp(OpEq, or("o_orderkey"), lr("l_orderkey")),
                          ForeachUnion(cr, relC,
                            IfThenElse(Cmp(OpEq, cr("c_custkey"), or("o_custkey")),
                              Singleton(Tuple("c_name" -> cr("c_name"), "c_nationkey" -> cr("c_nationkey"))))))))))))

  val program = Program(Assignment(name, query3))
}


/**
  * Let partsuppliers = For ps in PS Union
  * For s in S Union
  *   If (s.s_suppkey = ps.ps_suppkey) // skew join
  *   Then Sng((p_partkey := p.p_partkey, s_name := s.s_name, s_nationkey := s.s_nationkey))
  *
  * Let orders = For o in O Union
  * For c in C Union
  *   If (c.c_custkey = o.o_custkey) // skew join
  *   Then Sng((o_orderkey := o.o_orderkey, c_name := c.c_name, c_nationkey := c.c_nationkey))))
  *
  * Let cparts = For o in orders Union
  *   For l in Lineitem Union
  *     If (o.o_orderkey = l.l_orderkey) 
  *     Then Sng((l_partkey := l.l_partkey, c_name := o.c_name, c_nationkey := o.c_nationkey))
  *
  * // the two joins on the same level should translate to a co-group operation
  * For p in P Union
  *   Sng((p_name := p.p_name, 
  *           suppliers := 
  *             For pss in partsuppliers Union
  *                If (pss.ps_partkey = p.p_partkey)
  *                Then Sng((s_name := s.s_name, s_nationkey := pss.s_nationkey)), 
  *           customers := 
  *             For pc in cparts Union 
  *               If (p.p_partkey = pc.l_partkey)
  *               Then Sng(c_name := pc.c_name, pc.c_nationkey))
  **/
object TPCHQuery3 extends TPCHBase {
  val name = "Query3"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")
  
  val partsuppliers = ForeachUnion(psr, relPS,
                    ForeachUnion(sr, relS,
                      IfThenElse(Cmp(OpEq, sr("s_suppkey"), psr("ps_suppkey")),
                        Singleton(Tuple("ps_partkey" -> psr("ps_partkey"), 
                                        "s_name" -> sr("s_name"), 
                                        "s_nationkey" -> sr("s_nationkey"))))))
  
  val (psjsr, pssr) = varset("partsuppliers", "pss", partsuppliers)

  val custorders = ForeachUnion(or, relO,
                ForeachUnion(cr, relC,
                  IfThenElse(Cmp(OpEq, cr("c_custkey"), or("o_custkey")),
                    Singleton(Tuple("o_orderkey" -> or("o_orderkey"), 
                                    "c_name" -> cr("c_name"), 
                                    "c_nationkey" -> cr("c_nationkey"))))))
  
  val (ojcr, cor) = varset("custorders", "co", custorders)

  val cparts = ForeachUnion(cor, ojcr,
                ForeachUnion(lr, relL,
                  IfThenElse(Cmp(OpEq, cor("o_orderkey"), lr("l_orderkey")),
                    Singleton(Tuple("l_partkey" -> lr("l_partkey"), 
                                    "c_name" -> cor("c_name"),
                                    "c_nationkey" -> cor("c_nationkey"))))))

  val (cpjlr, cpr) = varset("cparts", "cp", cparts)
  val query3 = ForeachUnion(pr, relP,
                Singleton(Tuple("p_name" -> pr("p_name"),
                                "suppliers" -> ForeachUnion(pssr, psjsr,
                                                IfThenElse(Cmp(OpEq, pssr("ps_partkey"), pr("p_partkey")),
                                                  Singleton(Tuple("s_name" -> pssr("s_name"),
                                                                  "s_nationkey" -> pssr("s_nationkey"))))),
                                "customers" -> ForeachUnion(cpr, cpjlr,
                                                IfThenElse(Cmp(OpEq, cpr("l_partkey"), pr("p_partkey")),
                                                  Singleton(Tuple("c_name" -> cpr("c_name"), 
                                                                  "c_nationkey" -> cpr("c_nationkey"))))))))             

  val program = Program(
    Assignment(psjsr.name, partsuppliers),
    Assignment(ojcr.name, custorders),
    Assignment(cpjlr.name, cparts),
    Assignment(name, query3)
  )
}


object TPCHQuery4Full extends TPCHBase {
  val name = "Query4Full"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val (q1r, cor) = varset(TPCHQuery1Full.name, "c2",
    TPCHQuery1Full.program(TPCHQuery1Full.name).varRef.asInstanceOf[BagExpr])

  val orders = BagProject(cor, "c_orders")
  val co2r = TupleVarRef("o2", orders.tp.tp)

  val parts = BagProject(co2r, "o_parts")
  val co3r = TupleVarRef("p2", parts.tp.tp)

  val query4 =
    ReduceByKey(
      ForeachUnion(cor, q1r,
        ForeachUnion(co2r, orders,
          ForeachUnion(co3r, parts,
            Singleton(Tuple(
              "c_name" -> cor("c_name"),
              "orderdate" -> co2r("o_orderdate"),
              "pname" -> co3r("p_name"),
              "qty" -> co3r("l_qty")))))),
      List("c_name", "orderdate", "pname"),
      List("qty")
    )

  val program = Program(Assignment(name, query4))
}

/**
For c in C Union
  Sng((c_name := c.c_name, suppliers := For co in Query2Full Union
    For co2 in co.customers2 Union
      If (co2.c_name2 = c.c_name)
      Then Sng((s_name := co.s_name))))
**/

object TPCHQuery6Full extends TPCHBase {
  val name = "Query6Full"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")
 
  val (q2r, cor) = varset(TPCHQuery2Full.name, "co",
    TPCHQuery2Full.program(TPCHQuery2Full.name).varRef.asInstanceOf[BagExpr])

  val cust = BagProject(cor, "customers2")
  val co2r = TupleVarRef("co2", cust.tp.tp)

  val query6 = ForeachUnion(cr, relC,
                Singleton(Tuple("c_name" -> cr("c_name"), "suppliers" -> 
                  ForeachUnion(cor, q2r,
                    ForeachUnion(co2r, cust,
                      IfThenElse(Cmp(OpEq, co2r("c_name2"), cr("c_name")),
                        Singleton(Tuple("s_name" -> cor("s_name")))))))))

  val program = Program(Assignment(name, query6))
}

object TPCHQuery6 extends TPCHBase {
  val name = "Query6"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")
 
  val (q2r, cor) = varset(TPCHQuery2Full.name, "co",
    TPCHQuery2Full.program(TPCHQuery2Full.name).varRef.asInstanceOf[BagExpr])

  val cust = BagProject(cor, "customers2")
  val co2r = TupleVarRef("co2", cust.tp.tp)
  
  val flat = ForeachUnion(cor, q2r,
              ForeachUnion(co2r, cust,
                Singleton(Tuple("c_name" -> co2r("c_name2"), "s_name" -> cor("s_name")))))
  val (cflatr, cfr) = varset("cflat", "cf", flat.asInstanceOf[BagExpr])
  val query6 = ForeachUnion(cr, relC,
                Singleton(Tuple("c_name" -> cr("c_name"), "suppliers" -> 
                  ForeachUnion(cfr, cflatr,
                    IfThenElse(Cmp(OpEq, cfr("c_name"), cr("c_name")),
                      Singleton(Tuple("s_name" -> cfr("s_name"))))))))

  val program = Program(Assignment(cflatr.name, flat), Assignment(name, query6))
}

object TPCHQuery6New extends TPCHBase {
  val name = "Query6New"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")
 
  val (q2r, cor) = varset(TPCHQuery2Full.name, "co",
    TPCHQuery2Full.program(TPCHQuery2Full.name).varRef.asInstanceOf[BagExpr])

  val cust = BagProject(cor, "customers2")
  val co2r = TupleVarRef("co2", cust.tp.tp)
  
  val flat = ForeachUnion(cor, q2r,
              ForeachUnion(co2r, cust,
                Singleton(Tuple("c_name" -> co2r("c_name2"), "s_name" -> cor("s_name")))))
  val (cflatr, cfr) = varset("cflat", "cf", flat.asInstanceOf[BagExpr])
  val query6 = ForeachUnion(cr, relC,
                Singleton(Tuple("c_name" -> cr("c_name"), "suppliers" -> 
                  Count(ForeachUnion(cfr, cflatr,
                    IfThenElse(Cmp(OpEq, cfr("c_name"), cr("c_name")),
                      Singleton(Tuple("s_name" -> cfr("s_name")))))))))

  val program = Program(Assignment(cflatr.name, flat), Assignment(name, query6))
}

/**
For n in N Union
  Sng((n_name := n.n_name, parts := For co in Query3 Union
    For x2 in co.suppliers Union
      If (x2.s_nationkey = n.n_nationkey AND
        // this is the forall check
        Total(For x1 in co.customers Union
                If (x1.c_nationkey = n.n_nationkey)
                Then Sng((count := 1))) == 0)
      Then Sng((p_name := co.p_name))))
**/

object TPCHQuery7Full extends TPCHBase {
  val name = "Query7Full"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val (q3r, cor) = varset(TPCHQuery3Full.name, "co",
    TPCHQuery3Full.program(TPCHQuery3Full.name).varRef.asInstanceOf[BagExpr])
  
  val customers = BagProject(cor, "customers")
  val c2r = TupleVarRef(Symbol.fresh(), customers.tp.tp)

  val suppliers = BagProject(cor, "suppliers")
  val s2r = TupleVarRef(Symbol.fresh(), suppliers.tp.tp)

  val custforall = ForeachUnion(c2r, customers,
                    IfThenElse(Cmp(OpEq, c2r("c_nationkey"), nr("n_nationkey")),
                      Singleton(Tuple("count" -> Const(1, IntType)))))
  
  val query7 = ForeachUnion(nr, relN,
                Singleton(Tuple("n_name" -> nr("n_name"), "parts" -> 
                  ForeachUnion(cor, q3r,
                    ForeachUnion(s2r, suppliers,
                      IfThenElse(And(Cmp(OpEq, s2r("s_nationkey"), nr("n_nationkey")),
                                     Cmp(OpEq, Count(custforall), Const(0, IntType))),
                        Singleton(Tuple("p_name" -> cor("p_name")))))))))

  val program = Program(Assignment(name, query7))
}


object TPCHQuery7 extends TPCHBase {
  val name = "Query7"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val (q3r, cor) = varset(TPCHQuery3Full.name, "co",
    TPCHQuery3Full.program(TPCHQuery3Full.name).varRef.asInstanceOf[BagExpr])
  
  val customers = BagProject(cor, "customers")
  val c2r = TupleVarRef(Symbol.fresh(), customers.tp.tp)

  val suppliers = BagProject(cor, "suppliers")
  val s2r = TupleVarRef(Symbol.fresh(), suppliers.tp.tp)

  val custforall = ForeachUnion(c2r, customers,
                    IfThenElse(Cmp(OpEq, c2r("c_nationkey"), nr("n_nationkey")),
                      Singleton(Tuple("count" -> Const(1, IntType)))))
  
  val query7 = ForeachUnion(nr, relN,
                Singleton(Tuple("n_name" -> nr("n_name"), "parts" -> 
                  ForeachUnion(cor, q3r,
                    ForeachUnion(s2r, suppliers,
                      IfThenElse(And(Cmp(OpEq, s2r("s_nationkey"), nr("n_nationkey")),
                                     Cmp(OpEq, Count(custforall), Const(0, IntType))),
                        Singleton(Tuple("p_name" -> cor("p_name")))))))))

  val program = Program(Assignment(name, query7))
}

object TPCHQuery72 extends TPCHBase {
  val name = "Query72"
  val tbls: Set[String] = Set("Customer", "Order", "Lineitem", "Part")

  val (q3r, cor) = varset(TPCHQuery3Full.name, "co",
    TPCHQuery3Full.program(TPCHQuery3Full.name).varRef.asInstanceOf[BagExpr])
  
  val customers = BagProject(cor, "customers")
  val c2r = TupleVarRef(Symbol.fresh(), customers.tp.tp)

  val suppliers = BagProject(cor, "suppliers")
  val s2r = TupleVarRef(Symbol.fresh(), suppliers.tp.tp)

  val custforall = ForeachUnion(c2r, customers,
                    IfThenElse(Cmp(OpEq, c2r("c_nationkey"), nr("n_nationkey")),
                      Singleton(Tuple("count" -> Const(1, IntType)))))
  
  val query72 = ForeachUnion(nr, relN,
                Singleton(Tuple("n_name" -> nr("n_name"), "parts" -> 
                  ForeachUnion(cor, q3r,
                    ForeachUnion(s2r, suppliers,
                      IfThenElse(And(Cmp(OpEq, s2r("s_nationkey"), nr("n_nationkey")),
                                     Cmp(OpEq, Count(custforall), Const(0, IntType))),
                        Singleton(Tuple("p_name" -> cor("p_name")))))))))

  val program = Program(Assignment(name, query72))
}
