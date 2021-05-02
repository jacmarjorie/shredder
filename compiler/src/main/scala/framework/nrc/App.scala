package framework.nrc

import framework.common._
import framework.examples.tpch.TPCHSchema
// import scala.collection.mutable.Map

object App extends MaterializeNRC with Printer {

  def main(args: Array[String]){


    val relC = BagVarRef("Customer", TPCHSchema.customertype)
    val cr = TupleVarRef("c", TPCHSchema.customertype.tp)

    val relO = BagVarRef("Order", TPCHSchema.orderstype)
    val or = TupleVarRef("o", TPCHSchema.orderstype.tp)

    val relL = BagVarRef("Lineitem", TPCHSchema.lineittype)
    val lr = TupleVarRef("l", TPCHSchema.lineittype.tp)

    val relP = BagVarRef("Part", TPCHSchema.parttype)
    val pr = TupleVarRef("p", TPCHSchema.parttype.tp)

    val relS = BagVarRef("Supplier", TPCHSchema.suppliertype)
    val sr = TupleVarRef("s", TPCHSchema.suppliertype.tp)
    
    val relPS = BagVarRef("PartSupp", TPCHSchema.partsupptype)
    val psr = TupleVarRef("ps", TPCHSchema.partsupptype.tp)

    val relN = BagVarRef("Nation", TPCHSchema.nationtype)
    val nr = TupleVarRef("n", TPCHSchema.nationtype.tp)

    val relR = BagVarRef("Region", TPCHSchema.regiontype)
    val rr = TupleVarRef("r", TPCHSchema.regiontype.tp)

    // example of deep embedding?
    // for (c <- relC
    //      yield ("name" -> c.c_name, "orders" -> 
    //         for (o <- relO
    //           yield ("date" -> o.o_date, "parts" ->
    //             for (l <- relL
    //               yield ("qty" -> l.l_qty, "pk" -> l.l_partkey)
    //               )
    //             )
    //           )
    //       )
    //   )

    // start with what i want

    // val query = cr in { relC union ("name" -> cr("c_name")) }
    // println(quote(query))

    // val query1 = cr in { relC union (
    //     "name" -> cr("c_name"), 
    //     "orders" -> { or in { relO union 
    //       ("date" -> or("o_orderdate")) } }
    //   )}


    // println(quote(query1))
    // println(quote(query2))

    var tbls = Map("C" -> TPCHSchema.customertype, 
                   "O" -> TPCHSchema.orderstype,
                   "L" -> TPCHSchema.lineittype, 
                   "P" -> TPCHSchema.parttype)

   val q1 = 
      s"""
        for c in C union 
          if (c.c_custkey > 9)
          then {(name := c.c_name, orders := for o in O union 
            if (c.c_custkey = o.o_custkey) then 
              {(date := o.o_orderdate, ok := o.o_orderkey, parts := for l in L union
                if (o.o_orderkey = l.l_orderkey) then
                  {(qty := l.l_quantity, pk := l.l_partkey)}
              )}
          )}
      """

    val parser = Parser(tbls)
    val cop:BagExpr = parser.parse(q1, parser.term).get.asInstanceOf[BagExpr]
    println(quote(cop))

    tbls += ("COP" -> cop.tp)

    val q2 = 
      s"""
        for c in COP union
          {(cname := c.name, corders := for o in c.orders union 
            {(odate := o.date, oparts := (for l in o.parts union
              for p in P union
                if (l.pk = p.p_partkey) then
                  {(pname := p.p_name, total := l.qty * p.p_retailprice)}).sumBy({pname}, {total})
             )}
          )}
      """

    val parser2 = Parser(tbls)
    val cop2:BagExpr = parser2.parse(q2, parser2.term).get.asInstanceOf[BagExpr]
    println(quote(cop2))

    val q3 = 
      s"""
        ( for c in COP union
          for o in c.orders union
            for l in o.parts union
              if (l.pk = 2) then
              for p in P union
                if (l.pk != p.p_partkey && l.pk = 2 || l.pk = 3 && ! (l.pk = 2 && l.pk = 2)) then
                  {(cname := c.name, total := (l.qty + 0.01) * (p.p_retailprice + 0.01) )}).sumBy({cname}, {total})
      """
     
     val parser3 = Parser(tbls)
     val cop3:BagExpr = parser3.parse(q3, parser3.term).get.asInstanceOf[BagExpr]
     println(quote(cop3))

    val q4 = 
      s"""
        let nParts :=
          for p in P union 
            {( pk := p.p_partkey, pr := p.p_retailprice )}
        in 
        ( for c in COP union
          for o in c.orders union
            for l in o.parts union
              if (l.pk = 2) then
              for p in nParts union
                if (l.pk != p.pk && l.pk = 2 || l.pk = 3 && ! (l.pk = 2 && l.pk = 2)) then
                  {(cname := c.name, total := (l.qty + 0.01) * (p.pr + 0.01) )}).sumBy({cname}, {total})
      """
     
     val parser4 = Parser(tbls)
     val cop4:BagExpr = parser4.parse(q4, parser4.term).get.asInstanceOf[BagExpr]
     println(quote(cop4))


  }

}


