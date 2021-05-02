package framework.nrc

import org.scalatest.FunSuite
import framework.common._
import framework.examples.tpch._

class TestParser extends FunSuite with MaterializeNRC {

  val tbls = Map("Customer" -> TPCHSchema.customertype,
                 "Order" -> TPCHSchema.orderstype,
                 "Lineitem" -> TPCHSchema.lineittype,
                 "Part" -> TPCHSchema.parttype)

  val parser = Parser(tbls)

  test("numeric expression"){
  
    val n1 = "1"
    assert(parser.parse(n1, parser.numeric).get == NumericConst(1, IntType))

    val n2 = "1020394"
    assert(parser.parse(n2, parser.numeric).get == NumericConst(1020394, IntType))
  
    val n3 = "2.0"
    assert(parser.parse(n3, parser.numeric).get == NumericConst(2.0, DoubleType))



  }


}
