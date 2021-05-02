
package sparkutils.generated
/** Generated **/
import org.apache.spark.SparkConf
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.spark.sql.expressions.scalalang._
import scala.collection.mutable.HashMap
import sparkutils._
import sparkutils.loader._
import sparkutils.skew.SkewDataset._
case class Record323f5b7c73f6473e94cc5954ccd66a89(o_shippriority: Int, o_orderdate: String, o_custkey: Int, o_orderpriority: String, Order_index: Long, o_clerk: String, o_orderstatus: String, o_totalprice: Double, o_orderkey: Int, o_comment: String)
case class Record4701f3b27ec84c36b4d65ad642e8e73a(o_orderdate: String, o_custkey: Int, Order_index: Long, o_orderkey: Int)
case class Recordf3ad4a9b5b9041f58677af694dd2239a(l_returnflag: String, l_comment: String, l_linestatus: String, l_shipmode: String, l_shipinstruct: String, l_quantity: Double, l_receiptdate: String, l_linenumber: Int, l_tax: Double, Lineitem_index: Long, l_shipdate: String, l_extendedprice: Double, l_partkey: Int, l_discount: Double, l_commitdate: String, l_suppkey: Int, l_orderkey: Int)
case class Record5e16d7d7bfcb4fef81112a701cae2c45(l_quantity: Double, l_partkey: Int, l_orderkey: Int)
case class Record609a8705227849eb8a0faff8eabfb820(o_orderdate: String, o_custkey: Int, l_quantity: Option[Double], Order_index: Long, l_partkey: Option[Int], o_orderkey: Int, l_orderkey: Option[Int])
case class Recorde1111985aa8e4e569fe59b03e8630ca4(o_orderdate: String, o_custkey: Int, l_quantity: Option[Double], Order_index: Long, l_partkey: Option[Int])
case class Record72fa5b17b9734220a77a4eaf72190a46(o_orderdate: String, o_custkey: Int, Order_index: Long)
case class Record52a9376e89124f53bfa368d9a2cc5483(l_partkey: Int, l_quantity: Double)
case class Record4a93f809b18b463ba733d3e7a2e29881(o_orderdate: String, o_custkey: Int, Order_index: Long, o_parts: Seq[Record52a9376e89124f53bfa368d9a2cc5483])
case class Record2743f23c67354463bd7e04ed9bb08ffc(o_custkey: Int, o_orderdate: String, o_parts: Seq[Record52a9376e89124f53bfa368d9a2cc5483])
case class Record33d39030fd5844e8be7aa9b41049ba9e(c_acctbal: Double, c_name: String, Customer_index: Long, c_nationkey: Int, c_custkey: Int, c_comment: String, c_address: String, c_mktsegment: String, c_phone: String)
case class Record4ba1925836f04201ac5cd510cb1bc952(c_name: String, Customer_index: Long, c_custkey: Int)
case class Record0fdce3d8887e483189047e739753ef51(o_custkey: Int, o_orderdate: String, o_parts: Seq[Record52a9376e89124f53bfa368d9a2cc5483], orders_index: Long)
case class Recorddc04fff5bd774c3c98a0911f26d8d1df(o_orderdate: Option[String], o_custkey: Option[Int], c_name: String, o_parts: Option[Seq[Record52a9376e89124f53bfa368d9a2cc5483]], Customer_index: Long, c_custkey: Int)
case class Record60c3ca06816949158179e6e54958adac(Customer_index: Long, c_name: String, o_orderdate: Option[String], o_parts: Option[Seq[Record52a9376e89124f53bfa368d9a2cc5483]])
case class Record8454df479e22453ca9480c25fbc0ab4b(c_name: String, Customer_index: Long)
case class Record1e211f539e99487496cbf573e21d63c8(o_orderdate: String, o_parts: Seq[Record52a9376e89124f53bfa368d9a2cc5483])
case class Record8369c86c8ce44e82a653726f7bf07cc6(Customer_index: Long, c_name: String, c_orders: Seq[Record1e211f539e99487496cbf573e21d63c8])
case class Record713e08096f7749dcb9a46384ac44d3f4(c_name: String, c_orders: Seq[Record1e211f539e99487496cbf573e21d63c8])
object Test2Spark {
 def main(args: Array[String]){
   val sf = Config.datapath.split("/").last
   val conf = new SparkConf()
     .setAppName("Test2Spark"+sf)
     .set("spark.sql.shuffle.partitions", Config.maxPartitions.toString)
   val spark = SparkSession.builder().config(conf).getOrCreate()
   
   import spark.implicits._
   val tpch = TPCHLoader(spark)
val Lineitem = tpch.loadLineitemDF()
Lineitem.cache
Lineitem.count

val Customer = tpch.loadCustomerDF()
Customer.cache
Customer.count

val Order = tpch.loadOrderDF()
Order.cache
Order.count


   def f = { 
 
 val x19 = Order.withColumn("Order_index", monotonically_increasing_id())
 .as[Record323f5b7c73f6473e94cc5954ccd66a89]
 
val x22 = x19.select("o_orderdate", "o_custkey", "Order_index", "o_orderkey")
          
 .as[Record4701f3b27ec84c36b4d65ad642e8e73a]
 
val x23 = Lineitem.withColumn("Lineitem_index", monotonically_increasing_id())
 .as[Recordf3ad4a9b5b9041f58677af694dd2239a]
 
val x26 = x23.select("l_quantity", "l_partkey", "l_orderkey")
          
 .as[Record5e16d7d7bfcb4fef81112a701cae2c45]
 
val x29 = x22.equiJoin(x26, 
 Seq("o_orderkey"), Seq("l_orderkey"), "left_outer").as[Record609a8705227849eb8a0faff8eabfb820]
 
val x31 = x29.select("o_orderdate", "o_custkey", "l_quantity", "Order_index", "l_partkey")
          
 .as[Recorde1111985aa8e4e569fe59b03e8630ca4]
 
val x33 = x31.groupByKey(x32 => Record72fa5b17b9734220a77a4eaf72190a46(x32.o_orderdate, x32.o_custkey, x32.Order_index)).mapGroups{
 case (key, value) => 
   val grp = value.flatMap(x32 => 
    (x32.l_quantity,x32.l_partkey) match {
      case (None,_) => Seq()
case (_,None) => Seq()
      case _ => Seq(Record52a9376e89124f53bfa368d9a2cc5483(x32.l_partkey match { case Some(x) => x; case _ => 0 }, x32.l_quantity match { case Some(x) => x; case _ => 0.0 }))
   }).toSeq
   Record4a93f809b18b463ba733d3e7a2e29881(key.o_orderdate, key.o_custkey, key.Order_index, grp)
 }.as[Record4a93f809b18b463ba733d3e7a2e29881]
 
val x35 = x33.select("o_custkey", "o_orderdate", "o_parts")
          
 .as[Record2743f23c67354463bd7e04ed9bb08ffc]
 
val x36 = x35
val orders = x36  
//orders.cache
//orders.count
val x37 = Customer.withColumn("Customer_index", monotonically_increasing_id())
 .as[Record33d39030fd5844e8be7aa9b41049ba9e]
 
val x40 = x37.select("c_name", "Customer_index", "c_custkey")
          
 .as[Record4ba1925836f04201ac5cd510cb1bc952]
 
val x41 = orders.withColumn("orders_index", monotonically_increasing_id())
 .as[Record0fdce3d8887e483189047e739753ef51]
 
val x44 = x41.select("o_custkey", "o_orderdate", "o_parts")
          
 .as[Record2743f23c67354463bd7e04ed9bb08ffc]
 
val x47 = x40.equiJoin(x44, 
 Seq("c_custkey"), Seq("o_custkey"), "left_outer").as[Recorddc04fff5bd774c3c98a0911f26d8d1df]
 
val x49 = x47.select("Customer_index", "c_name", "o_orderdate", "o_parts")
          
 .as[Record60c3ca06816949158179e6e54958adac]
 
val x51 = x49.groupByKey(x50 => Record8454df479e22453ca9480c25fbc0ab4b(x50.c_name, x50.Customer_index)).mapGroups{
 case (key, value) => 
   val grp = value.flatMap(x50 => 
    (x50.o_orderdate,x50.o_parts) match {
      case (None,_) => Seq()
case (_,None) => Seq()
      case _ => Seq(Record1e211f539e99487496cbf573e21d63c8(x50.o_orderdate match { case Some(x) => x; case _ => "null" }, x50.o_parts match { case Some(x) => x; case _ => null }))
   }).toSeq
   Record8369c86c8ce44e82a653726f7bf07cc6(key.Customer_index, key.c_name, grp)
 }.as[Record8369c86c8ce44e82a653726f7bf07cc6]
 
val x53 = x51.select("c_name", "c_orders")
          
 .as[Record713e08096f7749dcb9a46384ac44d3f4]
 
val x54 = x53
val Test2 = x54
//Test2.count


}
var start = System.currentTimeMillis()
f
var end = System.currentTimeMillis() - start 
   println("Flat,2,"+sf+","+end+",total,"+spark.sparkContext.applicationId)
 }
}
