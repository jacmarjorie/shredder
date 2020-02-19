
package experiments
/** Generated **/
import org.apache.spark.SparkConf
import org.apache.spark.sql.SparkSession
import sprkloader._
import sprkloader.SkewPairRDD._
import sprkloader.SkewDictRDD._
import sprkloader.DomainRDD._
import scala.collection.mutable.HashMap
case class Record323(lbl: Unit)
case class Record324(l_orderkey: Int, l_quantity: Double, l_partkey: Int)
case class Record325(p_name: String, p_partkey: Int)
case class Record326(l_orderkey: Int, p_partkey: Int, l_qty: Double)
case class Record327(c__Fc_custkey: Int)
case class Record328(c_name: String, c_orders: Record327)
case class Record329(lbl: Record327)
case class Record330(o_orderdate: String, o_orderkey: Int, o_custkey: Int)
case class Record332(o__Fo_orderkey: Int)
case class Record333(o_orderdate: String, o_parts: Record332)
case class Record334(lbl: Record332)
case class Record336(p_partkey: Int, l_qty: Double)
case class Record412(c2__Fc_orders: Record327)
case class Record413(c_name: String, totals: Record412)
case class Record414(lbl: Record412)
case class Record416(orderdate: String, partkey: Int)
case class Record438(orderdate: String, partkey: Int, _2: Double)
case class Record439(c_name: String, totals: Iterable[Record438])
object ShredQuery2SparkInterm2 {
 def main(args: Array[String]){
   val sf = Config.datapath.split("/").last
   val conf = new SparkConf().setMaster(Config.master).setAppName("ShredQuery2SparkInterm2"+sf)
   val spark = SparkSession.builder().config(conf).getOrCreate()
   val tpch = TPCHLoader(spark)

val L__F = 3
val L__D_1 = tpch.loadLineitemProjBzip
L__D_1.cache
spark.sparkContext.runJob(L__D_1, (iter: Iterator[_]) => {})
val P__F = 4
val P__D_1 = tpch.loadPartProj
P__D_1.cache
spark.sparkContext.runJob(P__D_1, (iter: Iterator[_]) => {})
val C__F = 1
val C__D_1 = tpch.loadCustomersProj
C__D_1.cache
spark.sparkContext.runJob(C__D_1, (iter: Iterator[_]) => {})
val O__F = 2
val O__D_1 = tpch.loadOrdersProjBzip
O__D_1.cache
spark.sparkContext.runJob(O__D_1, (iter: Iterator[_]) => {})

val x219 = L__D_1
val x224 = P__D_1
val x226 = x219.map{ case x225 => ({val x227 = x225.l_partkey 
x227}, x225) }
val x227 = x226.joinSkew(x224, (p: PartProj) => p.p_partkey)
 
val x236 = x227.map{ case (x230, x231) => 
   val x232 = x230.l_orderkey 
val x233 = x231.p_partkey
val x234 = x230.l_quantity 
val x235 = Record326(x232, x233, x234) 
x235 
} 
val ljp__D_1 = x236
val x237 = ljp__D_1

val x244 = C__D_1.map{ case x239 => 
   val x240 = x239.c_name 
val x241 = x239.c_custkey 
val x242 = Record327(x241) 
val x243 = Record328(x240, x242) 
x243 
} 
val M__D_1 = x244
val x245 = M__D_1

val x247 = M__D_1.createDomain(l => Record329(l.c_orders)) 
val c_orders_ctx1 = x247
val x253 = c_orders_ctx1

val x255 = c_orders_ctx1 
val x261 = O__D_1.map(x256 => { val x257 = x256.o_orderdate 
val x258 = x256.o_orderkey 
val x259 = x256.o_custkey 
val x260 = Record330(x257, x258, x259) 
x260 }) 
val x264 = x261.map{ case x263 => ({val x266 = x263.o_custkey 
x266}, x263) }
val x267 = x264.joinDomainSkew(x255, (l: Record329) => l.lbl.c__Fc_custkey)

val x277 = x267.map{ case (x269, x268) => 
   ({val x270 = (x268) 
x270.lbl}, {val x271 = x269.o_orderdate 
val x272 = x269.o_orderkey 
val x273 = Record332(x272) 
val x274 = Record333(x271, x273) 
x274})
}.groupByLabel() 

val c_orders__D_1 = x277
val x283 = c_orders__D_1

val x285 = c_orders__D_1.createDomain(l => Record334(l.o_parts))
val o_parts_ctx1 = x285

val x298 = o_parts_ctx1 
val x300 = ljp__D_1 

val x303 = x300.map{ case x302 => ({val x305 = x302.l_orderkey 
x305}, x302) }
val x304 = x303.joinDomainSkew(x298, (l: Record334) => l.lbl.o__Fo_orderkey)

val x315 = x304.map{ case (x308, x307) => 
  ({val x309 = (x307) 
x309.lbl}, {val x310 = x308.p_partkey 
val x311 = x308.l_qty 
val x312 = Record336(x310, x311) 
x312})
}.groupByLabel() 
val o_parts__D_1 = x315
val x321 = o_parts__D_1

//o_parts__D_1.collect.foreach(println(_))
val Query1__D_1 = M__D_1
Query1__D_1.cache
spark.sparkContext.runJob(Query1__D_1, (iter: Iterator[_]) => {})

val Query1__D_2c_orders_1 = c_orders__D_1
Query1__D_2c_orders_1.cache
spark.sparkContext.runJob(Query1__D_2c_orders_1, (iter: Iterator[_]) => {})

val Query1__D_2c_orders_2o_parts_1 = o_parts__D_1
Query1__D_2c_orders_2o_parts_1.cache
spark.sparkContext.runJob(Query1__D_2c_orders_2o_parts_1, (iter: Iterator[_]) => {})

 def f = {

var start0 = System.currentTimeMillis() 
val x371 = Query1__D_1.map{ case x366 => 
   val x367 = x366.c_name 
val x368 = x366.c_orders 
val x369 = Record412(x368) 
val x370 = Record413(x367, x369) 
x370 
} 
val M__D_1 = x371
val x372 = M__D_1

// don't wrap label in label
val x374 = M__D_1.createDomain(l =>  l.totals)
val totals_ctx1 = x374
val x380 = totals_ctx1

val x382 = totals_ctx1
// (opartsLabel, (label, o))
val x383 = Query1__D_2c_orders_1
val x384 = x383.lookupSkew(x382, (l: Record412) => l.c2__Fc_orders).mapPartitions( it =>
  it.flatMap{ case (lbl, bag) => bag.map(o => (o.o_parts, (lbl, o.o_orderdate))) }, true)

// map partitions has no affect here
val x385 = Query1__D_2c_orders_2o_parts_1.flatMap{ case (lbl, bag) => 
		bag.map(p => (lbl, p.p_partkey) -> p.l_qty) 
}.reduceByKey(_+_).map{
  case ((lbl, pk), tot) => lbl -> (pk, tot)
}

val x386 = x384.cogroup(x385).flatMap( pair =>
  for ((lbl, date) <- pair._2._1.iterator; (pk,tot) <- pair._2._2.iterator) yield (lbl, date, pk) -> tot
).reduceByKey(_+_).map{
  case ((lbl, date, pk), tot) => lbl -> (date, pk, tot)
}.groupByLabel()
val totals__D_1 = x386
spark.sparkContext.runJob(totals__D_1, (iter: Iterator[_]) => {})
var end0 = System.currentTimeMillis() - start0
println("ShredQuery2SparkInterm2,"+sf+","+Config.datapath+","+end0+",query,"+spark.sparkContext.applicationId)

var start1 = System.currentTimeMillis()
/**val x426 = M__D_1.map(c => c.totals -> c.c_name).cogroup(totals__D_1).flatMap{
  case (_, (left, x428)) => left.map( x427 => (x427, x428.flatten))
}
val newM__D_1 = x426
val x436 = newM__D_1
newM__D_1.collect.foreach(println(_))
spark.sparkContext.runJob(newM__D_1, (iter: Iterator[_]) => {})**/
var end = System.currentTimeMillis() - start0
var end1 = System.currentTimeMillis() - start1
println("ShredQuery2SparkInterm2,"+sf+","+Config.datapath+","+end+",total,"+spark.sparkContext.applicationId)
println("ShredQuery2SparkInterm2,"+sf+","+Config.datapath+","+end1+",unshredding,"+spark.sparkContext.applicationId)

/**newM__D_1.flatMap{ case (cname, totals) =>
  if (totals.isEmpty) List((cname, null, null, null))
  else totals.map(t => (cname, t.orderdate, t.partkey, t._2)) }.sortBy(_._1).collect.foreach(println(_))**/
    
}
f
 }
}