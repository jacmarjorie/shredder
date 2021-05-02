package framework.plans

import scala.collection.immutable.{Map => IMap}
import scala.collection.mutable.{HashMap, Map}

case class Estimate(inSize: Double, outSize: Double, 
  inRows: Double, outRows: Double, cpu: Double, network: Double){

  def total: Double = inSize + outSize + inRows + outRows + cpu + network
  def cost: Double = outSize + outRows + cpu + network

}

case class CostSE(wid: Int, subplan: CExpr, height: Int, est: Estimate)
case class CostCE(cover: CExpr, sig: Integer, ses: List[CostSE], est: Estimate)
case class CostEstimate(plan: CNamed, profit: Double, est: Estimate, wids: IMap[Int, Int])

class Cost(stats: Map[String, Statistics]) extends Extensions {

  val DISKREAD = 0.1
  val NETWORK = 10.0
  val NOSHUFF = 0.00001
  val RAMREAD = 0.001
  val RAMWRITE = 0.1
  val SELECTIVITY = 0.33
  val INDEXCOST = 1.1

  val NESTSIZE = 2.0
  val NESTROWS = 10.0
  val DEFAULTINC = 1.0

  // used to estimate row count from size
  val AVGSIZE = if (stats.nonEmpty){
      var i = 0
      // get the estimate size per element
      val summed = stats.filter(s => s._2.rowCount > 1.0).map(s => 
        {i+=1; (s._2.sizeInKB / s._2.rowCount)}
      ).reduce(_+_)
      // average size per element
      summed / i
    }else 100.0

  def estimateRows(rows: Double, size: Double): Double = {
    if (rows <= 1.0 && size > 1.0) {
      size / AVGSIZE
    }else {
      rows
    }
  }

  val default = Estimate(DEFAULTINC, DEFAULTINC, DEFAULTINC, DEFAULTINC, DEFAULTINC, DEFAULTINC)
  val statDefault = Statistics(1L, 1L)

  def estimate(plans: Vector[(CExpr, Int)]): Map[String, Estimate] = {
    val ests = Map.empty[String, Estimate]
    plans.foreach{ p => p._1 match {
      case LinearCSet(cs) => cs.foreach{ 
        c => c match {
          case c1:CNamed => ests(c1.name) = estimate(c1)
          case _ => ???
        }
      }
      case p1 => ??? //ests(p._2+"") = estimate(p1)
    }}
    ests
  }

  // single plan estimate
  def estimate(plan: CExpr): Estimate = {
    val stat = stats.getOrElse(plan.vstr, statDefault)
    val sel = estSelectivity(plan)
    plan match {

      // for testing
      case y if stats.isEmpty => default

      // todo inner vs outer join
      // this is the simpliest approach right now, some combination of left and right
      // need to use cardinality information where possible
      case j:JoinOp =>
        val leftEst = estimate(j.left)
        val rightEst = estimate(j.right)

        // some factor of cardinalities
        //val outsize = leftEst.outSize + rightEst.outSize 
        // val outsize = (stat.sizeInBytes / 1024)
        // this should be based on distincts, but doing this 
        // for now
        val lrows = leftEst.outRows.toDouble
        val rrows = rightEst.outRows.toDouble
        val lsize = leftEst.outSize.toDouble
        val rsize = rightEst.outSize.toDouble

        val outrows = if (j.cond == Constant(true)) lrows * rrows else Math.max(lrows, rrows)
        val outsize = if (j.cond == Constant(true)) lsize * rsize else Math.min(lsize, rsize)

        // network cost of the largest relation, plus what it costs to 
        // perform the operation give estimated output rows
        // val rowCount = estimateRows(stat.rowCount + 0.0, stat.sizeInKB + 0.0)
        val network = leftEst.network + rightEst.network + (lrows * NETWORK) + (outrows * .00002)

        val cpu = leftEst.cpu + rightEst.cpu + (outrows * .00002)

        val insize = lsize + rsize
        val inrows = lrows * rrows


        // println("join stat found: "+outrows+", "+outsize)
        Estimate(insize, outsize, inrows, outrows, network, cpu)

      // same here with unnest, need to use average nested collection sizes
      // cpu time is more because we are grouping, network is quite a 
      // bit since potentially large collections are being shuffled
      case n:Nest => 
        val childEst = estimate(n.in)
        // some growth factor based on column, for now just nestsize
        val outsize = childEst.outSize * NESTSIZE
        val outrows = childEst.outRows * ((n.key.size * 1.0) / n.in.tp.attrs.size)

        // four times a simple operation (noshuff)
        val cpu = (childEst.outRows * (.00004)) + childEst.cpu
        // network could be dependent on the size of the values
        val network = (childEst.outRows * NETWORK) + childEst.network

        // println("nest stat found: "+outrows+", "+outsize)
        Estimate(childEst.inSize, outsize, childEst.inRows, outrows, cpu, network)

      // TODO see what spark does for aggregation estimate
      // reduce factor based on key and values compared to child columns
      // effects outsize and outrows
      // cpu cost should be evaluated, but seems to be slightly more than 
      // a simple operation; TODO what other factors should be considered?
      case r:Reduce => 
        val childEst = estimate(r.in)
        val factor = ((r.keys.size + r.values.size) * 1.0) / r.in.tp.attrs.size

        val outsize = childEst.outSize * factor
        // possibly use column stats (dedup on keys)
        val outrows = childEst.outRows * factor

        // two times a simple operation (noshuff)
        val cpu = (childEst.outRows * (.00002)) + childEst.cpu
        val network = (childEst.outRows * NETWORK) + childEst.network

        // println("reduce stat found: "+outrows+", "+outsize)
        Estimate(childEst.inSize, outsize, childEst.inRows, outrows, cpu, network)

      // note statistics from spark seem to not be estimated for 
      // outerunnest, make sure that this gets around that
      // TODO capture column statistics to get proper NESTSIZE and NESTROWS
      // outsize should be the child outsize * some average size of nested collections
      // outrows should be the child outrows * some average rows in the nested collections
      // minor cpu addition, but no network cost
      case u:UnnestOp => 
        val childEst = estimate(u.in)
        val cpu = (childEst.outRows * NOSHUFF) + childEst.cpu
        val outsize = childEst.outSize * NESTSIZE
        val outrows = childEst.outRows * NESTROWS
        // println("unnest stat found: "+outrows+", "+outsize)
        Estimate(childEst.inSize, outsize, childEst.inRows, outrows, cpu, childEst.network)

      // TODO selectivity estmate, using default currently
      // outsize and rows are based on the selectivity of the filter
      // only minor cpu additions, no network cost 
      case s:Select => 
        val childEst = estimate(s.in)
        val cpu = (childEst.outRows * NOSHUFF) + childEst.cpu
        val (outsize, outrows) = s.p match {
          case Constant(true) => (childEst.outSize, childEst.outRows)
          case _ => (childEst.outSize * sel, childEst.outRows * sel)
        }

        // println("select stat found: "+outrows+", "+outsize)
        Estimate(childEst.inSize, outsize, childEst.inRows, outrows, cpu, childEst.network)

      // projection will reduce the number of columns, so should 
      // reduce the size of the child output by a factor proportional 
      // to the number of columns being removed
      // cpu addition is minor, but no changes to rows or network cost
      case p:Projection => 
        val childEst = estimate(p.in)
        val outsize = ((p.tp.attrs.size * 1.0) / p.in.tp.attrs.size) * childEst.outSize
        val cpu = childEst.cpu + (childEst.outRows * NOSHUFF)

        // println("projection stat found: "+childEst.inRows+", "+outsize)
        Estimate(childEst.inSize, outsize, childEst.inRows, outsize, cpu, childEst.network)

      // adding an index will add one column, so minor addition to size
      // adding the index will also take a small amount of cpu time
      // no network or additional rows added
      case i:AddIndex => 
        val childEst = estimate(i.in)
        val outsize = childEst.outSize * INDEXCOST
        val cpu = childEst.cpu + (childEst.outRows * NOSHUFF)

        // println("index stat found: "+childEst.outRows+", "+outsize+", ")
        Estimate(childEst.inSize, outsize, childEst.inRows, childEst.outRows, cpu, childEst.network)

      // the base cost estimate for all input relations
      // size and rows directly from stats
      // assume no network cost
      // cpu is just the time to scan

      case c:CNamed => 
        println(c.name)
        println(stat)
        estimate(c.e)

      case _ => 
        val size = stat.sizeInKB + 0.0
        val rows = estimateRows(stat.rowCount + 0.0, size)
        // println("base stat found: "+size+", "+rows)

        Estimate(size, size, rows, rows, rows * DISKREAD, 0.0)

    }
  }

  def selectCovers(covers: IMap[Integer, CNamed], subs: Map[Integer, List[SE]], flexibility: Int = 0): Map[Integer, CostEstimate] = {

    val selected = Map.empty[Integer, CostEstimate]
    
    covers.foreach{ c =>
      if (!c._2.e.isCacheUnfriendly){
        val ses = subs(c._1)
        // total cost of evaluating all subexpressions
        val totalwork = ses.map(s => estimate(s.subplan).total).reduce(_ + _) 

        // total cost of cover + add cache + (access cache * accesses)
        val cest = estimate(c._2.e)

        // adjust the profit here
        val covercost = cest.total + estMaterialization(cest.outSize) + 
          (estRetrieval(cest.outSize) * ses.size)

        val profit = totalwork - covercost

        if (profit > 0) {
          // get wids with height
          val wids = ses.map(s => (s.wid, s.height)).toMap
          selected(c._1) = CostEstimate(c._2, profit, cest, wids)

        }

      }
    }

    if (flexibility > 2) selected
    else minimizeOverlap(selected, flexibility)

  }


  // TODO option to adjust profits of subexpressions if they are kept
  def minimizeOverlap(covers: Map[Integer, CostEstimate], flexibility: Int = 0): Map[Integer, CostEstimate] = {
    
    var selected = covers
    covers.foreach{ c1 => 

      (covers - c1._1).foreach{ c2 =>

        // if c1 is a descendent of c2
        if (find(c2._2.plan, c1._2.plan)){

          // do not allow subexpressions at all (verona)
          if (flexibility == 0) selected = selected - c1._1
          else{


            val inSameQueries = c1._2.wids.keySet == c2._2.wids.keySet

            // slightly more flexible, only remove if in same query
            if (flexibility == 1 && inSameQueries) selected = selected - c1._1

            // most flexible, only remove if there are not profit / weight advantages
            else if (flexibility == 2 && inSameQueries && c1._2.profit <= c2._2.profit && c1._2.est.outSize >= c2._2.est.outSize){

              selected = selected - c1._1

            }

          } 

        }

      }

    }
    selected
  }

  def printEstimateAndStat(covers: IMap[Integer, CNamed], subs: Map[Integer, List[SE]]): Unit = {
    covers.foreach{ c =>
      val plan = c._2.e
      val est = estimate(plan)
      println("Cover:")
      println(Printer.quote(plan))
      println(stats.getOrElse(plan.vstr, statDefault))
      println(est)
      println("Subs:")
      val ses = subs(c._1)
      ses.foreach{ s =>
        val est = estimate(s.subplan)
        println(Printer.quote(s.subplan))
        println(stats.getOrElse(s.subplan.vstr, statDefault))
        println(est)
      }
      println("")
    }
  }

  def estMaterialization(card: Double): Double = card * RAMWRITE

  def estRetrieval(card: Double): Double = card * RAMREAD


  def compare(s1: (CExpr, Statistics), s2: (CExpr, Statistics)): CExpr = 
    if (s1._2.lessThan(s2._2)) s1._1 else s2._1

  // TODO
  def estSelectivity(plan: CExpr): Double = plan match {
    case _ => SELECTIVITY
  }

  // TODO this will need histogram information
  def estNestColumn(plan: CExpr, attr: String): (Double, Double) = (NESTSIZE, NESTROWS)



}