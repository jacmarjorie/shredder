package framework.plans

import framework.common._
import scala.collection.mutable.Map

case class Result(profit: Double, selected: Vector[(Integer, CNamed)])

class DynamicCachePlanner(covers: Map[Integer, CostEstimate]) {

	val initialSize = covers.size
	val coverSet = covers.toVector

	val init = Result(0.0, Vector.empty[(Integer, CNamed)])

	def max(x: Result, y: Result): Result = if (x.profit >= y.profit) x else y

	def solve(capacity: Double, i: Int = 0, accum: Result = init): Result = {

		if (i < initialSize){ //&& accum.profit <= capacity){

			val (sig: Integer, item: CostEstimate) = coverSet(i)
			val profit = item.profit
			val weight = item.est.outSize

			if (weight > capacity) solve(capacity, i+1, accum)
			else {

				val cont = solve(capacity - weight, i+1, 
					Result(accum.profit + profit, accum.selected :+ (sig, item.plan)))

				val next = solve(capacity, i+1, accum)
				
				max(cont, next)

			}

		}else accum

	}
	
}