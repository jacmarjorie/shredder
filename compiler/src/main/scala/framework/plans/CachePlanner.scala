package framework.plans

import framework.common._
import scala.collection.mutable.Map
import scala.collection.mutable.Stack

class CachePlanner(covers: Map[Integer, CostEstimate], capacity: Double = 1.0) {

	val knapsack = Map.empty[Integer, CExpr]

	val sortedCovers = covers.toVector.sortBy(s => s._2.profit)
	val initialSize = covers.size

	def solve(availability: Double = 0.0, fraction: Double = 1.0, i: Int = 0): Unit = {

		// restart with new fraction
		if (i <= initialSize && fraction > .499){

			var newAvail = availability

			if (availability < capacity){
				
				val (sig, candidate) = sortedCovers(i)
				val sizeWithAdd = candidate.est.outSize + newAvail

				if (sizeWithAdd <= capacity) {

					knapsack(sig) = candidate.plan
					newAvail = sizeWithAdd

				}

				solve(newAvail, fraction, i+1)

			}

		}else solve(0.0, fraction * .75, 0)

	}


}