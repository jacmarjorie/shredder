package framework.plans

import framework.common._
import scala.collection.mutable.Map
import scala.collection.mutable.Stack

// IDEAS:
// only consider a pass that is globally maximal

class GreedyCachePlanner(covers: Map[Integer, CostEstimate], capacity: Double) {

	val knapsack = Map.empty[Integer, CNamed]

	val sortedCovers = covers.toVector.sortBy(s => s._2.profit)
	val initialSize = covers.size

	def solve(availability: Double = 0.0, fraction: Double = 1.0, i: Int = 0, retry: Boolean = false): Unit = {

		// restart with new fraction
		if (i < initialSize && fraction > .499){

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

		}
		else if (retry) { 
			// reset and try again
			knapsack.clear
			solve(0.0, fraction * .75, 0, retry) 
		}

	}


}