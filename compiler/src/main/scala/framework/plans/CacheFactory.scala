package framework.plans

import framework.common._

class CacheFactory(progs: Vector[(CExpr, Int)], capacity: Int, flex: Int = 0, ptype: String = "greedy", 
    zhost: String = "localhost", zport: Int = 8085) {

    val seBuilder = SEBuilder(progs)
    seBuilder.updateSubexprs()

    val subs = seBuilder.sharedSubs()
    val subexprs = seBuilder.getSubexprs()

    val nmap = seBuilder.getNameMap

    // subs.foreach{
    //     s => println(s._1)
    //     s._2.foreach{ s2 =>
    //         println(Printer.quote(s2.subplan))
    //     }
    // }

    val ces = CEBuilder.buildCoverMap(subs, nmap)
    
    // printCE(ces)

    val statsCollector = new StatsCollector(progs, zhost = zhost, zport = zport)
    val stats = statsCollector.getCost(subs, ces)

    val cost = new Cost(stats)

    val selected = cost.selectCovers(ces, subs, flexibility = flex)

    // selected0.foreach{ s =>
    //   println(s._1)
    //   println(s._2.profit)
    //   println(Printer.quote(s._2.plan))
    // }

    // val totalSize = selected.map(s => s._2.est.outSize).reduce(_+_)

    // println(totalSize)

    val candidates = ptype match {
        case "dynamic" =>
            val planner = new DynamicCachePlanner(selected)
            planner.solve(capacity).selected

        case "greedy" => 
            val planner = new GreedyCachePlanner(selected, capacity)
            planner.solve()
            planner.knapsack

        case _ => sys.error(s"unsupported cache type $ptype")
    }

    println("These are the input covers:")
    candidates.foreach{
      p => println(Printer.quote(p._2))
    }

    val rewriter = QueryRewriter(subexprs, names = nmap)
    val newplans = rewriter.rewritePlans(progs, candidates.toMap)
    // val newcovers = rewriter.coverset
    val execOrder = rewriter.ordered

    println("Execution order:")
    execOrder.foreach{
        p => println(Printer.quote(p))
    }

}