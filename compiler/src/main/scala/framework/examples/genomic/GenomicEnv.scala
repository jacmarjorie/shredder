package framework.examples.genomic

import framework.common._
import framework.examples.{Query, Environment}
import framework.plans._
import framework.loader.csv._


class GenomicEnv(val capacity: Int, val shred: Boolean = false, val flex: Int = 0, 
	ptype: String = "greedy", repeat: Int = 1,
	val zhost: String = "localhost", val zport: Int = 8085) extends Environment {
	
	val name = "GenomicEnv"

	// val shred: Boolean = init_shred

	// val capacity: Int = init_capacity

	// val flex: Int = init_flex

	val plannerType: String = ptype

	val dtp = new DriverGene{ val name: String = ""; val program: Program = Program(Nil)}

  	val tbls = Map("occurrences" -> dtp.occurmids.tp, 
                  "copynumber" -> dtp.copynum.tp, 
                  "samples" -> dtp.samples.tp)

	override def setup(shred: Boolean = shred, skew: Boolean = false, cache: Boolean = false): String = {
		if (shred){
  		  s"""|val samples = spark.table("fsamples")
	          |val IBag_samples__D = samples
	          |${if (cache) "IBag_samples__D.cache; IBag_samples__D.count" else ""}
	          |
	          |val copynumber = spark.table("fcopynumber")
	          |val IBag_copynumber__D = copynumber
	          |${if (cache) "IBag_copynumber__D.cache; IBag_copynumber__D.count" else ""}
	          |
	          |val odict1 = spark.table("fodict1")
	          |val IBag_occurrences__D = odict1
	          |${if (cache) "IBag_copynumber__D.cache; IBag_copynumber__D.count" else ""}
	          |
	          |// issue with partial shredding here
	          |val odict2 = spark.table("fodict2").drop("flags")
	          |val IDict_occurrences__D_transcript_consequences = odict2
	          |${if (cache) "IDict_occurrences__D_transcript_consequences.cache" else ""}
	          |${if (cache) "IDict_occurrences__D_transcript_consequences.count" else ""}
	          |
	          |val odict3 = spark.table("fodict3")
	          |val IDict_occurrences__D_transcript_consequences_consequence_terms = odict3
	          |${if (cache) "IDict_occurrences__D_transcript_consequences_consequence_terms.cache" else ""}
	          |${if (cache) "IDict_occurrences__D_transcript_consequences_consequence_terms.count" else ""}
	          |""".stripMargin
		}else{
  		  s"""|val samples = spark.table("fsamples")
	          |${if (cache) "samples.cache; samples.count" else ""}
	          |
	          |val copynumber = spark.table("fcopynumber")
	          |${if (cache) "copynumber.cache; copynumber.count" else ""}
	          |
	          |val occurrences = spark.table("foccurrences")
	          |${if (cache) "occurrences.cache; occurrences.count" else ""}
	          |""".stripMargin
		}
	}

	val queries: Vector[Query] = Vector(TestBaseQuery, TestBaseQuery2)

	val plans: Vector[(CExpr, Int)] = queries.map(q => 
		q.optimized(shred, optLevel, schema).asInstanceOf[CExpr]).zipWithIndex

	val cacheStrategy: Option[CacheFactory] = 
		Some(new CacheFactory(plans, capacity, flex = flex, ptype = plannerType))

}

class LetTestEnv(val capacity: Int, val shred: Boolean = false, 
	val flex: Int = 0, ptype: String = "greedy", repeat: Int = 1, 
	val zhost: String = "localhost", val zport: Int = 8085) extends Environment {
	
	val name = "LetTestEnv"

	// val shred: Boolean = init_shred

	// val capacity: Int = init_capacity

	// val flex: Int = init_flex

	val plannerType: String = ptype

	val dtp = new DriverGene{ val name: String = ""; val program: Program = Program(Nil)}

  	val tbls = Map("occurrences" -> dtp.occurmids.tp, 
                  "copynumber" -> dtp.copynum.tp, 
                  "samples" -> dtp.samples.tp)

	override def setup(shred: Boolean = shred, skew: Boolean = false, cache: Boolean = false): String = {
		if (shred){
  		  s"""|val samples = spark.table("samples")
	          |val IBag_samples__D = samples
	          |${if (cache) "IBag_samples__D.cache; IBag_samples__D.count" else ""}
	          |
	          |val copynumber = spark.table("copynumber")
	          |val IBag_copynumber__D = copynumber
	          |${if (cache) "IBag_copynumber__D.cache; IBag_copynumber__D.count" else ""}
	          |
	          |val odict1 = spark.table("odict1")
	          |val IBag_occurrences__D = odict1
	          |${if (cache) "IBag_copynumber__D.cache; IBag_copynumber__D.count" else ""}
	          |
	          |// issue with partial shredding here
	          |val odict2 = spark.table("odict2").drop("flags")
	          |val IDict_occurrences__D_transcript_consequences = odict2
	          |${if (cache) "IDict_occurrences__D_transcript_consequences.cache" else ""}
	          |${if (cache) "IDict_occurrences__D_transcript_consequences.count" else ""}
	          |
	          |val odict3 = spark.table("odict3")
	          |val IDict_occurrences__D_transcript_consequences_consequence_terms = odict3
	          |${if (cache) "IDict_occurrences__D_transcript_consequences_consequence_terms.cache" else ""}
	          |${if (cache) "IDict_occurrences__D_transcript_consequences_consequence_terms.count" else ""}
	          |""".stripMargin
		}else{
  		  s"""|val samples = spark.table("samples")
	          |${if (cache) "samples.cache; samples.count" else ""}
	          |
	          |val copynumber = spark.table("copynumber")
	          |${if (cache) "copynumber.cache; copynumber.count" else ""}
	          |
	          |val occurrences = spark.table("occurrences")
	          |${if (cache) "occurrences.cache; occurrences.count" else ""}
	          |""".stripMargin
		}
	}

	val query1 = LetTest2
	val query2 = LetTest1
	// val query1 = LetTest3
	// val query2 = LetTest4

	val queries: Vector[Query] = Vector(query1, query2)
	// val queries1: Vector[Query] = Vector(query1) //, query2)
	// val queries2: Vector[Query] = Vector(query2)

	val plans: Vector[(CExpr, Int)] = queries.map(q => 
		q.optimized(shred, optLevel, schema).asInstanceOf[CExpr]).zipWithIndex
	// val plans1: Vector[(CExpr, Int)] = queries1.map(q => 
	// 	q.optimized(shred, optLevel, schema).asInstanceOf[CExpr]).zipWithIndex
	// val plans2: Vector[(CExpr, Int)] = queries2.map(q => 
	// 	q.optimized(shred, optLevel, schema).asInstanceOf[CExpr]).zipWithIndex

 	val seBuilder = SEBuilder(plans)
 	seBuilder.updateSubexprs()
 	val subs = seBuilder.sharedSubs(limit = false)

	val stater = new StatsCollector(plans, zhost = zhost, zport = zport, inputs = this.setup(shred = shred))
	val stats = stater.getStats(subs)

	val cost = new Cost(stats)
	val estimates = cost.estimate(plans)
	estimates.foreach{
		p => 
			println("estimate for "+p._1)
			println(p._2)
			println(p._2.total)
			println(p._2.cost)
	}

	val cacheStrategy: Option[CacheFactory] = None

}
