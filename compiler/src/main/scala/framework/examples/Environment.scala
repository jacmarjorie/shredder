package framework.examples

import framework.common._
import framework.plans._
import framework.loader.csv._

trait Environment {

	val name: String

	val schema: Schema = Schema()

	val optLevel: Int = 1

	val capacity: Int

	val shred: Boolean

	val flex: Int

	val plannerType: String

	val zhost: String

	val zport: Int

	val tbls: Map[String, Type]

	def setup(shred: Boolean = shred, skew: Boolean = false, cache: Boolean = false): String

	val queries: Vector[Query]

	val plans: Vector[(CExpr, Int)] 

	val cacheStrategy: Option[CacheFactory]

}