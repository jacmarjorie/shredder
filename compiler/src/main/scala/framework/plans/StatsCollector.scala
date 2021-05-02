package framework.plans

import framework.common._
import scala.sys.process._
import scala.language.postfixOps
import framework.generator.spark._
import java.io._
import java.util.UUID.randomUUID
import scala.collection.mutable.Map
import scala.collection.immutable.{Map => IMap}
import scala.io.Source

case class Statistics(sizeInKB: Double, rowCount: Double) {

  def lessThan(s2: Any): Boolean = s2 match {
    case Statistics(size, rows) => 
      if (rows > -1 && rowCount > -1) sizeInKB <= size && rowCount <= rows
      else sizeInKB <= size
    case _ => false
  }

}

class StatsCollector(progs: Vector[(CExpr, Int)], zhost: String, zport: Int, inputs: String = "") { //, inputs: Map[String, String] = Map.empty[String, String]) {

  val data: String = if (inputs.isEmpty){
    s"""|   val copynumber = spark.table("fcopynumber")
        |   val occurrences = spark.table("foccurrences")
        |   val samples = spark.table("fsamples")
        |   val IBag_copynumber__D = copynumber
        |   val IBag_samples__D = samples
        |   val IBag_occurrences__D = spark.table("fodict1")
        |   val IDict_occurrences__D_transcript_consequences = spark.table("fodict2")
        |   val IDict_occurrences__D_transcript_consequences_consequence_terms = spark.table("fodict3")
      """
  }else inputs 
  
  val zep = new ZeppelinFactory(host = zhost, port = zport)
  val nameMap = Map.empty[String, String] //inputs
  val nameMapRev = Map.empty[String, String]
  //nameMap.foreach{x => nameMapRev(x._2) = x._1}

  val codeMap = Map.empty[String, String]
  val statsMap = Map.empty[String, Statistics]

  val KB = BigDecimal(1024)

  val StatsRegex = "Stat\\((.*),(.*),(.*)\\)".r
  var inc = 0

  def readStats(s: String): (Option[String], Option[Statistics]) = s match {
    case StatsRegex(n, sb, rc) => 
      val sbl = (BigDecimal(sb) / KB).toDouble
      val src = rc match { case "-1" => -1.0; case _ => (BigDecimal(rc) / KB).toDouble }
      (Some(n), Some(Statistics(sbl, src)))
    case _ => (None, None)
  }

  //TODO
  def readOutput(s: String): (Option[String], Option[Statistics]) =    
    StatsRegex.findFirstIn(s) match {
      case Some(str) => readStats(str) 
      case _ => (None, None)
    }

  def generateSpark(plans: List[CNamed], notebk: Boolean = false, wprogs: Boolean = true): String = {
    val generator = new SparkDatasetGenerator(false, false, evalFinal=false, dedup = false)

    // this ensures that the full program is defined 
    // so that a call to a "materialized" query does not throw an error
    var queries = ""
    if (wprogs){
      for (p <- progs){
        val name = p match { 
          case (LinearCSet(cs), _) => 
            val cn = cs.last.asInstanceOf[CNamed]
            updateNameMap(cn.vstr, cn.name)
            cn.name
          case _ => "Query"+p._2 
        }
        val anfBase = new BaseOperatorANF{}
        val anfer = new Finalizer(anfBase)
        val anfed = anfBase.anf(anfer.finalize(p._1).asInstanceOf[anfBase.Rep])
        val gcode = s"""
          | /** ${Printer.quote(p._1)} **/
          | ${generator.generate(anfed)}
          | val stat$inc = ${name}.queryExecution.optimizedPlan.stats
          | println(genStat("${name}", stat$inc))
          |"""
        queries += gcode
        inc += 1
      }
    }

    // this should get stats for all covers
    // and all subexpressions that are in non-filtered sig
    for (p <- plans){
      p match {
        case CNamed(name, i:InputRef) => 
          val gcode = s"""
            | /** ${Printer.quote(i)} **/
            | val stat$inc = ${name}.queryExecution.optimizedPlan.stats
            | println(genStat("${name}", stat$inc))
            """
          if (wprogs) codeMap += (name -> gcode) else queries += gcode

        case _ =>
          val anfBase = new BaseOperatorANF{}
          val anfer = new Finalizer(anfBase)
          val anfed = anfBase.anf(anfer.finalize(p).asInstanceOf[anfBase.Rep])
          val gcode = s"""
            | /** ${Printer.quote(p)} **/
            | ${generator.generate(anfed)}
            | val stat$inc = ${p.name}.queryExecution.optimizedPlan.stats
            | println(genStat("${p.name}", stat$inc))
            """
          if (wprogs) codeMap += (p.name -> gcode) else queries += gcode
        }
      inc += 1
    }

    val ghead = generator.generateHeader()
    val genc = generator.generateEncoders()
    val bname = "GenerateCosts"
    if (!notebk) {
      var fname = "../executor/spark/src/main/scala/sparkutils/generated/${bname}.scala"
      val fconts = writeApplication("GenerateCosts", data, ghead, queries+"\n"+codeMap.map(_._2).mkString("\n"), genc)
      val printer = new PrintWriter(new FileOutputStream(new File(fname), false))
      printer.println(fconts)
      printer.close
      "DONE"
    }else {
      val noteid = zep.addNote(bname)
      // println(s"Writing to notebook: $noteid")
      val pcontents = writeParagraph(bname, data, ghead, queries+"\n"+codeMap.map(_._2).mkString("\n"), genc)
      val para = new JsonWriter().buildParagraph("Generated paragraph $qname", pcontents)
      val pid = zep.writeParagraph(noteid, para)
      // println(s"Writing to paragraph: $pid")
      val status = zep.runParaSync(noteid, pid)
      zep.deleteNote(noteid)
      status
    }
  }

  def getUUID: String = randomUUID().toString().replace("-", "")

  def updateNameMap(key: String, value: String): String = {
    if (nameMap contains key) {
      nameMapRev += (value -> key)
      nameMap(key)
    }else {
      nameMap += (key -> value)
      nameMapRev += (value -> key)
      value
    }
  }

  def getCost(plans: List[CE]): Map[String, Statistics] = {
    val cnames = plans.flatMap{
      case ce => 
        val ceName = updateNameMap(ce.cover.vstr, s"Cost${getUUID}")
        val namedSes = ce.ses.map(s => {
            val seName = updateNameMap(s.subplan.vstr, s"Cost${getUUID}")
            CNamed(seName, s.subplan)
          })
        CNamed(ceName, ce.cover) +: namedSes
    }
    runCost(cnames) 
  }

  def getStats(subs: Map[Integer, List[SE]], notebk: Boolean = true): Map[String, Statistics] = {
    val cplans = getSubs(subs)
    runCost(cplans, notebk) //, wprogs = false)
  }

  def getCost(subs: Map[Integer, List[SE]], covers: IMap[Integer, CNamed], notebk: Boolean = true): Map[String, Statistics] = {
    val coverList = covers.values.toList
    val plans = getSubs(subs) ++ coverList
    // could handle duplicates better
    coverList.foreach{ ce => updateNameMap(ce.vstr, ce.name) }
    runCost(plans, notebk)
  }

  def getSubs(plans: Map[Integer, List[SE]]): List[CNamed] = {
    plans.values.toList.flatMap{ ses => ses.map{
      case se => 
        se.subplan match {
          case i:InputRef => CNamed(updateNameMap(i.vstr, i.data), i)
          case sp => CNamed(updateNameMap(sp.vstr, s"Cost${getUUID}"), sp)
        }
      }
    }
  }

  def getCoverCost(plans: IMap[Integer, CNamed], notebk: Boolean = true): Map[String, Statistics] = {
    val cnames = plans.values.toList
    cnames.foreach{ ce => updateNameMap(ce.e.vstr, ce.name) }
    runCost(cnames, notebk) 
  }

  // this is a slightly faster solution
  def runCost(plans: List[CNamed], notebk: Boolean = false, wprogs: Boolean = true): Map[String, Statistics] = {
    val out = generateSpark(plans, notebk, wprogs = wprogs)
    for (line <- out.split("\n")){
      readStats(line) match {
        case (Some(name), Some(stat)) => 
          statsMap += (nameMapRev(name) -> stat)
        case _ => 
      }
    }
    statsMap
  }

    /** Writes a generated application for a query using Spark Datasets **/
  def writeApplication(appname: String, data: String, header: String, gcode: String, encoders: String): String  = {
    s"""
      |package sparkutils.generated
      |/** Generated **/
      |import org.apache.spark.SparkConf
      |import org.apache.spark.sql.SparkSession
      |import org.apache.spark.sql._
      |import org.apache.spark.sql.functions._
      |import org.apache.spark.sql.catalyst.plans.logical._
      |import org.apache.spark.sql.types._
      |import org.apache.spark.sql.expressions.scalalang._
      |import sparkutils._
      |import sparkutils.loader._
      |import sparkutils.skew.SkewDataset._
      |$header
      |case class Stat(name: String, sizeInBytes:String, rowCount:String)
      |object $appname {
      | def main(args: Array[String]){
      |   val conf = new SparkConf()
      |     .setAppName("${appname}Cost")
      |   val spark = SparkSession.builder().config(conf).getOrCreate()
      |   $encoders
      |   import spark.implicits._
      |   def genStat(n: String, s: Statistics): Stat = s.rowCount match {
      |     case Some(rc) => Stat(n, s.sizeInBytes.toString, rc.toString)
      |     case _ => Stat(n, s.sizeInBytes.toString, "-1")
      |   }
      |   $data
      |   $gcode
      | }
      |}""".stripMargin
  }

  def writeParagraph(appname: String, data: String, header: String, gcode: String, encoders: String): String  = {
    s"""
      |import org.apache.spark.sql._
      |import org.apache.spark.sql.functions._
      |import org.apache.spark.sql.catalyst.plans.logical._
      |import org.apache.spark.sql.types._
      |import org.apache.spark.sql.expressions.scalalang._
      |import sparkutils._
      |import sparkutils.loader._
      |import sparkutils.skew.SkewDataset._
      |import java.io._
      |$header
      |case class Stat(name: String, sizeInBytes:String, rowCount:String)
      |$encoders
      |import spark.implicits._
      |def genStat(n: String, s: Statistics): Stat = s.rowCount match {
      |  case Some(rc) => Stat(n, s.sizeInBytes.toString, rc.toString)
      |  case _ => Stat(n, s.sizeInBytes.toString, "-1")
      |}
      |$data
      |$gcode
    """.stripMargin

      // |val fname = "/Users/jac/code/trance/compiler/out"
      // |val printer = new PrintWriter(new FileOutputStream(new File(fname), false))
  }



}
