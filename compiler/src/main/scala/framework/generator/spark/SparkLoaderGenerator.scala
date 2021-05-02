package framework.generator.spark

import framework.common._

import framework.plans._
import framework.utils.Utils.ind

object SparkLoaderGenerator extends SparkTypeHandler {

  var types: Map[Type, String] = Map()
  override val BAGTYPE: String = "Seq"


  def generateSchema(tp: List[(String, Type)]): String = 
      tp.map(m => 
        s"""StructField("${m._1}", ${generateSqlType(m._2)})"""
      ).mkString("Array(", ",\n",")")

  def generateSqlType(tp: Type): String = tp match {
    case StringType => "StringType"
    case IntType => "IntegerType"
    case DoubleType => "DoubleType"
    case _ => sys.error("not supported")
  }

  def generateTypeDef(name: String, tp: Type): String = {
    val translator = new NRCTranslator{}
    handleType(translator.translate(tp))
    typelst.map(x => generateTypeDef(x)).mkString("\n")
  }

  def generateTypeDef(name: String, tp: List[(String, Type)]): String = {
    s"case class $name(${tp.map(x => s"${x._1}: ${generateType(x._2)}").mkString(", ")})"
  }

  def generateLoader(tname: String, tp: List[(String, Type)], header: Boolean = true, delimiter: String = ","): String = {
    s"""|package sparkutils.loader
        |/** Generated Code **/
        |import org.apache.spark.sql.Dataset
        |import org.apache.spark.sql.SparkSession
        |import org.apache.spark.sql.types._
        |
        |${generateTypeDef(tname, tp)}
        |
        |class ${tname}Loader(spark: SparkSession) extends Table[$tname] {
        | 
        |   import spark.implicits._
        |   val schema = StructType(${generateSchema(tp)})
        |   val header: Boolean = $header
        |   val delimiter: String = "$delimiter"
        |   
        |   def load(path: String): Dataset[$tname] = {
        |     spark.read.schema(schema)
        |       .option("header", header)
        |       .option("delimiter", delimiter)
        |       .csv(path)
        |       .as[$tname]        
        |   }
        |}
        |""".stripMargin
  }

}