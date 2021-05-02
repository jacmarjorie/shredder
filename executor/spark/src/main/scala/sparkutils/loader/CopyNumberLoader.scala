package sparkutils.loader
/** Generated Code **/
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._
import java.io.File
import org.apache.spark.sql.functions._

/** Loads copy number data from GDC / ICGC (gene_level files), such as:
  * https://portal.gdc.cancer.gov/files/a27ea895-37a6-4cbb-8aa9-26abad949937
  *
  **/

case class CopyNumber(cn_gene_id: String, cn_gene_name: String, cn_chromosome: String, cn_start: Int, cn_end: Int, cn_copy_number: Int, min_copy_number: Int, max_copy_number: Int, cn_aliquot_uuid: String)

class CopyNumberLoader(spark: SparkSession) extends Serializable {
 
   import spark.implicits._
   val schema = StructType(Array(StructField("cn_gene_id", StringType),
    StructField("cn_gene_name", StringType),
    StructField("cn_chromosome", StringType),
    StructField("cn_start", IntegerType),
    StructField("cn_end", IntegerType),
    StructField("cn_copy_number", IntegerType, nullable=true),
    StructField("min_copy_number", IntegerType, nullable=true),
    StructField("max_copy_number", IntegerType, nullable=true)))

   val header: Boolean = true
   val delimiter: String = "\t"
  

  val aliquotUdf = udf { s: String => s.split("\\.")(1) }

  def load(path: String, dir: Boolean = true): Dataset[CopyNumber] = {
    val files = if (dir) (new File(path)).listFiles().toSeq.map(f => s"$path/${f.getName}")
      else Seq(path)
    spark.read.schema(schema)
      .format("csv")
      .option("header", "true")
      .option("comment", "#")
      .option("delimiter", delimiter)
      .option("inferSchema", "true")
      .load(files:_*)
    .withColumn("filename", input_file_name)
    .withColumn("cn_aliquot_uuid", aliquotUdf($"filename"))
    .drop("filename")
    .withColumn("cn_copy_number", when($"cn_copy_number".isNull, 0).otherwise($"cn_copy_number"))
    .withColumn("min_copy_number", when($"min_copy_number".isNull, 0).otherwise($"min_copy_number"))
    .withColumn("max_copy_number", when($"max_copy_number".isNull, 0).otherwise($"max_copy_number"))
    .as[CopyNumber]
  }


}

