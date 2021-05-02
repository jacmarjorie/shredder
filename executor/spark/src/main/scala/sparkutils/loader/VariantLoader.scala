package sparkutils.loader

import org.apache.spark.SparkConf
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import scala.collection.JavaConverters._
import org.apache.spark.rdd.RDD
import org.apache.hadoop.io.LongWritable
import htsjdk.variant.variantcontext.{CommonInfo, VariantContext, Genotype}
import org.seqdoop.hadoop_bam.{VCFInputFormat, VariantContextWritable}
import org.apache.spark.sql.types.{StringType, IntegerType, StructField, StructType}
import sparkutils.Config

/** Load variant informatin from VCF files, including consensus VCFs. 
  * This loader is dependent on the htsjdk and hadoop-bam.
  *
  */

case class Call(g_sample: String, call: Int)
case class CallBig(g_sample: String, call: Long)
case class Variant(id: String, contig: String, start: Int, reference: String, 
  alternate: String, genotypes: Seq[Call])
case class VariantBig(id: String, contig: String, start: Long, reference: String, 
  alternate: String, genotypes: Seq[CallBig])
case class IVariant(index: Long, contig: String, start: Int, reference: String, 
  alternate: String, genotypes: Seq[Call])

case class SCall(_1: Long, g_sample: String, call: Int)
case class SVariant(contig: String, start: Int, reference: String,
                    alternate: String, genotypes: Long)

case class GeneInfo(contig: String, name: String, id: String, strand: String, affect: String)

case class Variant2(id: String, contig: String, start: Int, reference: String,
                    alternate: String, gene: List[GeneInfo])

case class SNP(id: String, chr: Int, pos: Int)

class VariantLoader(spark: SparkSession, path: String) extends Serializable {

  import spark.implicits._

  implicit val genotypeEncoder = Encoders.product[Call]
  implicit val variantEncoder = Encoders.product[Variant]

  def loadVCF: RDD[VariantContext] = {
    spark.sparkContext
      .newAPIHadoopFile[LongWritable, VariantContextWritable, VCFInputFormat](path)
      .map(_._2.get)
  }

  def loadAnnotated: Dataset[Variant2] = {
    spark.sparkContext
      .newAPIHadoopFile[LongWritable, VariantContextWritable, VCFInputFormat](path)
      .map{ case (k, v) =>
        val variant = v.get
        val genotypes = variant.getGenotypes.iterator.asScala.toSeq.map(s => Call(s.getSampleName, callCategory(s)))
        Variant2(variant.getID, variant.getContig, variant.getStart, variant.getReference.toString,
          variant.getAlternateAllele(0).toString, List(getGene(variant)))
      }.toDF().as[Variant2]
  }

  def loadDS: Dataset[Variant] = {
    spark.sparkContext
      .newAPIHadoopFile[LongWritable, VariantContextWritable, VCFInputFormat](path)
      .map{ case (k, v) =>
        val variant = v.get
        val genotypes = variant.getGenotypes.iterator.asScala.toSeq.map(s => Call(s.getSampleName, callCategory(s)))
        Variant(variant.getID, variant.getContig, variant.getStart, variant.getReference.toString, 
          variant.getAlternateAllele(0).toString, genotypes)
      }.toDF().as[Variant].repartition(Config.maxPartitions) 
  }

  def shredDS: (Dataset[SVariant], Dataset[SCall]) = {
    val input = loadDS.withColumn("index", monotonically_increasing_id()).as[IVariant]
    val variants = input.drop("genotypes").withColumnRenamed("index", "genotypes").as[SVariant]
    val genotypes = input.flatMap{
      case variant => variant.genotypes.map{
        case genotype => SCall(variant.index, genotype.g_sample, genotype.call)
      }
    }.as[SCall]
    (variants, genotypes.repartition($"_1"))
  }

  def loadSnps(spath: String): Dataset[SNP] = {
    val schema = StructType(Array(
      StructField("id", StringType),
      StructField("chr", IntegerType),
      StructField("pos", IntegerType)
    ))
    spark.read.schema(schema)
      .option("header", true)
      .option("delimiter", "\t")
      .csv(spath)
      .as[SNP]
  }

  private def getGene(v: VariantContext): GeneInfo = {
    val info = v.getCommonInfo.getAttributeAsString("VA", "").split(":")
    GeneInfo(info(0), info(1), info(2), info(3), info(4))
  }

  private def callCategory(g: Genotype): Int = g match {
    case c if g.isHet => 1
    case c if g.isHomVar => 2
    case _ => 0
  }

}
