
package sparkutils.generated
/** Generated **/
import org.apache.spark.SparkConf
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.spark.sql.expressions.scalalang._
import scala.collection.mutable.HashMap
import sparkutils._
import sparkutils.loader._
import sparkutils.skew.SkewDataset._
case class Recordde748c7cae9640b5aaccbd4aff918af0(bcr_patient_uuid: String, bcr_aliquot_uuid: String)
case class Recordaeb7cd807c4e45a182121d648337c810(cn_aliquot_uuid: String, cn_gene_id: String, cn_copy_number: Int)
case class Record1713f2ff91a64246bc9814502dd89ea8(bcr_patient_uuid: String, cn_aliquot_uuid: String, cn_gene_id: String, bcr_aliquot_uuid: String, cn_copy_number: Int)
case class Record5bc7e8cb7e264f78a221f275f5588533(sid: String, gene: String, cnum: Int)
case class Record0710816f4ef146fbb24e8c037a3e5d60(oid: String, donorId: String, transcript_consequences: String)
case class Record63771ce4f3864bad86d0b535f343d644(o__F_donorId: String, o__F_transcript_consequences: String)
case class Recordb297f49e08a64be590cfe5653ec87c16(oid: String, sid1: String, cands1: Record63771ce4f3864bad86d0b535f343d644)
case class Record2889c970379347c18c1e42c11899e5db(cands1: Record63771ce4f3864bad86d0b535f343d644)
case class Record3915ac8058cd4edba64820717337ad98(_LABEL: Record63771ce4f3864bad86d0b535f343d644)
case class Record4cc42dee5c3e4cbb8d8299bd6d648fad(polyphen_score: Double, sift_prediction: String, polyphen_prediction: String, flags: String, protein_end: Long, ts_strand: Long, impact: String, gene_id: String, amino_acids: String, sift_score: Double, cdna_start: Long, transcript_id: String, cdna_end: Long, exon: String, cds_end: Long, _1: String, cds_start: Long, intron: String, consequence_terms: String, case_id: String, protein_start: Long, variant_allele: String, codons: String, distance: Long)
case class Record970aa8e5d6924d3bbc2c96c104b315e9(impact: String, gene_id: String, _1: String)
case class Record3592b6f24f9640e986358a42c4a8f7a0(impact: String, gene_id: String, o__F_transcript_consequences_1: String)
case class Recordcdb26785726c4f8680cd65a5f15b7257(_LABEL: Record63771ce4f3864bad86d0b535f343d644, impact: String, gene_id: String)
case class Record7e58693d2adb41b9aa2d4e3cc2ac1f70(cnum: Int, impact: String, gene_id: String, gene: String, sid: String, _LABEL: Record63771ce4f3864bad86d0b535f343d644)
case class Recorda0262d840ff0461eb7533482ba5330b7(gene1: String, score1: Double, _1: Record63771ce4f3864bad86d0b535f343d644)
case class Recorddae95679c1fc44bb98248e02a38ef9ff(gene1: String, _1: Record63771ce4f3864bad86d0b535f343d644, score1: Double)
case class Recordefaf180c70f8465ba22cccfdb52095d9(_1: Record63771ce4f3864bad86d0b535f343d644, gene1: String)
object ShredExampleQuerySpark {
 def main(args: Array[String]){
   val sf = Config.datapath.split("/").last
   val conf = new SparkConf()
     .setAppName("ShredExampleQuerySpark"+sf)
     .set("spark.sql.shuffle.partitions", Config.maxPartitions.toString)
   val spark = SparkSession.builder().config(conf).getOrCreate()
   implicit val encoderRecord63771ce4f3864bad86d0b535f343d644: Encoder[Record63771ce4f3864bad86d0b535f343d644] = Encoders.product[Record63771ce4f3864bad86d0b535f343d644]
val udfRecord63771ce4f3864bad86d0b535f343d644 = udf { (o__F_donorId: String,o__F_transcript_consequences: String) => Record63771ce4f3864bad86d0b535f343d644(o__F_donorId,o__F_transcript_consequences) }

   import spark.implicits._
   
    def f = {
 
 
var start0 = System.currentTimeMillis()
val x102 = IBag_samples__D.select("bcr_patient_uuid", "bcr_aliquot_uuid")
          
 .as[Recordde748c7cae9640b5aaccbd4aff918af0]
 
val x104 = IBag_copynumber__D.select("cn_aliquot_uuid", "cn_gene_id", "cn_copy_number")
          
 .as[Recordaeb7cd807c4e45a182121d648337c810]
 
val x107 = x102.equiJoin(x104, 
 Seq("bcr_aliquot_uuid"), Seq("cn_aliquot_uuid"), "inner").as[Record1713f2ff91a64246bc9814502dd89ea8]
 
val x109 = x107.select("bcr_patient_uuid", "cn_gene_id", "cn_copy_number")
           .withColumnRenamed("bcr_patient_uuid", "sid")
 .withColumnRenamed("cn_gene_id", "gene")
 .withColumnRenamed("cn_copy_number", "cnum")
.withColumn("cnum", when(col("cnum").isNull, 0).otherwise(col("cnum")))
 .as[Record5bc7e8cb7e264f78a221f275f5588533]
 
val x110 = x109
val MBag_cnvCases1_1 = x110  
//MBag_cnvCases1_1.cache
//MBag_cnvCases1_1.count
val x112 = IBag_occurrences__D.select("oid", "donorId", "transcript_consequences")
          
 .as[Record0710816f4ef146fbb24e8c037a3e5d60]
 
val x114 = x112
            .withColumn("cands1", udfRecord63771ce4f3864bad86d0b535f343d644(col("donorId"), col("transcript_consequences")))
 .withColumnRenamed("donorId", "sid1")
 .as[Recordb297f49e08a64be590cfe5653ec87c16]
 
val x115 = x114
val MBag_hybridScore1_1 = x115  
//MBag_hybridScore1_1.cache
//MBag_hybridScore1_1.count
val x117 = MBag_hybridScore1_1.select("cands1")
          
 .as[Record2889c970379347c18c1e42c11899e5db]
 
val x119 = x117
           .withColumnRenamed("cands1", "_LABEL")
 .as[Record3915ac8058cd4edba64820717337ad98]
 
val x120 = x119.distinct 
val x121 = x120
val Dom_cands1_1 = x121  
//Dom_cands1_1.cache
//Dom_cands1_1.count
val x123 = Dom_cands1_1
          
 
 
val x125 = IDict_occurrences__D_transcript_consequences.filter(col("sift_score") > 0.0)
 
val x127 = x125.select("impact", "gene_id", "_1")
          
 .as[Record970aa8e5d6924d3bbc2c96c104b315e9]
 
val x130 = x123.withColumn("o__F_transcript_consequences_LABEL", col("_LABEL").getField("o__F_transcript_consequences"))
   .as[Record3915ac8058cd4edba64820717337ad98].equiJoin(
   x127.withColumnRenamed("_1", "o__F_transcript_consequences_1").as[Record3592b6f24f9640e986358a42c4a8f7a0], 
   Seq("o__F_transcript_consequences_LABEL"), Seq("o__F_transcript_consequences_1"), "inner").drop("o__F_transcript_consequences_LABEL", "o__F_transcript_consequences_1")
   .as[Recordcdb26785726c4f8680cd65a5f15b7257]
 
val x132 = MBag_cnvCases1_1
          
 
 
val x135 = x130.join(x132, col("gene_id") === col("gene") && col("_LABEL").getField("o__F_donorId") === col("sid"), "inner")
  .as[Record7e58693d2adb41b9aa2d4e3cc2ac1f70]
 
val x137 = x135.select("gene_id", "cnum", "impact", "_LABEL")
            .withColumn("score1", ((col("cnum") + 0.01) * when(col("impact") === "HIGH", 0.8).otherwise(when(col("impact") === "MODERATE", 0.5).otherwise(when(col("impact") === "LOW", 0.3).otherwise(0.01)))))
.withColumn("score1", when(col("score1").isNull, 0.0).otherwise(col("score1")))
 .withColumnRenamed("gene_id", "gene1")
 .withColumnRenamed("_LABEL", "_1")
 .as[Recorda0262d840ff0461eb7533482ba5330b7]
 
val x139 = x137.groupByKey(x138 => Recordefaf180c70f8465ba22cccfdb52095d9(x138._1, x138.gene1))
 .agg(typed.sum[Recorda0262d840ff0461eb7533482ba5330b7](x138 => x138.score1)
).mapPartitions{ it => it.map{ case (key, score1) =>
   Recorddae95679c1fc44bb98248e02a38ef9ff(key.gene1, key._1, score1)
}}.as[Recorddae95679c1fc44bb98248e02a38ef9ff]
 
val x140 = x139
val MDict_hybridScore1_1_cands1_1 = x140.repartition($"_1")
//MDict_hybridScore1_1_cands1_1.count




var end0 = System.currentTimeMillis() - start0
println("ExampleTest,standard,"+sf+","+end0+",query,"+spark.sparkContext.applicationId)
    
}
var start = System.currentTimeMillis()
f
var end = System.currentTimeMillis() - start
    
   println("ExampleTest,standard,"+sf+","+end+",total,"+spark.sparkContext.applicationId)
 }
}
