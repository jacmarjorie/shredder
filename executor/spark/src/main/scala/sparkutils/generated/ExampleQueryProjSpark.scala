
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
case class Record3e0772e40dee46b08cce186311f6bccc(name: String)
case class Record5694ecc9bc674f30b10c4c6781274929(p_name: String, url: String, gene_set: Seq[Record3e0772e40dee46b08cce186311f6bccc], pathways_index: Long)
case class Recorde308dae1543141eeaefe601ee9e47369(p_name: String, gene_set: Seq[Record3e0772e40dee46b08cce186311f6bccc], pathways_index: Long)
case class Recordd4f660b9a3ed46cb8b80198714d78499(element: String)
case class Record57f2209755a74014b52a66069d3eec65(polyphen_score: Double, sift_prediction: String, polyphen_prediction: String, flags: Seq[String], protein_end: Long, ts_strand: Long, impact: String, gene_id: String, amino_acids: String, sift_score: Double, cdna_start: Long, transcript_id: String, cdna_end: Long, exon: String, cds_end: Long, cds_start: Long, intron: String, consequence_terms: Seq[String], case_id: String, protein_start: Long, variant_allele: String, codons: String, distance: Long)
case class Record964bdf25b9244b17b777586bbb031953(vid: String, vend: Long, Tumor_Seq_Allele1: String, allele_string: String, Reference_Allele: String, oid: String, vstart: Long, projectId: String, donorId: String, seq_region_name: String, transcript_consequences: Seq[Record57f2209755a74014b52a66069d3eec65], chromosome: String, occurrences_index: Long, assembly_name: String, end: Long, Tumor_Seq_Allele2: String, strand: Long, start: Long, input: String, most_severe_consequence: String)
case class Record7241e3ab9b64487eb5a9e55dd01720dc(donorId: String, transcript_consequences: Seq[Record57f2209755a74014b52a66069d3eec65])
case class Recordfbba47cdd038463e85f22ad079852c95(p_name: String, donorId: Option[String], transcript_consequences: Option[Seq[Record57f2209755a74014b52a66069d3eec65]], pathways_index: Long, gene_set: Seq[Record3e0772e40dee46b08cce186311f6bccc])
case class Record8e90d4fd0e054e0693e417c07a786777(p_name: String, transcript_consequences_index: Long, donorId: Option[String], transcript_consequences: Option[Seq[Record57f2209755a74014b52a66069d3eec65]], pathways_index: Long, gene_set: Seq[Record3e0772e40dee46b08cce186311f6bccc])
case class Record359a2e87f7194690805d45d91b8ff996(p_name: String, impact: Option[String], donorId: Option[String], pathways_index: Long, gene_set: Seq[Record3e0772e40dee46b08cce186311f6bccc])
case class Record7d7ac56c24de4abb8f0dfd326bb42260(p_name: String, gene_set_index: Long, impact: Option[String], donorId: Option[String], pathways_index: Long, gene_set: Seq[Record3e0772e40dee46b08cce186311f6bccc])
case class Record435128ad7b6b495f9e80aaf273aef74c(p_name: String, impact: Option[String], donorId: Option[String], pathways_index: Long)
case class Record02845a3a271347ae8358ce5e1bdb5069(p_name: String, burden: Double, pathways_index: Long, sid: Option[String])
case class Record7603eed5978840f9b80726a9719ece80(p_name: String, pathways_index: Long, sid: Option[String], burden: Double)
case class Record955f01b6d7a34887a45c4bf9c0cd49b1(p_name: String, pathways_index: Long, sid: Option[String])
case class Record2808bb9e30644d5a875f3d05ac335cc5(p_name: String, pathways_index: Long)
case class Record251229a6347347358306c4925fa601eb(sid: String, burden: Double)
case class Record66a0d313d1f247768c4f15e04289e154(p_name: String, pathways_index: Long, burdens: Seq[Record251229a6347347358306c4925fa601eb])
case class Recorddf3fc23f0fd64d53bd14ed6c5c8e9184(pathway: String, burdens: Seq[Record251229a6347347358306c4925fa601eb])
object ExampleQueryProjSpark {
 def main(args: Array[String]){
   val sf = Config.datapath.split("/").last
   val conf = new SparkConf()
     .setAppName("ExampleQueryProjSpark"+sf)
     .set("spark.sql.shuffle.partitions", Config.maxPartitions.toString)
   val spark = SparkSession.builder().config(conf).getOrCreate()
   
   import spark.implicits._
   val sloader = new BiospecLoader(spark)
val samples = sloader.load("/mnt/app_hdd/data/biospecimen/aliquot/nationwidechildrens.org_biospecimen_aliquot_dlbc.txt")
val cloader = new CopyNumberLoader(spark)
val copynumber = cloader.load("/mnt/app_hdd/data/cnv", true)

val geLoader = new GeneExpressionLoader(spark)
val expression = geLoader.load("/mnt/app_hdd/data/expression", true, aliquot_file = "/mnt/app_hdd/data/fpkm_uq_case_aliquot.txt")

val occurrences = spark.read.json("/mnt/app_hdd/data/somatic/datasetPRAD")
val pathways =

   def f = { 
 
 val x17 = pathways.withColumn("pathways_index", monotonically_increasing_id())
 .as[Record5694ecc9bc674f30b10c4c6781274929]
 
val x20 = x17.select("p_name", "gene_set", "pathways_index")
          
 .as[Recorde308dae1543141eeaefe601ee9e47369]
 
val x21 = occurrences.withColumn("occurrences_index", monotonically_increasing_id())
 .as[Record964bdf25b9244b17b777586bbb031953]
 
val x24 = x21.select("donorId", "transcript_consequences")
          
 .as[Record7241e3ab9b64487eb5a9e55dd01720dc]
 
val x27 = x20.crossJoin(x24)
  .as[Recordfbba47cdd038463e85f22ad079852c95]
 
val x28 = x27.withColumn("transcript_consequences_index", monotonically_increasing_id())
 .as[Record8e90d4fd0e054e0693e417c07a786777]
 
val x32 = x28.flatMap{
 case x29 => 
      x29.transcript_consequences match {
     case Some(transcript_consequences) if transcript_consequences.nonEmpty => 
       transcript_consequences.map( x31 => Record359a2e87f7194690805d45d91b8ff996(x29.p_name, Some(x31.impact), x29.donorId, x29.pathways_index, x29.gene_set) )
     case _ => Seq(Record359a2e87f7194690805d45d91b8ff996(x29.p_name, None, x29.donorId, x29.pathways_index, x29.gene_set))
   }

}.as[Record359a2e87f7194690805d45d91b8ff996]
 
val x33 = x32.withColumn("gene_set_index", monotonically_increasing_id())
 .as[Record7d7ac56c24de4abb8f0dfd326bb42260]
 
val x36 = x33.flatMap{
 case x34 => 
      if (x34.gene_set.isEmpty) Seq(Record435128ad7b6b495f9e80aaf273aef74c(x34.p_name, x34.impact, x34.donorId, x34.pathways_index))
   else x34.gene_set.flatMap( x35 => if (x35.name == x35.gene_id) Seq(Record435128ad7b6b495f9e80aaf273aef74c(x34.p_name, x34.impact, x34.donorId, x34.pathways_index)) else Seq() )

}.as[Record435128ad7b6b495f9e80aaf273aef74c]
 
val x38 = x36
            .withColumn("burden", when(col("impact") === "HIGH", 0.8).otherwise(when(col("impact") === "MODERATE", 0.5).otherwise(when(col("impact") === "LOW", 0.3).otherwise(0.01))))
.withColumn("burden", when(col("burden").isNull, 0.0).otherwise(col("burden")))
 .withColumnRenamed("donorId", "sid")
 .as[Record02845a3a271347ae8358ce5e1bdb5069]
 
val x40 = x38.groupByKey(x39 => Record955f01b6d7a34887a45c4bf9c0cd49b1(x39.p_name, x39.pathways_index, x39.sid))
 .agg(typed.sum[Record02845a3a271347ae8358ce5e1bdb5069](x39 => x39.burden)
).mapPartitions{ it => it.map{ case (key, burden) =>
   Record7603eed5978840f9b80726a9719ece80(key.p_name, key.pathways_index, key.sid, burden)
}}.as[Record7603eed5978840f9b80726a9719ece80]
 
val x42 = x40.groupByKey(x41 => Record2808bb9e30644d5a875f3d05ac335cc5(x41.p_name, x41.pathways_index)).mapGroups{
 case (key, value) => 
   val grp = value.flatMap(x41 => 
    (x41.sid) match {
      case (None) => Seq()
      case _ => Seq(Record251229a6347347358306c4925fa601eb(x41.sid match { case Some(x) => x; case _ => "null" }, x41.burden))
   }).toSeq
   Record66a0d313d1f247768c4f15e04289e154(key.p_name, key.pathways_index, grp)
 }.as[Record66a0d313d1f247768c4f15e04289e154]
 
val x44 = x42.select("p_name", "burdens")
           .withColumnRenamed("p_name", "pathway")
 .as[Recorddf3fc23f0fd64d53bd14ed6c5c8e9184]
 
val x45 = x44
val GMB = x45
//GMB.count

}
var start = System.currentTimeMillis()
f
var end = System.currentTimeMillis() - start 
   println("ExampleTest,standard,"+sf+","+end+",total,"+spark.sparkContext.applicationId)
 }
}
