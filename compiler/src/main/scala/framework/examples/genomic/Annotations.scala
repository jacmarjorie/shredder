package framework.examples.genomic

import framework.common._
import framework.examples.Query

/** Commented out String Type represent some 
  * continous assignment for a Sequence Ontology impact factor
  * This is a scale from HIGH to MODIFIER (.99 - .01)
  */ 
trait Vep {
  
  val element = TupleType("element" -> StringType)

  val transcriptFull2 = TupleType(
    "amino_acids" -> StringType,
    "cdna_end" -> LongType,
    "cdna_start" -> LongType,
    "cds_end" -> LongType,
    "cds_start" -> LongType,
    "codons" -> StringType,
    "consequence_terms" -> BagType(element),
    "distance" -> LongType,
    "exon" -> StringType,
    "flags" -> BagType(element),
    "gene_id" -> StringType,
    "impact" -> StringType,
    "intron" -> StringType,
    "polyphen_prediction" -> StringType,
    "polyphen_score" -> DoubleType,
    "protein_end" -> LongType,
    "protein_start" -> LongType,
    "sift_prediction" -> StringType,
    "sift_score" -> DoubleType,
    "ts_strand" -> LongType,
    "transcript_id" -> StringType,
    "variant_allele" -> StringType
  )
 

  val vep_type_full = TupleType(
  	"vid" -> StringType, 
	"allele_string" -> StringType, 
	"assembly_name" -> StringType,
  	"end" -> LongType, 
	"id" -> StringType, 
	"input" -> StringType, 
	"most_severe_consequence" -> StringType, 
	"seq_region_name" -> StringType,
  	"start" -> LongType, 
	"strand" -> LongType, 
	"transcript_consequences" -> BagType(transcriptFull2))
  
  
  //val element = TupleType("element" -> DoubleType)
  
  val intergenic = TupleType("consequence_terms" -> BagType(element),
    // "impact" -> StringType, 
    "impact" -> DoubleType,
    "variant_allele" -> StringType)
  
  val motif = TupleType("bp_overlap" -> LongType,
    "consequence_terms" -> BagType(element),
    // "impact" -> StringType, 
    "impact" -> DoubleType,
    "variant_allele" -> StringType,
    "motif_feature_id" -> StringType, "percentage_overlap" -> DoubleType)
  
  val regulatory = TupleType("bp_overlap" -> LongType,
    "consequence_terms" -> BagType(element),
    // "impact" -> StringType, 
    "impact" -> DoubleType,
    "variant_allele" -> StringType,
    "regulatory_feature_id" -> StringType, "percentage_overlap" -> DoubleType)
  
  val transcript = TupleType(
    "appris" -> StringType,
    // "biotype" -> StringType,
    "biotype" -> DoubleType,
    "bp_overlap" -> LongType,
    "canonical" -> LongType,
    "ccds" -> StringType,
    "cdna_end" -> LongType,
    "cdna_start" -> LongType,
    "cds_end" -> LongType,
    "cds_start" -> LongType,
    "consequence_terms" -> BagType(element),
    "distance" -> LongType,
    "domains" -> BagType(TupleType("db" -> StringType, "name" -> StringType)),
    "exon" -> StringType,
    "gene_id" -> StringType,
    "gene_symbol" -> StringType,
    "gene_symbol_source" -> StringType,
    "hgnc_id" -> StringType,
    // "impact" -> StringType,
    "impact" -> DoubleType,
    "intron" -> StringType,
    "mane" -> StringType,
    "percentage_overlap" -> DoubleType,
    "protein_end" -> LongType,
    "protein_id" -> StringType,
    "protein_start" -> LongType,
    "strand" -> LongType,
    "swissprot" -> BagType(element),
    "transcript_id" -> StringType,
    "tsl" -> LongType,
    "uniparc" -> BagType(element),
    "variant_allele" -> StringType)

  val transcriptQuant = TupleType(
	"aliquot_id" -> StringType,
	"amino_acids" -> StringType,
    "distance" -> LongType,
    "cdna_end" -> LongType,
    "cdna_start" -> LongType,
    "cds_end" -> LongType,
    "cds_start" -> LongType,
    "codons" -> StringType,
    "consequence_terms" -> BagType(element),
    "flags" -> BagType(element),
    "gene_id" -> StringType,
    "impact" -> StringType,
    "protein_end" -> LongType,
    "protein_start" -> LongType,
    "ts_strand" -> LongType,
    "transcript_id" -> StringType,
    //"appris" -> StringType,
    //"biotype" -> DoubleType,
    //"bp_overlap" -> LongType,
    //"canonical" -> LongType,
    //"ccds" -> StringType,
    //"domains" -> BagType(TupleType("db" -> StringType, "name" -> StringType)),
    //"exon" -> StringType,
    //"gene_symbol" -> StringType,
    //"gene_symbol_source" -> StringType,
    //"hgnc_id" -> StringType,
    // "impact" -> StringType,
    //"intron" -> StringType,
    //"mane" -> StringType,
    //"percentage_overlap" -> DoubleType,
    //"protein_id" -> OptionType(StringType),
    //"swissprot" -> BagType(element),
    //"tsl" -> LongType,
    //"uniparc" -> BagType(element),
    "variant_allele" -> StringType)

  val transcriptFull = TupleType(
    "case_id" -> StringType,
    "amino_acids" -> StringType,
    "cdna_end" -> LongType,
    "cdna_start" -> LongType,
    "cds_end" -> LongType,
    "cds_start" -> LongType,
    "codons" -> StringType,
    "consequence_terms" -> BagType(element),
    "distance" -> LongType,
    "exon" -> StringType,
    "flags" -> BagType(element),
    "gene_id" -> StringType,
    "impact" -> StringType,
    "intron" -> StringType,
    "polyphen_prediction" -> StringType,
    "polyphen_score" -> DoubleType,
    "protein_end" -> LongType,
    "protein_start" -> LongType,
    "sift_prediction" -> StringType,
    "sift_score" -> DoubleType,
    "ts_strand" -> LongType,
    "transcript_id" -> StringType,
    "variant_allele" -> StringType
  )


  val vep_type = TupleType("allele_string" -> StringType, 
    "assembly_name" -> StringType, "end" -> LongType, "id" -> StringType,
    "input" -> StringType, "intergenic_consequences" -> BagType(intergenic),
    "most_severe_consequence" -> StringType,
    "motif_feature_consequences" -> BagType(motif),
    "regulatory_feature_consequences" -> BagType(regulatory),
    "seq_region_name" -> StringType,
    "start" -> LongType, 
    "transcript_consequences" -> BagType(transcript),
    "variant_class" -> StringType)

  val vcf_vep_type = TupleType(
    "id" -> StringType, "contig" -> StringType, "start" -> IntType,
    "reference" -> StringType, "genotypes" -> 
      BagType(TupleType("g_sample" -> StringType, "call" -> IntType)),
    "allele_string" -> StringType, 
    "assembly_name" -> StringType, "end" -> LongType, "id" -> StringType,
    "input" -> StringType, "intergenic_consequences" -> BagType(intergenic),
    // "most_severe_consequence" -> StringType,
    "most_severe_consequence" -> DoubleType,   
    "motif_feature_consequences" -> BagType(motif),
    "regulatory_feature_consequences" -> BagType(regulatory),
    "seq_region_name" -> StringType,
    "start" -> LongType, 
    "transcript_consequences" -> BagType(transcript),
    "variant_class" -> StringType
  )



}
