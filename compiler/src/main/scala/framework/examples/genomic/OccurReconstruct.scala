package framework.examples.genomic

import framework.common._
import framework.examples.Query

/** This file contains the reconstruction queries 
  * for the occurrences endpoint. 
  * This query would be a common example of something coming from 
  * a user-interface (lacking projections, various levels of grouping)
  */

// Group occurrences (100K) endpoint by sample
// no projections, nested join (converts to broadcast)
object OccurGroupByCase extends DriverGene {
  val name = "OccurGroupByCase"
  val query = ForeachUnion(br, biospec,
    Singleton(Tuple("o_case_id" -> br("bcr_patient_uuid"), "o_mutations" ->
      ForeachUnion(or, occurrences, 
        IfThenElse(Cmp(OpEq, or("donorId"), br("bcr_patient_uuid")),
          projectTuple(or, Map("aliquotId" -> br("bcr_aliquot_uuid"), 
            "o_trans_conseq" -> ForeachUnion(ar, BagProject(or, "transcript_consequences"),
            projectTuple(ar, Map("o_conseq_terms" -> 
              ForeachUnion(cr, BagProject(ar, "consequence_terms"),
                            ForeachUnion(conr, conseq,
                              IfThenElse(Cmp(OpEq, conr("so_term"), cr("element")),
                                Singleton(Tuple("conseq_term" -> conr("so_term"), "conseq_weight" -> conr("so_weight"))))))),
             List("consequence_terms", "flags"))
          )), List("transcript_consequences"))
      )))))
  val program = Program(Assignment(name, query))
}

// Group occurrences (100K) endpoint by sample
// projections on the cnv table, two nested joins
// (one converts to broadcast)
object OccurCNVGroupByCase extends DriverGene {
  val name = "OccurCNVGroupByCase"

  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    s"${super.loadTables(shred, skew)}\n${loadCopyNumber(shred, skew)}"

  val query = ForeachUnion(br, biospec,
    Singleton(Tuple("o_case_id" -> br("bcr_patient_uuid"), "o_mutations" ->
      ForeachUnion(or, occurrences, 
        IfThenElse(Cmp(OpEq, or("donorId"), br("bcr_patient_uuid")),
          projectTuple(or, Map(//"aliquotId" -> br("bcr_aliquot_uuid"), 
            "o_trans_conseq" -> ForeachUnion(ar, BagProject(or, "transcript_consequences"), 
              ForeachUnion(cnr, copynum,
                    IfThenElse(And(Cmp(OpEq, cnr("cn_aliquot_uuid"), ar("aliquot_id")),
                      Cmp(OpEq, ar("gene_id"), cnr("cn_gene_id"))),
            projectTuple(ar, Map(
              "o_copy_number" -> cnr("cn_copy_number"),
              "o_max_copy" -> cnr("max_copy_number"),
              "o_min_copy" -> cnr("min_copy_number"),
              "o_cn_start" -> cnr("cn_start"),
              "o_cn_end" -> cnr("cn_end"),
              "o_cn_chr" -> cnr("cn_chromosome"),
              "o_conseq_terms" -> 
              ForeachUnion(cr, BagProject(ar, "consequence_terms"),
                            ForeachUnion(conr, conseq,
                              IfThenElse(Cmp(OpEq, conr("so_term"), cr("element")),
                                Singleton(Tuple("conseq_term" -> conr("so_term"), "conseq_weight" -> conr("so_weight"))))))),
             List("consequence_terms", "flags"))
          )))), List("transcript_consequences"))
      )))))
  val program = Program(Assignment(name, query))
}

// Group occurrence (100k) by sample, 
// aggregate at the second level (across transcripts)
// two nested joins (one converts to broadcast)
object OccurCNVAggGroupByCase extends DriverGene {

  val name = "OccurCNVAggGroupByCase"

  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
      s"${super.loadTables(shred, skew)}\n${loadCopyNumber(shred, skew)}"

  val avgcn = ((cncr("max_copy_number").asNumeric + cncr("min_copy_number").asNumeric
    + cncr("cn_copy_number").asNumeric + NumericConst(0.01, DoubleType)) / NumericConst(3.0, DoubleType)) 

  val query = ForeachUnion(br, biospec,
    Singleton(Tuple("o_case_id" -> br("bcr_patient_uuid"), 
      "o_aliquot_id" -> br("bcr_aliquot_uuid"),
      "o_center" -> br("center_id"),
      "o_mutations" ->
      ForeachUnion(or, occurmids, 
        IfThenElse(Cmp(OpEq, or("donorId"), br("bcr_patient_uuid")),
          projectTuple(or, Map(//"aliquotId" -> br("bcr_aliquot_uuid"), 
            "o_trans_conseq" -> ReduceByKey(
              ForeachUnion(ar, BagProject(or, "transcript_consequences"),
                ForeachUnion(cncr, cnvCases,
                  IfThenElse(And(Cmp(OpEq, cncr("cn_case_uuid"), ar("aliquot_id")),
                      Cmp(OpEq, ar("gene_id"), cncr("cn_gene_id"))),
                        ForeachUnion(cr, BagProject(ar, "consequence_terms"),
                          ForeachUnion(conr, conseq,
                            IfThenElse(Cmp(OpEq, conr("so_term"), cr("element")),
                              Singleton(Tuple("o_gene_id" -> ar("gene_id"), 
                                "hybrid_score" -> (conr("so_weight").asNumeric * avgcn * matchImpact)
                                )))))))),
            List("o_gene_id"),
            List("hybrid_score"))), List("transcript_consequences")))))))

  val program = Program(Assignment("cnvCases", mapCNV), Assignment(name, query))
  
}

// Group occurrences (500K) endpoint by sample
// no projections, nested join (converts to broadcast)
object OccurGroupByCaseMid extends DriverGene {

  val name = "OccurGroupByCaseMid"

  val query = ForeachUnion(br, biospec,
    Singleton(Tuple("o_case_id" -> br("bcr_patient_uuid"), "o_mutations" ->
      ForeachUnion(omr, occurmids, 
        IfThenElse(Cmp(OpEq, omr("donorId"), br("bcr_patient_uuid")),
          projectTuple(omr, Map(//"aliquotId" -> br("bcr_aliquot_uuid"), 
            "o_trans_conseq" -> ForeachUnion(amr, BagProject(omr, "transcript_consequences"),
            projectTuple(amr, Map("o_conseq_terms" -> 
              ForeachUnion(cr, BagProject(amr, "consequence_terms"),
                            ForeachUnion(conr, conseq,
                              IfThenElse(Cmp(OpEq, conr("so_term"), cr("element")),
                                Singleton(Tuple("conseq_term" -> conr("so_term"), "conseq_weight" -> conr("so_weight"))))))),
             List("consequence_terms", "flags"))
          )), List("transcript_consequences")))))))

  val program = Program(Assignment(name, query))
  
}

// Group occurrences (500K) endpoint by sample
// projections on the cnv table, nested join 
// (one converts to broadcast)
object OccurCNVGroupByCaseMid extends DriverGene {

  val name = "OccurCNVGroupByCaseMid"

  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
      s"${super.loadTables(shred, skew)}\n${loadCopyNumber(shred, skew)}"

  val query = ForeachUnion(br, biospec,
    Singleton(Tuple("o_case_id" -> br("bcr_patient_uuid"), 
      "o_aliquot_id" -> br("bcr_aliquot_uuid"),
      "o_center" -> br("center_id"),
      "o_mutations" ->
      ForeachUnion(omr, occurmids, 
        IfThenElse(Cmp(OpEq, omr("donorId"), br("bcr_patient_uuid")),
          projectTuple(omr, Map(//"aliquotId" -> br("bcr_aliquot_uuid"), 
            "o_trans_conseq" -> ForeachUnion(amr, BagProject(omr, "transcript_consequences"),
              ForeachUnion(cncr, cnvCases,
                    IfThenElse(And(Cmp(OpEq, cncr("cn_case_uuid"), omr("case_id")),
                      Cmp(OpEq, amr("gene_id"), cncr("cn_gene_id"))),
            projectTuple(amr, Map(
              "o_copy_number" -> cncr("cn_copy_number"),
              "o_max_copy" -> cncr("max_copy_number"),
              "o_min_copy" -> cncr("min_copy_number"),
              "o_cn_start" -> cncr("cn_start"),
              "o_cn_end" -> cncr("cn_end"),
              "o_cn_chr" -> cncr("cn_chromosome"),
              "o_conseq_terms" -> 
              ForeachUnion(cr, BagProject(amr, "consequence_terms"),
                            ForeachUnion(conr, conseq,
                              IfThenElse(Cmp(OpEq, conr("so_term"), cr("element")),
                                Singleton(Tuple("conseq_term" -> conr("so_term"), "conseq_weight" -> conr("so_weight"))))))),
             List("consequence_terms", "flags"))
          )))), List("transcript_consequences")))))))

  val program = Program(Assignment("cnvCases", mapCNV), Assignment(name, query))
  
}

// Group occurrence (500k) by sample, 
// aggregate at the second level (across transcripts)
// two nested joins (one converts to broadcast)
object OccurCNVAggGroupByCaseMid extends DriverGene {

  val name = "OccurCNVAggGroupByCaseMid"

  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
      s"${super.loadTables(shred, skew)}\n${loadCopyNumber(shred, skew)}"

  val avgcn = ((cncr("max_copy_number").asNumeric + cncr("min_copy_number").asNumeric
    + cncr("cn_copy_number").asNumeric + NumericConst(0.01, DoubleType)) / NumericConst(3.0, DoubleType)) 

  val query = ForeachUnion(br, biospec,
    Singleton(Tuple("o_case_id" -> br("bcr_patient_uuid"), 
      "o_aliquot_id" -> br("bcr_aliquot_uuid"),
      "o_center" -> br("center_id"),
      "o_mutations" ->
      ForeachUnion(omr, occurmids, 
        IfThenElse(Cmp(OpEq, omr("donorId"), br("bcr_patient_uuid")),
          projectTuple(omr, Map(//"aliquotId" -> br("bcr_aliquot_uuid"), 
            "o_trans_conseq" -> ReduceByKey(
              ForeachUnion(amr, BagProject(omr, "transcript_consequences"),
                ForeachUnion(cncr, cnvCases,
                  IfThenElse(And(Cmp(OpEq, cncr("cn_case_uuid"), omr("case_id")),
                      Cmp(OpEq, amr("gene_id"), cncr("cn_gene_id"))),
                        ForeachUnion(cr, BagProject(amr, "consequence_terms"),
                          ForeachUnion(conr, conseq,
                            IfThenElse(Cmp(OpEq, conr("so_term"), cr("element")),
                              Singleton(Tuple("o_gene_id" -> amr("gene_id"), 
                                "hybrid_score" -> (conr("so_weight").asNumeric * avgcn * matchImpactMid)
                                )))))))),
            List("o_gene_id"),
            List("hybrid_score"))), List("transcript_consequences")))))))

  val program = Program(Assignment("cnvCases", mapCNV), Assignment(name, query))
  
}

object ClinicalRunExample extends DriverGene {

  val name = "ClinicalRunExample"

  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
      s"${loadBiospec(shred, skew)}\n${loadOccurrence(shred, skew)}\n${loadCopyNumber(shred, skew)}"

  val avgcn = ((cncr("max_copy_number").asNumeric + cncr("min_copy_number").asNumeric
    + cncr("cn_copy_number").asNumeric + NumericConst(0.01, DoubleType)) / NumericConst(3.0, DoubleType)) 

  val query = ForeachUnion(br, biospec,
    Singleton(Tuple("sample" -> br("bcr_patient_uuid"), 
      "mutations" ->
      ForeachUnion(omr, occurmids, 
        IfThenElse(Cmp(OpEq, omr("donorId"), br("bcr_patient_uuid")),
          Singleton(Tuple("mutId" -> omr("vid"), 
            "scores" -> ReduceByKey(
              ForeachUnion(amr, BagProject(omr, "transcript_consequences"),
                ForeachUnion(cncr, cnvCases,
                  IfThenElse(And(Cmp(OpEq, cncr("cn_case_uuid"), omr("donorId")),
                      Cmp(OpEq, amr("gene_id"), cncr("cn_gene_id"))),
                    Singleton(Tuple("gene" -> amr("gene_id"), 
                                "score" -> matchImpactMid * (cncr("cn_copy_number").asNumeric + NumericConst(0.01, DoubleType))
                                  * amr("sift_score").asNumeric * amr("polyphen_score").asNumeric))))),
            List("gene"),
            List("score")))))))))

  val program = Program(Assignment("cnvCases", mapCNV), Assignment(name, query))
  
}
