package framework.examples.genomic

import framework.common._
import framework.examples.Query
import framework.nrc.Parser

class LetTest0(override val letOpt: Boolean = false) extends DriverGene {

  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    if (shred){
      s"""|val samples = spark.table("samples")
          |val IBag_samples__D = samples
          |
          |val copynumber = spark.table("copynumber")
          |val IBag_copynumber__D = copynumber
          |
          |val odict1 = spark.table("odict1")
          |val IBag_occurrences__D = odict1
          |
          |// issue with partial shredding here
          |val odict2 = spark.table("odict2").drop("flags")
          |val IDict_occurrences__D_transcript_consequences = odict2
          |
          |val odict3 = spark.table("odict3")
          |val IDict_occurrences__D_transcript_consequences_consequence_terms = odict3
          |""".stripMargin
    }else{
      s"""|val samples = spark.table("samples")
          |
          |val copynumber = spark.table("copynumber")
          |
          |val occurrences = spark.table("occurrences")
          |""".stripMargin
    }

  val name = "LetTest0"
  
  val tbls = Map("occurrences" -> occurmids.tp, 
                  "copynumber" -> copynum.tp, 
                  "samples" -> samples.tp)

    val imp = """if (t.impact = "HIGH") then 0.80 
                  else if (t.impact = "MODERATE") then 0.50
                  else if (t.impact = "LOW") then 0.30
                  else 0.01"""

    // val query = 
    // 	s"""
    //     initScores <= 
    //    let cnvCases := 
	  //       for s in samples union 
	  //         for c in copynumber union 
	  //           if (s.bcr_aliquot_uuid = c.cn_aliquot_uuid)
	  //           then {(sid := s.bcr_patient_uuid, gene := c.cn_gene_id, cnum := c.cn_copy_number)}
	  //   in 
    // 			(for o in occurrences union 
    // 				for t in o.transcript_consequences union 
    // 					for c in cnvCases union 
    // 						if (o.donorId = c.sid && t.gene_id = c.gene)
    // 						then {(hybrid_case := o.donorId, hybrid_gene := t.gene_id, hybrid_score := $imp * (c.cnum + 0.01))}).sumBy({hybrid_case, hybrid_gene}, {hybrid_score});
    	
    //     LetTest0 <= 
    // 		for s in samples union 
    // 			{( hybrid_sample := s.bcr_patient_uuid,
    // 			   hybrid_aliquot := s.bcr_aliquot_uuid,
    // 			   hybrid_center := s.center_id,
    // 			   hybrid_genes := for i in initScores union
    // 			   	if (s.bcr_patient_uuid = i.hybrid_case)
    // 			   	then {(hybrid_gene := i.hybrid_gene, hybrid_score := i.hybrid_gene)})}
    // 	"""

    val cnvs = 
      s"""
      for s in samples union 
        for c1 in copynumber union 
          if (s.bcr_aliquot_uuid = c1.cn_aliquot_uuid)
          then {(sid := s.bcr_patient_uuid, gene := c1.cn_gene_id, cnum := c1.cn_copy_number)}
      """
    val initScores = 
      s"""
         (for o in occurrences union 
           for t in o.transcript_consequences union 
             for c in cnvCases union 
               if (o.donorId = c.sid && t.gene_id = c.gene)
               then {(hybrid_case := o.donorId, hybrid_gene := t.gene_id, hybrid_score := (c.cnum + 0.01) * $imp )}).sumBy({hybrid_case, hybrid_gene}, {hybrid_score})
      """

      val lettest = 
        s"""
         for s in samples union 
           {( hybrid_sample := s.bcr_patient_uuid,
              hybrid_aliquot := s.bcr_aliquot_uuid,
              hybrid_center := s.center_id,
              hybrid_genes := for i in initScores0 union
               if (s.bcr_patient_uuid = i.hybrid_case)
               then {(hybrid_gene := i.hybrid_gene, hybrid_score := i.hybrid_score)})}
       """

    val query =
      s"""
        LetTest0 <= 
          let cnvCases := $cnvs
          in let initScores0 := $initScores 
          in $lettest
      """

    val parser = Parser(tbls)
    val program = parser.parse(query).get.asInstanceOf[Program]
    // val bagexpr = parser.parse(query, parser.term).get.asInstanceOf[BagExpr]
    // val program = Program(Assignment(name, bagexpr))

}

object LetTest1 extends DriverGene {

  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    if (shred){
      s"""|val samples = spark.table("samples")
          |val IBag_samples__D = samples
          |
          |val copynumber = spark.table("copynumber")
          |val IBag_copynumber__D = copynumber
          |
          |val odict1 = spark.table("odict1")
          |val IBag_occurrences__D = odict1
          |
          |// issue with partial shredding here
          |val odict2 = spark.table("odict2").drop("flags")
          |val IDict_occurrences__D_transcript_consequences = odict2
          |
          |val odict3 = spark.table("odict3")
          |val IDict_occurrences__D_transcript_consequences_consequence_terms = odict3
          |""".stripMargin
    }else{
      s"""|val samples = spark.table("samples")
          |
          |val copynumber = spark.table("copynumber")
          |
          |val occurrences = spark.table("occurrences")
          |""".stripMargin
    }

  val name = "LetTest1"
  
  val tbls = Map("occurrences" -> occurmids.tp, 
                  "copynumber" -> copynum.tp, 
                  "samples" -> samples.tp)

    val imp = """if (t.impact = "HIGH") then 0.80 
                  else if (t.impact = "MODERATE") then 0.50
                  else if (t.impact = "LOW") then 0.30
                  else 0.01"""

    val query = 
     s"""
        cnvCases <=
            for s in samples union 
              for c in copynumber union 
                if (s.bcr_aliquot_uuid = c.cn_aliquot_uuid)
                then {(sid := s.bcr_patient_uuid, gene := c.cn_gene_id, cnum := c.cn_copy_number)};

        initScores1 <=  
         (for o in occurrences union 
           for t in o.transcript_consequences union 
             for c in cnvCases union 
               if (o.donorId = c.sid && t.gene_id = c.gene)
               then {(hybrid_case := o.donorId, hybrid_gene := t.gene_id, hybrid_score := (c.cnum + 0.01) * $imp )}).sumBy({hybrid_case, hybrid_gene}, {hybrid_score});
      
        LetTest1 <= 
         for s in samples union 
           {( hybrid_sample := s.bcr_patient_uuid,
              hybrid_aliquot := s.bcr_aliquot_uuid,
              hybrid_center := s.center_id,
              hybrid_genes := for i in initScores1 union
               if (s.bcr_patient_uuid = i.hybrid_case)
               then {(hybrid_gene := i.hybrid_gene, hybrid_score := i.hybrid_score)})}
     """

    val parser = Parser(tbls)
    val program = parser.parse(query).get.asInstanceOf[Program]
    // val bagexpr = parser.parse(query, parser.term).get.asInstanceOf[BagExpr]
    // val program = Program(Assignment(name, bagexpr))

}

object LetTest2 extends DriverGene {

  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    if (shred){
      s"""|val samples = spark.table("samples")
          |val IBag_samples__D = samples
          |
          |val copynumber = spark.table("copynumber")
          |val IBag_copynumber__D = copynumber
          |
          |val odict1 = spark.table("odict1")
          |val IBag_occurrences__D = odict1
          |
          |// issue with partial shredding here
          |val odict2 = spark.table("odict2").drop("flags")
          |val IDict_occurrences__D_transcript_consequences = odict2
          |
          |val odict3 = spark.table("odict3")
          |val IDict_occurrences__D_transcript_consequences_consequence_terms = odict3
          |""".stripMargin
    }else{
      s"""|val samples = spark.table("samples")
          |
          |val copynumber = spark.table("copynumber")
          |
          |val occurrences = spark.table("occurrences")
          |""".stripMargin
    }

  val name = "LetTest2"
  
  val tbls = Map("occurrences" -> occurmids.tp, 
                  "copynumber" -> copynum.tp, 
                  "samples" -> samples.tp)

    val imp = """if (t.impact = "HIGH") then 0.80 
                  else if (t.impact = "MODERATE") then 0.50
                  else if (t.impact = "LOW") then 0.30
                  else 0.01"""

    val cnvs = 
      s"""
      for s in samples union 
        for c1 in copynumber union 
          if (s.bcr_aliquot_uuid = c1.cn_aliquot_uuid)
          then {(sid := s.bcr_patient_uuid, gene := c1.cn_gene_id, cnum := c1.cn_copy_number)}
      """
    val initScores = 
      s"""
         (for o in occurrences union 
           for t in o.transcript_consequences union 
             for c in $cnvs union 
               if (o.donorId = c.sid && t.gene_id = c.gene)
               then {(hybrid_case := o.donorId, hybrid_gene := t.gene_id, hybrid_score := (c.cnum + 0.01) * $imp )}).sumBy({hybrid_case, hybrid_gene}, {hybrid_score})
      """
    val query = 
     s"""
      LetTest2 <= 
       for s in samples union 
         {( hybrid_sample := s.bcr_patient_uuid,
            hybrid_aliquot := s.bcr_aliquot_uuid,
            hybrid_center := s.center_id,
            hybrid_genes := for i in $initScores union
             if (s.bcr_patient_uuid = i.hybrid_case)
             then {(hybrid_gene := i.hybrid_gene, hybrid_score := i.hybrid_score)}
         )}
     """

    val parser = Parser(tbls)
    val program = parser.parse(query).get.asInstanceOf[Program]
}

object LetTest3 extends DriverGene {

  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    if (shred){
      s"""|val samples = spark.table("samples")
          |val IBag_samples__D = samples
          |
          |val copynumber = spark.table("copynumber")
          |val IBag_copynumber__D = copynumber
          |
          |val odict1 = spark.table("odict1")
          |val IBag_occurrences__D = odict1
          |
          |// issue with partial shredding here
          |val odict2 = spark.table("odict2").drop("flags")
          |val IDict_occurrences__D_transcript_consequences = odict2
          |
          |val odict3 = spark.table("odict3")
          |val IDict_occurrences__D_transcript_consequences_consequence_terms = odict3
          |""".stripMargin
    }else{
      s"""|val samples = spark.table("samples")
          |
          |val copynumber = spark.table("copynumber")
          |
          |val occurrences = spark.table("occurrences")
          |""".stripMargin
    }

  val name = "LetTest3"
  
  val tbls = Map("occurrences" -> occurmids.tp, 
                  "copynumber" -> copynum.tp, 
                  "samples" -> samples.tp)

    val imp = """if (t.impact = "HIGH") then 0.80 
                  else if (t.impact = "MODERATE") then 0.50
                  else if (t.impact = "LOW") then 0.30
                  else 0.01"""

    val query = 
     s"""
        cnvCases <=
            for s in samples union 
              for c in copynumber union 
                if (s.bcr_aliquot_uuid = c.cn_aliquot_uuid)
                then {(sid := s.bcr_patient_uuid, gene := c.cn_gene_id, cnum := c.cn_copy_number)};

        initScores1 <=  
         (for o in occurrences union 
           for t in o.transcript_consequences union 
             for c in cnvCases union 
               if (o.donorId = c.sid && t.gene_id = c.gene)
               then {(hybrid_case := o.donorId, hybrid_gene := t.gene_id, hybrid_score := (c.cnum + 0.01) * $imp )}).sumBy({hybrid_case, hybrid_gene}, {hybrid_score});
        
        intermMatrix <= 
          for s in samples union 
            for i in initScores1 union
              if (s.bcr_patient_uuid = i.hybrid_case)
              then {(hybrid_sample := s.bcr_aliquot_uuid, hybrid_gene := i.hybrid_gene, hybrid_score := i.hybrid_score)}

     """      
     //    intermMatrix <= 
     //     for s in samples union 
     //       {( hybrid_sample := s.bcr_patient_uuid,
     //          hybrid_aliquot := s.bcr_aliquot_uuid,
     //          hybrid_center := s.center_id,
     //          hybrid_genes := for i in initScores1 union
     //           if (s.bcr_patient_uuid = i.hybrid_case)
     //           then {(hybrid_gene := i.hybrid_gene, hybrid_score := i.hybrid_score)})};

     //    finalMatrix <= 
     //      (for s in intermMatrix union 
     //        for h in intermMatrix.hybrid_genes union 
     //          {(hybrid_gene := h.hybrid_gene, hybrid_score := h.hybrid_score)}).sumBy({hybrid_gene}, {hybrid_score})

     // """

    val parser = Parser(tbls)
    val program = parser.parse(query).get.asInstanceOf[Program]
    // val bagexpr = parser.parse(query, parser.term).get.asInstanceOf[BagExpr]
    // val program = Program(Assignment(name, bagexpr))

}

object LetTest4 extends DriverGene {

  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    if (shred){
      s"""|val samples = spark.table("samples")
          |val IBag_samples__D = samples
          |
          |val copynumber = spark.table("copynumber")
          |val IBag_copynumber__D = copynumber
          |
          |val odict1 = spark.table("odict1")
          |val IBag_occurrences__D = odict1
          |
          |// issue with partial shredding here
          |val odict2 = spark.table("odict2").drop("flags")
          |val IDict_occurrences__D_transcript_consequences = odict2
          |
          |val odict3 = spark.table("odict3")
          |val IDict_occurrences__D_transcript_consequences_consequence_terms = odict3
          |""".stripMargin
    }else{
      s"""|val samples = spark.table("samples")
          |
          |val copynumber = spark.table("copynumber")
          |
          |val occurrences = spark.table("occurrences")
          |""".stripMargin
    }

  val name = "LetTest4"
  
  val tbls = Map("occurrences" -> occurmids.tp, 
                  "copynumber" -> copynum.tp, 
                  "samples" -> samples.tp)

    val imp = """if (t.impact = "HIGH") then 0.80 
                  else if (t.impact = "MODERATE") then 0.50
                  else if (t.impact = "LOW") then 0.30
                  else 0.01"""

    val cnvs = 
      s"""
      for s in samples union 
        for c1 in copynumber union 
          if (s.bcr_aliquot_uuid = c1.cn_aliquot_uuid)
          then {(sid := s.bcr_patient_uuid, gene := c1.cn_gene_id, cnum := c1.cn_copy_number)}
      """
    val initScores = 
      s"""
         (for o in occurrences union 
           for t in o.transcript_consequences union 
             for c in $cnvs union 
               if (o.donorId = c.sid && t.gene_id = c.gene)
               then {(hybrid_case := o.donorId, hybrid_gene := t.gene_id, hybrid_score := (c.cnum + 0.01) * $imp )}).sumBy({hybrid_case, hybrid_gene}, {hybrid_score})
      """
    val query = 
     s"""
      samples2 <= 
        for s1 in samples union 
          {(sid2 := s1.bcr_patient_uuid, aid2 := s1.bcr_aliquot_uuid)};

      intermMatrix2 <= 
        for s2 in samples2 union 
          for i in $initScores union
            if (s2.sid2 = i.hybrid_case)
            then {(hybrid_sample := s2.aid2, hybrid_gene := i.hybrid_gene, hybrid_score := i.hybrid_score)}

     """

    val parser = Parser(tbls)
    val program = parser.parse(query).get.asInstanceOf[Program]
}

object LetTest0 {
  def apply(letOpt: Boolean = false): Query = new LetTest0(letOpt)
  def apply(): LetTest0 = new LetTest0()
}