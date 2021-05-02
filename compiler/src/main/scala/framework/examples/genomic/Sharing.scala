package framework.examples.genomic

import framework.common._
import framework.examples.Query
import framework.nrc.Parser
// import scala.collection.mutable.Map

object HybridQuery extends DriverGene {
  
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

  val name = "HybridQuery"
  
  val tbls = Map("occurrences" -> occurmids.tp, 
                  "copynumber" -> copynum.tp, 
                  "samples" -> samples.tp)

    val cnavg = "(((c.cn_copy_number + c.min_copy_number) + c.max_copy_number) / 3)"
    val imp = """if (t.impact = "HIGH") then 0.80 
                  else if (t.impact = "MODERATE") then 0.50
                  else if (t.impact = "LOW") then 0.30
                  else 0.01"""

    val query = 
      s"""
        HybridQuery <=
        for s in samples union 
          {(sample := s.bcr_patient_uuid, scores := 
            (for o in occurrences union 
              if (s.bcr_patient_uuid = o.donorId) 
              then for t in o.transcript_consequences union 
                for c in copynumber union 
                  if (t.gene_id = c.cn_gene_id && s.bcr_aliquot_uuid = c.cn_aliquot_uuid)
                  then {(gene := t.gene_id, score := ((($cnavg * $imp) * t.polyphen_score) * t.sift_score) )}).sumBy({gene}, {score})
          )}

      """

    val parser = Parser(tbls)
    val program = parser.parse(query).get.asInstanceOf[Program]

}

/** Queries for sharing benchmark with filters **/
object TestBaseQuery extends DriverGene {
  
  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
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

  val name = "TestBaseQuery"
  
  val tbls = Map("occurrences" -> occurmids.tp, 
                  "copynumber" -> copynum.tp, 
                  "samples" -> samples.tp)

  // all samples that have a TP53 mutation with non-high impact
  val query = 
    s"""
      cnvCases1 <= 
        for s in samples union 
          for c in copynumber union 
            if (s.bcr_aliquot_uuid = c.cn_aliquot_uuid)
            then {(sid := s.bcr_patient_uuid, gene := c.cn_gene_id, cnum := c.cn_copy_number)};

      hybridScore1 <= 
          for o in occurrences union
            {( oid := o.oid, sid1 := o.donorId, cands1 := 
              ( for t in o.transcript_consequences union
                 if (t.sift_score > 0.0)
                 then for c in cnvCases1 union
                    if (t.gene_id = c.gene && o.donorId = c.sid) then
                      {( gene1 := t.gene_id, score1 := (c.cnum + 0.01) * if (t.impact = "HIGH") then 0.80 
                          else if (t.impact = "MODERATE") then 0.50
                          else if (t.impact = "LOW") then 0.30
                          else 0.01 )}).sumBy({gene1}, {score1}) )}
    """

    val parser = Parser(tbls)
    val program = parser.parse(query).get.asInstanceOf[Program]

}

object TestBaseQuery2 extends DriverGene {
  
  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
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

  val name = "TestBaseQuery2"
  
  val tbls = Map("occurrences" -> occurmids.tp, 
                  "copynumber" -> copynum.tp, 
                  "samples" -> samples.tp)

  // all samples that have a TP53 mutation with non-high impact
  val query = 
    s"""
      cnvCases2 <= 
        for s in samples union 
          for c in copynumber union 
            if (s.bcr_aliquot_uuid = c.cn_aliquot_uuid)
            then {(sid := s.bcr_patient_uuid, gene := c.cn_gene_id, cnum := c.cn_copy_number)};

      hybridScore2 <= 
        for o in occurrences union
          {( oid := o.oid, sid2 := o.donorId, cands2 := 
            ( for t in o.transcript_consequences union
               if (t.polyphen_score > 0.0)
               then for c in cnvCases2 union
                  if (t.gene_id = c.gene && o.donorId = c.sid) then
                    {( gene2 := t.gene_id, score2 := (c.cnum + 0.01) * t.polyphen_score )}).sumBy({gene2}, {score2}) )}
    """

    val parser = Parser(tbls)
    val program = parser.parse(query).get.asInstanceOf[Program]

}

object TestBaseQuery3 extends DriverGene {
  
  override def loadTables(shred: Boolean = false, skew: Boolean = false): String = ""

  val name = "TestBaseQuery3"
  
  val tbls = Map("occurrences" -> occurmids.tp, 
                  "copynumber" -> copynum.tp, 
                  "samples" -> samples.tp)

  // all samples that have a TP53 mutation with non-high impact
  val query = 
    s"""
      cnvCases3 <= 
        for s in samples union  
          {(sid := s.bcr_patient_uuid)};

      hybridScore3 <=
        for o in occurrences union 
          {( oid := o.oid, sid3 := o.donorId, cands3 := 
            for t in o.transcript_consequences union 
              {( gene3 := t.gene_id, score3 := t.impact )} )}
    """

    val parser = Parser(tbls)
    val program = parser.parse(query).get.asInstanceOf[Program]

}

object SamplesFilterByTP53 extends DriverGene {
  
  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    s"""|${loadOccurrence(shred, skew)}""".stripMargin

  val name = "SamplesFilterByTP53"
  
  val tbls = Map("occurrences" -> occurmids.tp, 
                  "cnvCases" -> cnvCases.tp, 
                  "samples" -> samples.tp)

  // all samples that have a TP53 mutation with non-high impact
  val sampleFilter = 
    s"""
      FilterSamples <= dedup(for o in occurrences union
        for t in o.transcript_consequences union
          if (t.gene_id = "ENSG00000141510" && t.impact != "HIGH") then 
            {( sid := o.donorId )})

      HybridScores <= for s in FilterSamples union
        for o in occurrences union
         if (s.sid = o.donorId) then 
         {( oid := o.oid, sid := o.donorId, cands := 
           ( for t in o.transcript_consequences union
              for c in cnvCases union
                if (o.donorId = c.cn_case_uuid && t.gene_id = c.cn_gene_id) then
                  {( gene := t.gene_id, score := (c.cn_copy_number + 0.01) * if (t.impact = "HIGH") then 0.80 
                      else if (t.impact = "MODERATE") then 0.50
                      else if (t.impact = "LOW") then 0.30
                      else 0.01 )}).sumBy({gene}, {score}) )}
    """

    val parser = Parser(tbls)
    val program0: Program = parser.parse(sampleFilter).get.asInstanceOf[Program]

    val program = Program(Assignment("cnvCases", mapCNV) +: program0.statements)
}

object HybridTP53 extends DriverGene {
  
  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    s"""|${super.loadTables(shred, skew)}
        |${loadOccurrence(shred, skew)}
        |${loadCopyNumber(shred, skew)}""".stripMargin

  val name = "HybridTP53"
  
  val tbls = Map("occurrences" -> occurmids.tp, 
                 "cnvCases" -> cnvCases.tp)

  val qstr = 
    s"""
      for o in occurrences union
        {( oid := o.oid, sid := o.donorId, cands := 
          ( for t in o.transcript_consequences union
            if (t.gene_id != "TP53") then  
              for c in cnvCases union
                if (o.donorId = c.cn_case_uuid && t.gene_id = c.cn_gene_id) then
                  {( gene := t.gene_id, score := c.cn_copy_number * if (t.impact = "HIGH") then 0.80 
                      else if (t.impact = "MODERATE") then 0.50
                      else if (t.impact = "LOW") then 0.30
                      else 0.01 )}).sumBy({gene}, {score}) )}
    """

     val parser = Parser(tbls)
     val query: BagExpr = parser.parse(qstr, parser.term).get.asInstanceOf[BagExpr]
     
     val program = Program(Assignment("cnvCases", mapCNV), Assignment(name, query))

}


object HybridBRCA extends DriverGene {
  
  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    s"""|${super.loadTables(shred, skew)}
        |${loadOccurrence(shred, skew)}
        |${loadCopyNumber(shred, skew)}""".stripMargin

  val name = "HybridBRCA"

  val tbls = Map("occurrences" -> occurmids.tp, 
                 "cnvCases" -> cnvCases.tp)

  val qstr = 
    s"""
      for o in occurrences union
        {( oid := o.oid, sid := o.donorId, cands := 
          ( for t in o.transcript_consequences union
            if (t.gene_id != "BRCA") then  
              for c in cnvCases union
                if (o.donorId = c.cn_case_uuid && t.gene_id = c.cn_gene_id) then
                  {( gene := t.gene_id, score := c.cn_copy_number * if (t.impact = "HIGH") then 0.80 
                      else if (t.impact = "MODERATE") then 0.50
                      else if (t.impact = "LOW") then 0.30
                      else 0.01 )}).sumBy({gene}, {score}) )}
    """

     val parser = Parser(tbls)
     val query: BagExpr = parser.parse(qstr, parser.term).get.asInstanceOf[BagExpr]
     
     val program = Program(Assignment("cnvCases", mapCNV), Assignment(name, query))

}

object SequentialFilters extends DriverGene {

 override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    s"""|${super.loadTables(shred, skew)}
        |${loadOccurrence(shred, skew)}
        |${loadCopyNumber(shred, skew)}""".stripMargin

  val name = "SequentialFilters"

  val tp53 = HybridTP53.query
  val brca = HybridBRCA.query

  val program = Program(Assignment("cnvCases", mapCNV), 
  	Assignment(HybridTP53.name, HybridTP53.query.asInstanceOf[SequentialFilters.BagExpr]), 
	Assignment(HybridBRCA.name, HybridBRCA.query.asInstanceOf[SequentialFilters.BagExpr]))

}


object SharedFilters extends DriverGene {

  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    s"""|${super.loadTables(shred, skew)}
        |${loadOccurrence(shred, skew)}
        |${loadCopyNumber(shred, skew)}""".stripMargin

  val name = "SharedFilters"

  val tbls = Map("occurrences" -> occurmids.tp, 
                 "copynumber" -> copynum.tp, 
				 "samples" -> biospec.tp)
 
  val cnvFilter = 
  	s"""
		for c in copynumber union
		  if (c.cn_gene_id != "TP53" || c.cn_gene_id != "BRCA") then
			for s in samples union 
			  if (c.cn_aliquot_uuid = s.bcr_aliquot_uuid) then 
			  {(cn_case_uuid := s.bcr_patient_uuid, cn_copy_num := c.cn_copy_number, cn_gene := c.cn_gene_id)}
	"""

  val cnvParser = Parser(tbls)
  val cnvQuery: BagExpr = cnvParser.parse(cnvFilter, cnvParser.term).get.asInstanceOf[BagExpr]
	
  val tbls2 = tbls ++ Map("cnvFilter" -> cnvQuery.tp)

  val occurShare = 
    s"""
      for o in occurrences union
        {( oid := o.oid, sid := o.donorId, cands := 
          ( for t in o.transcript_consequences union
            if (t.gene_id != "BRCA" || t.gene_id != "TP53") then  
              for c in cnvFilter union
                if (o.donorId = c.cn_case_uuid && t.gene_id = c.cn_gene) then
                  {( gene := t.gene_id, score := c.cn_copy_num * if (t.impact = "HIGH") then 0.80 
                      else if (t.impact = "MODERATE") then 0.50
                      else if (t.impact = "LOW") then 0.30
                      else 0.01 )}).sumBy({gene}, {score}) )}
    """

  val occurParser = Parser(tbls2)
  val occurQuery: BagExpr = occurParser.parse(occurShare, occurParser.term).get.asInstanceOf[BagExpr]
 
  val tbls3 = tbls2 ++ Map("occurShare" -> occurQuery.tp)
 
  val tp53 = 
    s"""
      for o in occurShare union
        {( oid := o.oid, sid := o.sid, cands := 
           for t in o.cands union
            if (t.gene != "TP53") then  
              {( gene := t.gene, score := t.score )} )}
    """

  val tp53Parser = Parser(tbls3)
  val tp53Query: BagExpr = tp53Parser.parse(tp53, tp53Parser.term).get.asInstanceOf[BagExpr]

  val brca = 
    s"""
      for o in occurShare union
        {( oid := o.oid, sid := o.sid, cands := 
           for t in o.cands union
            if (t.gene != "BRCA") then  
              {( gene := t.gene, score := t.score )} )}
    """

  val brcaParser = Parser(tbls3)
  val brcaQuery: BagExpr = brcaParser.parse(brca, brcaParser.term).get.asInstanceOf[BagExpr]

     
  val program = Program(Assignment("cnvFilter", cnvQuery), Assignment("occurShare", occurQuery),
  	Assignment("RewriteTP53", tp53Query), Assignment("RewriteBRCA", brcaQuery))

}

object SharedFiltersNoNest extends DriverGene {

  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    s"""|${loadBiospec(shred, skew, "samples")}
        |${loadOccurrence(shred, skew)}
        |${loadCopyNumber(shred, skew)}""".stripMargin

  val name = "SharedFiltersNoNest"

  val tbls = Map("occurrences" -> occurmids.tp, 
                 "copynumber" -> copynum.tp, 
         "samples" -> biospec.tp)
 
  val cnvFilter = 
    s"""
    for c in copynumber union
      if (c.cn_gene_id != "TP53" || c.cn_gene_id != "BRCA") then
      for s in samples union 
        if (c.cn_aliquot_uuid = s.bcr_aliquot_uuid) then 
        {(cn_case_uuid := s.bcr_patient_uuid, cn_copy_num := c.cn_copy_number, cn_gene := c.cn_gene_id)}
  """

  val cnvParser = Parser(tbls)
  val cnvQuery: BagExpr = cnvParser.parse(cnvFilter, cnvParser.term).get.asInstanceOf[BagExpr]
  
  val tbls2 = tbls ++ Map("cnvFilter" -> cnvQuery.tp)

  val occurShare = 
    s"""
      (for o in occurrences union
        for t in o.transcript_consequences union
          if (t.gene_id != "BRCA" || t.gene_id != "TP53") then  
            for c in cnvFilter union
              if (o.donorId = c.cn_case_uuid && t.gene_id = c.cn_gene) then
                {( oid := o.oid, sid := o.donorId, gene := t.gene_id, score := c.cn_copy_num * if (t.impact = "HIGH") then 0.80 
                      else if (t.impact = "MODERATE") then 0.50
                      else if (t.impact = "LOW") then 0.30
                      else 0.01 )}).sumBy({oid, sid, gene}, {score})
    """

  val occurParser = Parser(tbls2)
  val occurQuery: BagExpr = occurParser.parse(occurShare, occurParser.term).get.asInstanceOf[BagExpr]
 
  val tbls3 = tbls2 ++ Map("occurShare" -> occurQuery.tp)
 
  val tp53 = 
    s"""
      (for o in occurShare union 
        if (o.gene != "TP53") then {o}).groupBy({oid, sid}, {gene, score}, "cands")
    """

  val tp53Parser = Parser(tbls3)
  val tp53Query: BagExpr = tp53Parser.parse(tp53, tp53Parser.term).get.asInstanceOf[BagExpr]

  val brca = 
    s"""
      (for o in occurShare union 
        if (o.gene != "BRCA") then {o}).groupBy({oid, sid}, {gene, score}, "cands")
    """

  val brcaParser = Parser(tbls3)
  val brcaQuery: BagExpr = brcaParser.parse(brca, brcaParser.term).get.asInstanceOf[BagExpr]

     
  val program = Program(Assignment("cnvFilter", cnvQuery), Assignment("occurShare", occurQuery),
    Assignment("RewriteTP53", tp53Query), Assignment("RewriteBRCA", brcaQuery))

}

/** Sharing with projection **/

object HybridImpact extends DriverGene {
  
  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    s"""|${super.loadTables(shred, skew)}
        |${loadOccurrence(shred, skew)}
        |${loadCopyNumber(shred, skew)}""".stripMargin

  val name = "HybridImpact"
  
  val tbls = Map("occurrences" -> occurmids.tp, 
                 "cnvCases" -> cnvCases.tp)

  val qstr = 
    s"""
      for o in occurrences union
        {( oid := o.oid, sid := o.donorId, cands := 
          ( for t in o.transcript_consequences union
              for c in cnvCases union
                if (o.donorId = c.cn_case_uuid && t.gene_id = c.cn_gene_id) then
                  {( gene := t.gene_id, score := (c.cn_copy_number + 0.01) * if (t.impact = "HIGH") then 0.80 
                      else if (t.impact = "MODERATE") then 0.50
                      else if (t.impact = "LOW") then 0.30
                      else 0.01 )}).sumBy({gene}, {score}) )}
    """

     val parser = Parser(tbls)
     val query: BagExpr = parser.parse(qstr, parser.term).get.asInstanceOf[BagExpr]
     
     val program = Program(Assignment("cnvCases", mapCNV), Assignment(name, query))

}

object HybridScores extends DriverGene {
  
  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    s"""|${super.loadTables(shred, skew)}
        |${loadOccurrence(shred, skew)}
        |${loadCopyNumber(shred, skew)}""".stripMargin

  val name = "HybridScores"

  val tbls = Map("occurrences" -> occurmids.tp, 
                 "cnvCases" -> cnvCases.tp)

  val qstr = 
    s"""
      for o in occurrences union
        {( oid := o.oid, sid := o.donorId, cands := 
          ( for t in o.transcript_consequences union
              for c in cnvCases union
                if (o.donorId = c.cn_case_uuid && t.gene_id = c.cn_gene_id) then
                  {( gene := t.gene_id, score := (c.cn_copy_number + 0.01) * t.polyphen_score )}).sumBy({gene}, {score}) )}
    """

     val parser = Parser(tbls)
     val query: BagExpr = parser.parse(qstr, parser.term).get.asInstanceOf[BagExpr]
     
     val program = Program(Assignment("cnvCases", mapCNV), Assignment(name, query))

}

object SequentialProjections extends DriverGene {

 override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    s"""|${super.loadTables(shred, skew)}
        |${loadOccurrence(shred, skew)}
        |${loadCopyNumber(shred, skew)}""".stripMargin

  val name = "SequentialProjections"

  val impacts = HybridImpact.query
  val scores = HybridScores.query

  val program = Program(Assignment("cnvCases", mapCNV), 
    Assignment(HybridImpact.name, HybridImpact.query.asInstanceOf[SequentialProjections.BagExpr]), 
  Assignment(HybridScores.name, HybridScores.query.asInstanceOf[SequentialProjections.BagExpr]))

}

object SharedProjections extends DriverGene {

  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    s"""|${loadBiospec(shred, skew)}
        |${loadOccurrence(shred, skew)}
        |${loadCopyNumber(shred, skew)}""".stripMargin

  val name = "SharedProjections"

  val tbls = Map("occurrences" -> occurmids.tp, 
                 "cnvCases" -> cnvCases.tp, 
         "biospec" -> biospec.tp)
  
  val tbls2 = tbls

  /**val occurShare = 
    s"""
      for o in occurrences union
        {( oid := o.oid, sid := o.donorId, cands := 
          (for t in o.transcript_consequences union
            for c in cnvCases union
              if (o.donorId = c.cn_case_uuid && t.gene_id = c.cn_gene_id) then
                {( gene := t.gene_id, score1 := c.cn_copy_number * if (t.impact = "HIGH") then 0.80 
                  else if (t.impact = "MODERATE") then 0.50
                  else if (t.impact = "LOW") then 0.30
                  else 0.01, score2 := c.cn_copy_number * t.polyphen_score )}).sumBy({gene}, {score1, score2}) )}
    """**/

  val occurShare = 
    s"""
      for o in occurrences union
        {( oid := o.oid, sid := o.donorId, cands := 
          (for t in o.transcript_consequences union
            for c in cnvCases union
              if (o.donorId = c.cn_case_uuid && t.gene_id = c.cn_gene_id) then
                {( gene := t.gene_id, cnum := c.cn_copy_number + 0.01, impactNum := if (t.impact = "HIGH") then 0.80 
                  else if (t.impact = "MODERATE") then 0.50
                  else if (t.impact = "LOW") then 0.30
                  else 0.01, poly := t.polyphen_score )}).sumBy({gene}, {cnum, impactNum, poly}) )}
    """

  val occurParser = Parser(tbls2)
  val occurQuery: BagExpr = occurParser.parse(occurShare, occurParser.term).get.asInstanceOf[BagExpr]
 
  val tbls3 = tbls2 ++ Map("occurShare" -> occurQuery.tp)
 
  // val q1Rewrite = 
  //   s"""
  //     for o in occurShare union
  //       {( oid := o.oid, sid := o.sid, cands := 
  //          for t in o.cands union
  //            {( gene := t.gene, score := t.score1 )} )}
  //   """

  val q1Rewrite = 
    s"""
      for o in occurShare union
        {( oid := o.oid, sid := o.sid, cands := 
           for t in o.cands union
             {( gene := t.gene, score := t.impactNum * t.cnum )} )}
    """

  val q1RewriteParser = Parser(tbls3)
  val q1RewriteQuery: BagExpr = q1RewriteParser.parse(q1Rewrite, q1RewriteParser.term).get.asInstanceOf[BagExpr]

  // val q2Rewrite = 
  //   s"""
  //     for o in occurShare union
  //       {( oid := o.oid, sid := o.sid, cands := 
  //          for t in o.cands union
  //            {( gene := t.gene, score := t.score2 )} )}
  //   """

  val q2Rewrite = 
    s"""
      for o in occurShare union
        {( oid := o.oid, sid := o.sid, cands := 
           for t in o.cands union
             {( gene := t.gene, score := t.poly * t.cnum )} )}
    """

  val q2RewriteParser = Parser(tbls3)
  val q2RewriteQuery: BagExpr = q2RewriteParser.parse(q2Rewrite, q2RewriteParser.term).get.asInstanceOf[BagExpr]

     
  val program = Program(Assignment("cnvCases", mapCNV), Assignment("occurShare", occurQuery),
    Assignment("RewriteImpact", q1RewriteQuery), Assignment("RewriteScores", q2RewriteQuery))

}

object SharedProjectionsNoNest extends DriverGene {

  override def loadTables(shred: Boolean = false, skew: Boolean = false): String =
    s"""|${loadBiospec(shred, skew)}
        |${loadOccurrence(shred, skew)}
        |${loadCopyNumber(shred, skew)}""".stripMargin

  val name = "SharedProjectionsNoNest"

  val tbls = Map("occurrences" -> occurmids.tp, 
                 "cnvCases" -> cnvCases.tp, 
         "biospec" -> biospec.tp)
  
  val tbls2 = tbls

  val occurShare = 
    s"""
      (for o in occurrences union
        for t in o.transcript_consequences union
          for c in cnvCases union
            if (o.donorId = c.cn_case_uuid && t.gene_id = c.cn_gene_id) then
              {( oid := o.oid, sid := o.donorId, gene := t.gene_id, cnum := c.cn_copy_number + 0.01, 
                impactNum := if (t.impact = "HIGH") then 0.80 
                  else if (t.impact = "MODERATE") then 0.50
                  else if (t.impact = "LOW") then 0.30
                  else 0.01, poly := t.polyphen_score )}).sumBy({oid, sid, gene}, {cnum, impactNum, poly})
    """

  val occurParser = Parser(tbls2)
  val occurQuery: BagExpr = occurParser.parse(occurShare, occurParser.term).get.asInstanceOf[BagExpr]
 
  val tbls3 = tbls2 ++ Map("occurShare" -> occurQuery.tp)
 
  val q1Rewrite = 
    s"""
      (for o in occurShare union
        {( oid := o.oid, sid := o.sid, gene := o.gene, 
          score := o.cnum * o.impactNum )}).groupBy({oid, sid}, {gene, score}, "cands")
    """

  val q1RewriteParser = Parser(tbls3)
  val q1RewriteQuery: BagExpr = q1RewriteParser.parse(q1Rewrite, q1RewriteParser.term).get.asInstanceOf[BagExpr]

  val q2Rewrite = 
    s"""
      (for o in occurShare union
        {( oid := o.oid, sid := o.sid, gene := o.gene, 
          score := o.cnum * o.poly)}).groupBy({oid, sid}, {gene, score}, "cands")
    """

  val q2RewriteParser = Parser(tbls3)
  val q2RewriteQuery: BagExpr = q2RewriteParser.parse(q2Rewrite, q2RewriteParser.term).get.asInstanceOf[BagExpr]

     
  val program = Program(Assignment("cnvCases", mapCNV), Assignment("occurShare", occurQuery),
    Assignment("RewriteImpact", q1RewriteQuery), Assignment("RewriteScores", q2RewriteQuery))

}
