package framework.plans

import org.scalatest.FunSuite
import framework.common._
import framework.examples.tpch._
import framework.examples.genomic._
import framework.nrc._
import framework.plans.{Equals => CEquals, Project => CProject}

class TestCost extends FunSuite with MaterializeNRC with NRCTranslator {

  val compileCost = false

  val occur = new Occurrence{}
  val cnum = new CopyNumber{}
  val samps = new Biospecimen{}
  val tbls = Map("occurrences" -> BagType(occur.occurmid_type), 
                 "copynumber" -> BagType(cnum.copyNumberType), 
                 "samples" -> BagType(samps.biospecType))

  val parser = Parser(tbls)
  val normalizer = new Finalizer(new BaseNormalizer{})
  val optimizer = new Optimizer()

  def getPlan(query: Program): LinearCSet = {
    val ncalc = normalizer.finalize(translate(query)).asInstanceOf[CExpr]
    optimizer.applyPush(Unnester.unnest(ncalc)(Map(), Map(), None, "_2")).asInstanceOf[LinearCSet]
  }

  test("aggregates"){

    val query1str = 
      s"""
        cnvCases1 <= 
          for c in copynumber union
            for s in samples union 
              if (c.cn_aliquot_uuid = s.bcr_aliquot_uuid)
              then {(cn_case_uuid := s.bcr_patient_uuid, cn_gene_id := c.cn_gene_id, cn_copy_number := c.cn_copy_number)};

        hybridScore1 <= 
          for o in occurrences union
            {( oid := o.oid, sid := o.donorId, cands := 
              ( for t in o.transcript_consequences union
                  for c in cnvCases1 union
                    if (t.gene_id = c.cn_gene_id) then
                      {( gene := t.gene_id, score := (c.cn_copy_number + 0.01) * if (t.impact = "HIGH") then 0.80 
                          else if (t.impact = "MODERATE") then 0.50
                          else if (t.impact = "LOW") then 0.30
                          else 0.01 )}).sumBy({gene}, {score}) )}
      """
    val query1 = parser.parse(query1str).get
    val plan1 = getPlan(query1.asInstanceOf[Program])

    val query2str = 
      s"""
        cnvCases2 <= 
          for c in copynumber union
            for s in samples union 
              if (c.cn_aliquot_uuid = s.bcr_aliquot_uuid)
              then {(cn_case_uuid := s.bcr_patient_uuid, cn_gene_id := c.cn_gene_id, cn_copy_number := c.cn_copy_number)};

        hybridScore2 <= 
          for o in occurrences union
            {( oid := o.oid, sid := o.donorId, cands := 
              ( for t in o.transcript_consequences union
                  for c in cnvCases2 union
                    if (t.gene_id = c.cn_gene_id) then
                      {( gene := t.gene_id, score := (c.cn_copy_number + 0.01) * t.polyphen_score )}).sumBy({gene}, {score}) )}
      """
    val query2 = parser.parse(query2str).get
    val plan2 = getPlan(query2.asInstanceOf[Program])

    // equivsig -> {SE}
    val subs = SEBuilder.sharedSubsFromProgram(Vector(plan1, plan2))

    val ces = subs.map{
      case (id, se) => 
        val cover = CEBuilder.buildCoverFromSE(se)
        CE(cover, id, se)
    }.toList
    
    val stats = StatsCollector.getCost(ces)
    for (s <- stats){
      println(s)
    }

    val costs = ces.map{
      case ce => 
        val scosts = ce.ses.map(s => 
            CostSE(s.wid, s.subplan, s.height, Cost.estimate(s.subplan, stats)))
        CostCE(ce.cover, ce.sig, scosts, Cost.estimate(ce.cover, stats))
    }

    for (c <- costs){
      println(c)
    }

  }

}