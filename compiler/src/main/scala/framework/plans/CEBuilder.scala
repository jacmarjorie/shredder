package framework.plans

import framework.common._
import scala.collection.immutable.{Map => IMap}
import scala.collection.mutable.{Map, HashMap}
import java.util.UUID.randomUUID

case class CE(name: String, cover: CExpr, sig: Integer, ses: List[SE])

object CE{
  def apply(cover: CExpr, sig: Integer, ses: List[SE]): CE = 
    CE("", cover, sig, ses)
}

object CEBuilder extends Extensions {

  val normalizer = new BaseNormalizer{}
  import normalizer._

  val vmap = Map.empty[String, String]

  def getFromVmap(k: String): String = vmap.get(k) match {
    case Some(f) => 
      // need a better way to handle joins on domains
      if (k == "_1" && vmap.contains("_LABEL")) "_LABEL"
      else f
    case _ => k
  }

  // todo preserve height
  def buildCoverFromSE(plans: List[SE]): CExpr = {
    val plan1 = plans.head.subplan
    val ce1 = plans.tail.map(se => se.subplan).reduce((ce, p) => buildCover(ce, p))
    buildCover(plan1, ce1)
  }

  // renames, assumes it is in the cache - used for testing
  // def buildCovers(subs: Map[Integer, List[SE]]): List[CE] = subs.map{
  //   case (id, se) => 
  //     val cover = buildCoverFromSE(se)
  //     val cnamed = "Cover"+randomUUID().toString().replace("-", "")
  //     CE(cnamed, cover, id, se)
  // }.toList

  def buildCoverMap(subs: Map[Integer, List[SE]], nameMap: Map[String, Integer] = Map.empty[String, Integer]): IMap[Integer, CNamed] = subs.flatMap{
    case (sig, ses) =>  
      if (!ses.head.subplan.isCacheUnfriendly && ses.size > 1){
        Seq((sig, CNamed("Cover"+randomUUID().toString().replace("-", ""), buildCovers(ses.map(_.subplan), nameMap))))
      }else Nil
  }.toMap

  def buildCovers(plans: List[CExpr], nameMap: Map[String, Integer] = Map.empty[String, Integer]): CExpr = plans match {
    case head :: Nil => head
    case head :: tail => 
      val ce1 = tail.reduce((ce, p) => buildCover(ce, p, nameMap))
      buildCover(head, ce1, nameMap)
    case _ => sys.error("empty subepxression list")
  }

  // cover should not rename anything
  def buildCover(plan1: CExpr, plan2: CExpr, nameMap: Map[String, Integer] = Map.empty[String, Integer]): CExpr = (plan1, plan2) match {
    
    // equivalence
    case y if plan1.vstr == plan2.vstr => plan1

    // somehow need to ensure that cnvCases1 is actually selected 
    // for this to work
    // TODO
    case (i1:AddIndex, i2:AddIndex) => i1
    case (i1:InputRef, i2:InputRef) => i1

    // cover building
    case (Reduce(in1, v1, ks1, vs1), Reduce(in2, v2, ks2, vs2)) => 
      val child = buildCover(in1, in2, nameMap)
      val ks = ks1.toSet ++ ks2.toSet
      val vs = vs1.toSet ++ vs2.toSet
      val v = Variable.freshFromBag(child.tp)
      Reduce(child, v, ks.toList.map(k => getFromVmap(k)), 
        vs.toList.map(k => getFromVmap(k)))

    case (u1:UnnestOp, u2:UnnestOp) => 
      assert(u1.path == u2.path)
      val child = buildCover(u1.in, u2.in, nameMap)
      val v = Variable.freshFromBag(child.tp)
      val v2 = Variable.freshFromBag(v.tp.asInstanceOf[RecordCType](u1.path))
      // assume lower level, since it should be pushed
      val cond = or(replace(u1.filter, v2), replace(u2.filter, v2))
      if (u1.outer) OuterUnnest(child, v, u1.path, v2, cond, u1.fields ++ u2.fields)
      else Unnest(child, v, u1.path, v2, cond, u1.fields ++ u2.fields)

    // capture below joins
    case (j1:JoinOp, j2:JoinOp) =>
      assert(j1.ps == j2.ps)

      val lsig = SEUtils.signature(j1.left, nameMap)
      val rsig = SEUtils.signature(j2.left, nameMap)
      val (left, right) = 
        if (lsig == rsig) (buildCover(j1.left, j2.left, nameMap), buildCover(j1.right, j2.right, nameMap))
        else (buildCover(j1.left, j2.right, nameMap), buildCover(j1.right, j2.left, nameMap))

      val v1 = Variable.freshFromBag(left.tp)
      val v2 = Variable.freshFromBag(right.tp)

      var cond = j1.cond
      // var cond = replace(j1.cond, v1, useType = true)
      // cond = replace(cond, v2, useType = true)

      if (j1.jtype == "left_outer" || j2.jtype == "left_outer") OuterJoin(left, v1, right, v2, cond, j1.fields ++ j2.fields)
      else Join(left, v1, right, v2, cond, j1.fields ++ j2.fields)

    // union columns
    // TODO no implicit renaming should happen in the cover expression
    case (Projection(in1, v1, f1:Record, fs1), Projection(in2, v2, f2:Record, fs2)) => 

      val child = buildCover(in1, in2, nameMap)
      val v = Variable.freshFromBag(child.tp)

      // not sure if this will work for everything...
      def updateVmap(r: Record): IMap[String, CExpr] = {
        r.fields.map{
          case (field1, Project(v, field2)) => (vmap.get(field1), vmap.get(field2)) match {
              case (Some(n1), Some(n2)) => (n2, Project(v, n2))
              case (Some(n1), None) => (n1, Project(v, n1))
              case _ => vmap(field1) = field2; (field2, Project(v, field2))
            }
          // keep complex expressions, assuming they reduce the 
          // overall amount of projections
          case (field1, field2) => (field1, replace(field2, v))
        }.toMap
      }

      val r = Record(updateVmap(f1) ++ updateVmap(f2))
      val nr = replace(r, v)
      val nfs1 = fs1.toList.map(k => getFromVmap(k))
      val nfs2 = fs2.toList.map(k => getFromVmap(k))
      Projection(child, v, nr, nfs1 ++ nfs2)

   // OR filters
    case (Select(in1, v1, f1), Select(in2, v2, f2)) =>
      val v = Variable.fresh(in1.tp)
      val child = buildCover(in1, in2, nameMap)
      or(replace(f1, v), replace(f2, v)) match {
        case Constant(true) => child
        case cond => Select(child, v, cond)
      }

    case (o, f:FlatDict) => buildCover(o, f.in, nameMap)
    case (o, g:GroupDict) => buildCover(o, g.in, nameMap)

    case (f:FlatDict, o) => buildCover(o,f, nameMap)
    case (g:GroupDict, o) => buildCover(o, g, nameMap)

    case _ =>  sys.error(s"unsupported operator pair:\n ${Printer.quote(plan1)}\n${Printer.quote(plan2)}")

  }

}
