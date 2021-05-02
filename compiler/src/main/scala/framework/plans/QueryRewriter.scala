package framework.plans

import framework.common._
import scala.collection.immutable.{Map => IMap}
import scala.collection.mutable.{Map, HashMap}
import scala.collection.mutable.ListBuffer

// move the covers here as well..
class QueryRewriter(sigs: HashMap[(CExpr, Int), Integer] = HashMap.empty[(CExpr, Int), Integer], 
  names: Map[String, Integer] = Map.empty[String, Integer]) extends Extensions {

  var coverset: ListBuffer[CExpr] = ListBuffer.empty[CExpr]
  val vmap: Map[String, String] = Map.empty[String, String]

  val depmap: Map[String, Set[CExpr]] = Map[String, Set[CExpr]]().withDefaultValue(Set.empty[CExpr])
  var ordered: Vector[CExpr] = Vector.empty[CExpr]

  def updateOrder(e: CExpr): Unit = {
    if (!ordered.contains(e)) {
      ordered = ordered :+ e
    }
    if (coverset.contains(e)){
      coverset = coverset - e
    }
  }

  // init rewrites give a ce
  // recall that a ce has:
  //   OR'd the filters
  //   UNION'd the projections and aggregations
  def rewritePlans(ce: CE): CE = {
    CE(ce.cover, ce.sig, ce.ses.map(s => 
      SE(s.wid, rewritePlan(s.subplan, ce.name, ce.cover), s.height)))
  }

  def rewritePlans(plans: Vector[(CExpr, Int)], covers: IMap[Integer, CNamed]): Vector[CExpr] = {
    
    // first rewrite covers over cover
    val rewriteCovers = covers.transform((sig, cover) => 
      rewriteCoverOverCover(cover, covers))

    // keep track of which inputs the covers are dependent on
    rewriteCovers.foreach{ p => getInputs(p._2, p._2) }

    // then rewrite plans over cover
    val rewriteQueries = plans.map(p => rewritePlanOverCover(p, rewriteCovers))

    rewriteQueries.foreach{
      p => p match {
        case l:LinearCSet => 
          l.exprs.foreach{ p1 => 
            updateOrder(p1)
            getDeps(p1).foreach(f => 
              updateOrder(f)
            )
          }
        case _ => ???
      }
    }

    ordered = coverset.toVector ++ ordered

    // for each query 
    //   if query is an input to a cover
    //   then evaluate it before the cover
    // after all queries have been addressed
    // put the covers before

    rewriteQueries

  }

  def getDeps(p: CExpr): Set[CExpr] = p match {
    case CNamed(n, e) => depmap.getOrElse(n, Set.empty[CExpr]) 
    case _ => Set()
  }

  def rewriteCoverOverCover(plan: CNamed, covers: IMap[Integer, CNamed]): CNamed = plan.e match {
    case u:UnaryOp => 
      SEUtils.equivSig((u.in, -1), names)(sigs)
      val ncov = CNamed(plan.name, rewritePlanOverCover((plan.e, -1), covers))
      if (ncov.vstr != plan.vstr) coverset = coverset :+ ncov
      else coverset = ncov +: coverset 
      ncov
    case i:InputRef => CNamed(plan.name, i)
    case _ => sys.error(s"unimplemented $plan")
  }

  def getInputs(e: CExpr, cov: CExpr): Unit = e match {
    case c:CNamed => getInputs(c.e, c)
    case i:InputRef => depmap(i.data) = depmap(i.data) + cov//Set(i.data)
    case v:Variable => depmap(v.name) = depmap(v.name) + cov//Set(v.name)
    case u:UnaryOp => getInputs(u.in, cov)
    case n:Nest => getInputs(n.in, cov)
    case j:JoinOp => getInputs(j.left, cov); getInputs(j.right, cov)
    case _ => 
  }

  // for each plan 
  //   if plan is an immedate similar expression of cover in cache
  //      then rewrite plan over cover 
  //   else rewrite plan with recurse on subplan
  //
  // a join and a nest will never be a subexpression, so this makes sure that 
  // anything beneath it gets replaced with the proper subexpression
  // 
  // TODO handle the case where the cover expression is rewritten over the other cover expressions
  def rewritePlanOverCover(plan: (CExpr, Int), covers: IMap[Integer, CNamed]): CExpr = {

    val default:Integer = -1
    val sig = sigs.getOrElse(plan, default)

    covers.get(sig) match {

      // in subexpression list
      case Some(cover) => 
        rewritePlan(plan._1, cover.name, cover.e)


      // not in cover, see if subexpression is
      case None => plan match {

        case (j:JoinOp, id) => 
          // assert(j.isEquiJoin)
          val lCover = rewritePlanOverCover((j.left, id), covers)
          val rCover = rewritePlanOverCover((j.right, id), covers)
          val lv = Variable.freshFromBag(lCover.tp)
          val rv = Variable.freshFromBag(rCover.tp)

          if (j.jtype.contains("outer")) OuterJoin(lCover, lv, rCover, rv, j.cond, j.fields)
          else Join(lCover, lv, rCover, rv, j.cond, j.fields)

        case (n: Nest, id) => 
          val childCover = rewritePlanOverCover((n.in, id), covers)
          val v = Variable.freshFromBag(childCover.tp)
          Nest(childCover, v, n.key, replace(n.value, v), replace(n.filter, v), n.nulls, n.ctag)

        case (r:Reduce, id) => 
          val childCover = rewritePlanOverCover((r.in, id), covers)
          val v = Variable.freshFromBag(childCover.tp)
          Reduce(childCover, v, r.keys, r.values)

        // handle the outer unnest case
        case (u:UnnestOp, id) => 
          val childCover = rewritePlanOverCover((u.in, id), covers)
          val v = Variable.freshFromBag(childCover.tp)
          if (u.outer) OuterUnnest(childCover, v, u.path, u.v2, u.filter, u.fields)
          else Unnest(childCover, v, u.path, u.v2, u.filter, u.fields)

        case (p:Projection, id) => 
          val childCover = rewritePlanOverCover((p.in, id), covers)
          val v = Variable.freshFromBag(childCover.tp)
          Projection(childCover, v, replace(p.filter, v), p.fields)

        case (s:Select, id) => 
          val childCover = rewritePlanOverCover((s.in, id), covers)
          val v = Variable.freshFromBag(childCover.tp)
          Select(childCover, v, replace(s.p, v))

        case (i:AddIndex, id) => 
          // is subexpression a cover?
          val childCover = rewritePlanOverCover((i.in, id), covers)
          AddIndex(childCover, i.name)

        case (c:CNamed, id) => 
          val nsig = names.getOrElse(c.name, default)
          val childCover = covers.get(nsig) match {
            case Some(cov:CNamed) => 
              println("in this case with "+c.name+" "+cov.name)
              // rewritePlan(c.e, cov.name, cov.e)
              // println(Printer.quote(rwp))
              InputRef(cov.name, cov.tp)
            case _ => rewritePlanOverCover((c.e, id), covers)
          }

          CNamed(c.name, childCover)
          
        case (l:LinearCSet, id) => 
          LinearCSet(l.exprs.map( exp => rewritePlanOverCover((exp, id), covers)))

        case _ => plan._1
      }

    }

  }

  def rewritePlan(plan: CExpr, name:String, coverPlan: CExpr): CExpr = {
  
    val cover = Variable(name, coverPlan.tp)
    val v = Variable.freshFromBag(cover.tp)

    (plan, coverPlan) match {

      case y if plan.vstr == coverPlan.vstr => coverPlan

      case (r1 @ Reduce(p @ Projection(in, _, r:Record, fs), _, ks1, vs1), r2:Reduce) =>

        // update name map here
        r.fields.foreach(f => f match {
            case (field1, Project(_, field2)) => vmap(field1) = field2
            case _ => vmap(f._1) = f._1
          })

        val names = ks1.map(k => vmap.getOrElse(k, k))

        if (names.toSet == r2.keys.toSet) {
          val nrec = Record(r.fields.map(f => 
            f._1 -> Project(v, vmap.getOrElse(f._1, f._1))))
          Projection(cover, v, nrec, nrec.fields.keys.toList)
        } else {
          val p1 = Projection(cover, v, replace(r, v), fs)
          val v1 = Variable.freshFromBag(p1.tp)
          Reduce(p1, v1, ks1, vs1)
        }  

      // need to handle outer case
      case (u1:UnnestOp, u2:UnnestOp) => 
        assert(u1.path == u2.path)
        val fs = (u1.fields ++ u2.fields).toSet
        val nrec = Record(fs.map(f => f -> Project(v, f)).toMap)
        Projection(cover, v, nrec, fs.toList)

      // reapply the projection
      case (p1:Projection, p2:Projection) => 

        // handle outer to inner join
        val fields = (p1.in, p2.in) match {
          case (_:Join, _:OuterJoin) => p1.filter.tp.attrs.keySet.toList :+ "remove_nulls"
          case _ => p1.filter.tp.attrs.keySet.toList
        }
        Projection(cover, v, p1.filter, fields)

      // reapply the filter
      case (s:Select, _) => 
        Select(cover, v, replace(s.p, v))

      // the index may or may not be needed...
      case (i:AddIndex, _) => AddIndex(cover, i.name)

      // input refs just are replaced with the cover
      case (i:InputRef, _) => cover

      // all other cases don't rewrite
      case _ => plan
    
    } 

  }

}

object QueryRewriter {
  def apply() = new QueryRewriter(sigs = HashMap.empty[(CExpr, Int), Integer])
  def apply(sigs: HashMap[(CExpr, Int), Integer]) = new QueryRewriter(sigs = sigs)
  def apply(sigs: HashMap[(CExpr, Int), Integer], names: Map[String, Integer] = Map.empty[String, Integer]) = new QueryRewriter(sigs, names)
}
