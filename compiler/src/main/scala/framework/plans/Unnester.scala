package framework.plans

import framework.common._


/**
  * This is a batch version of the unnesting procedure introduced in the paper. 
  * Rather than the tuple stream version of Fegaras and Maier, 
  * every operator returns an intermediate result.
  */
object Unnester {
  
  type Ctx = (Map[String, Type], Map[String, Type], Option[CExpr], String)
  @inline def u(implicit ctx: Ctx): Map[String, Type] = ctx._1
  @inline def w(implicit ctx: Ctx): Map[String, Type] = ctx._2
  @inline def E(implicit ctx: Ctx): Option[CExpr] = ctx._3
  @inline def tag(implicit ctx: Ctx): String = ctx._4

  def isNestedComp(e: CExpr): Boolean = e match {
    case Variable(_, BagCType(_)) => false
    case c:Comprehension => true
    case c:CReduceBy => true
    case c:CGroupBy => true
    case c:CLookup => true
    case _ => false
  }

  val extensions = new Extensions{}
  val normalizer = new BaseNormalizer{}
  import extensions._

  def flat(tp1: Map[String, Type], tp2: Type): Map[String, Type] =
    RecordCType(tp1).merge(tp2).attrs

  def wvar(mtp: Map[String, Type]): Variable = Variable.fresh(RecordCType(mtp))

  def getName(e: CExpr): String = e match {
    case InputRef(name, _) => name
    case FlatDict(e1) => getName(e1)
    case Variable(n, tp) => n
    case _ => sys.error(s"unsupported name extraction ${Printer.quote(e)}")
  }

  def unnest(e: CExpr)(implicit ctx: Ctx): CExpr = e match {

    /** Base case for unnesting algorithm: Rule C4 **/
    case Comprehension(e1, v, p, e2) if u.isEmpty && w.isEmpty && E.isEmpty =>
      val ne1 = AddIndex(e1, getName(e1)+"_index")
      val nv = Variable(v.name, ne1.tp.tp)
      unnest(e2)((u, nv.tp.attrs, Some(Select(ne1, nv, replace(p, nv))), tag))

    case c @ Comprehension(e1 @ Project(e0, f), v, p, e2) if !w.isEmpty =>
      assert(!E.isEmpty)

      if (u.isEmpty)
        unnest(e2)((u, flat((w - f), v.tp), Some(Unnest(E.get, wvar(w), f, v, p, Nil)), tag))
      else{
        val ne1 = AddIndex(E.get, f+"_index")
        val nv = Variable(v.name, ne1.tp.tp)
        val nw = flat(w - f, nv.tp)
        val nE = OuterUnnest(ne1, wvar(nw), f, v, p, Nil)
        unnest(e2)((u - f), flat(nw - f, v.tp.outer), Some(nE), tag)
      }

    case e1 @ CLookup(lbl, dict) => 
      assert(!E.isEmpty)
      val nv = Variable.fresh(e1.tp.tp)
      unnest(Comprehension(e1, nv, Constant(true), Sng(nv)))((u, w, E, tag))

    case Comprehension(e1 @ CLookup(Project(_, p1), dict), v1, filt, e2) => 
      assert(!E.isEmpty)
      val fields = ((w.keySet - p1) ++ (v1.tp.attrs.keySet - "_1"))
      val nv = wvar(w)
      val nv1 = Variable.freshFromBag(dict.tp)
	    val fdict = Select(dict, nv1, filt)
      val joinCond = Equals(Project(nv, p1), Project(nv1, "_1"))
      val nE = OuterJoin(E.get, nv, fdict, nv1, joinCond, fields.toList)
      unnest(e2)((u, flat(w, nv1.tp), Some(nE), tag))

    case Comprehension(e1:Comprehension, v, p, Constant(c)) => unnest(e1)((u, w, E, tag)) match {
      case Nest(e2, v2, keys2, values2, filt, nulls, ntag) => 
        Nest(e2, v2, keys2, Constant(c), values2, nulls, ntag)
      case _ => ???
    }

    case Comprehension(e1, v, cond, e2) if !w.isEmpty =>
      assert(!E.isEmpty)
      val name = getName(e1)
      val right = AddIndex(e1, name+"_index")
      val nv = Variable(v.name, right.tp.tp)

	    val (nw, nE) = 
        if (u.isEmpty) (flat(w, nv.tp), Join(E.get, wvar(w), Select(right, nv, Constant(true)), nv, cond, Nil))
        else (flat(w, nv.tp.outer), OuterJoin(E.get, wvar(w), Select(right, nv, Constant(true)), nv, cond, Nil))
      unnest(e2)((u, nw, Some(nE), tag))

    case s @ If(cnd, Sng(t @ Record(fs)), nextIf) =>
      handleLevel(fs, x => If(cnd, Sng(x), nextIf))((u, w, E, tag))

    case s @ Sng(t @ Record(fs)) if !w.isEmpty =>
      handleLevel(fs, identity)((u, w, E, tag))

    case Sng(v @ Variable(_, tp)) if !w.isEmpty => 
      val fs = tp.attrs.map(k => k._1 -> Project(v, k._1))
      handleLevel(fs, identity)((u, w, E, tag))

    case CReduceBy(e1, v1, keys, values) => unnest(e1)((u, w, E, tag)) match {
      case Nest(e2, v2, keys2, values2, filter, nulls, ctag) =>
        // adjust input for reduce operation
        val pattern = replace(extendIf(keys2, values2, v2), v2)
        val reduceInput = Projection(e2, v2, pattern, Nil)
        val nv1 = Variable(v2.name, reduceInput.tp)

        val crd = Reduce(reduceInput, nv1, keys2 ++ keys, values)
        val nv2 = wvar(v2.tp.attrs ++ crd.tp.attrs)
        val nr = Record((keys ++ values).map(k => k -> Project(nv2, k)).toMap)
        Nest(crd, nv2, keys2, nr, filter, nulls, ctag)

      case e2 => Reduce(e2, v1, keys, values)
    }

    case CGroupBy(e1, v1, keys, values, gname) => 
      val nE = unnest(e1)((u, w, E, gname)) 
      val nv = wvar(w)
      val tup = Record(values.map(v => v -> Project(nv, v)).toMap)
      Nest(nE, nv, keys, tup, Constant(true), (w.keySet -- u.keySet).toList, gname)

    case CDeDup(e1) => CDeDup(unnest(e1)((u, w, E, tag)))
    case Bind(x, e1, e2) => 
      Bind(x, unnest(e1)((u, w, E, tag)), unnest(e2)((u, w, E, tag)))
    case FlatDict(e1) => FlatDict(unnest(e1)((u, w, E, tag)))
    case GroupDict(e1) => GroupDict(unnest(e1)((u, w, E, tag)))
    case LinearCSet(exprs) => LinearCSet(exprs.map(unnest(_)((u, w, E, tag))))
    case CNamed(n, exp) => 
      CNamed(n, unnest(exp)((u, w, E, tag)))
    case _ => e

  }

  private def extendIf(ls: List[String], e: CExpr, v: Variable): CExpr = e match {
    case If(cond, Sng(Record(fs)), nextIf) => 
      val extendedAttrs = ls.map(l => l -> Project(v, l)).toMap
      val next = nextIf match { case Some(ni) => Some(extendIf(ls, ni, v)); case _ => None}
      If(cond, Sng(extendIf(ls, Record(fs), v)), next)
    case Record(ms) => 
      val extendedAttrs = ls.map(l => l -> Project(v, l)).toMap
      Record(extendedAttrs ++ ms)
    case _ => e
  }

  private def handleLevel(fs: Map[String, CExpr], exp: CExpr => CExpr)(implicit ctx: Ctx): CExpr = {
   fs.find(c => isNestedComp(c._2)) match {
    case Some((key, value)) =>
      val nE = unnest(value)((w, w, E, key))
      val nv = Variable.freshFromBag(nE.tp)
      val nr = Record(fs.map(f => f._1 -> replace(f._2, nv)) + (key -> Project(nv, key)))
      val nw = w + (key -> nv.tp)
      unnest(exp(Sng(nr)))((u, nw, Some(nE), tag))
    case y if u.isEmpty => 
      Projection(E.get, wvar(w), exp(Record(fs)), Nil)
    case _ => 
      val nv = wvar(w)
      val tup = exp(Record(fs))
      val rtup = replace(tup, nv)
      Nest(E.get, nv, u.keys.toList, rtup, Constant(true), (w.keySet -- u.keySet).toList, tag)
    }   
  }

  private def liftFilter(f: CExpr, e: CExpr): CExpr = f match {
    case Constant(true) => e
    case _ => e match {
      case Comprehension(e1, v, p, e2) => Comprehension(e1, v, normalizer.and(f, p), e2)
      case If(cnd, e1, e2) => If(normalizer.and(f, cnd), e1, e2)
      case Sng(e1) => If(f, e1, None)
      case _ => sys.error(s"unsupported expression $e")
    } 
  }
}
