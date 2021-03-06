package framework.nrc

import framework.common.{BagType, LabelType, MatDictType, OpEq, TupleType, VarDef}
import framework.utils.Utils.Symbol

trait MaterializationDomain extends Printer {
  this: MaterializeNRC with MaterializationContext =>

  def eliminateDomain(lblTp: LabelType, b: BagExpr, ctx: Context): Option[(BagExpr, Context)] = {
    val newCtx = lblTp.attrTps.foldLeft (ctx) {
      case (acc, (n, t)) => acc.addVarDef(VarDef(n, t))
    }
    val (flatBag: BagExpr, flatCtx) = rewriteUsingContext(b, newCtx)
    val iv = inputVars(flatBag).filterNot(x =>
      x.tp.isInstanceOf[MatDictType] || x.tp.isInstanceOf[BagType])

    val flatBag2 = eliminateDomain(flatBag, iv)
    val iv2 = inputVars(flatBag2).filterNot(x =>
      x.tp.isInstanceOf[MatDictType] || x.tp.isInstanceOf[BagType])

    if (iv2.isEmpty) Some(flatBag2, flatCtx) else None
  }

  private def eliminateDomain(b: BagExpr, iv: Set[VarRef]): BagExpr =
    replace(b, {
      // If hoisting
      case ForeachUnion(x, b1,
        BagIfThenElse(PrimitiveCmp(OpEq, p1: Project, p2: VarRef), b2, None))
          if !iv.contains(p1.tuple) && iv.contains(p2) =>

        val lbl = NewLabel(Map(p2.name -> ProjectLabelParameter(p1)))
        ForeachUnion(x, b1,
          addOutputField(KEY_ATTR_NAME -> lbl, eliminateDomain(b2, iv)))

      // If hoisting
      case ForeachUnion(x, b1,
        BagIfThenElse(PrimitiveCmp(OpEq, p1: VarRef, p2: Project), b2, None))
          if !iv.contains(p2.tuple) && iv.contains(p1) =>
        val lbl = NewLabel(Map(p1.name -> ProjectLabelParameter(p2)))
        ForeachUnion(x, b1,
          addOutputField(KEY_ATTR_NAME -> lbl, eliminateDomain(b2, iv)))

      // Dictionary iteration
      case ForeachUnion(x, MatDictLookup(l: VarRef, d), b2) if iv.contains(l) =>
        val dictBag = MatDictToBag(d)
        val kv = TupleVarRef(Symbol.fresh(name = "kv"), dictBag.tp.tp)
        val lbl = LabelProject(kv, KEY_ATTR_NAME)
        val attrs = kv.tp.attrTps.keys.filter(_ != KEY_ATTR_NAME).map(k => k -> kv(k)).toMap
        val newLabel = NewLabel(Map(l.name -> ProjectLabelParameter(lbl)))
        ForeachUnion(kv, dictBag,
          BagLet(x, Tuple(attrs),
            addOutputField(KEY_ATTR_NAME -> newLabel, eliminateDomain(b2, iv))))

      // Aggregation
      case GroupByKey(e, ks, vs, n) =>
        val e2 = eliminateDomain(e, iv)
        if (e2.tp.tp.attrTps.contains(KEY_ATTR_NAME))
          GroupByKey(e2, (KEY_ATTR_NAME :: ks).distinct, vs, n)
        else
          GroupByKey(e2, ks, vs, n)

      // Aggregation
      case ReduceByKey(e, ks, vs) =>
        val e2 = eliminateDomain(e, iv)
        if (e2.tp.tp.attrTps.contains(KEY_ATTR_NAME))
          ReduceByKey(e2, (KEY_ATTR_NAME :: ks).distinct, vs)
        else
          ReduceByKey(e2, ks, vs)

    }).asInstanceOf[BagExpr]

  @deprecated
  def eliminateDomainOld(lblTp: LabelType, b: BagExpr, ctx: Context): Option[(BagExpr, Context)] = {
    val newCtx = lblTp.attrTps.foldLeft (ctx) {
      case (acc, (n, t)) => acc.addVarDef(VarDef(n, t))
    }
    val (flatBag: BagExpr, flatCtx) = rewriteUsingContext(b, newCtx)
    eliminateDomainOld(flatBag).map(_ -> flatCtx)
  }

  @deprecated
  private def eliminateDomainOld(b: BagExpr): Option[BagExpr] = {
    val iv = inputVars(b).filterNot(x =>
      x.tp.isInstanceOf[MatDictType] || x.tp.isInstanceOf[BagType])
    eliminateDomainIfHoisting(b, iv) orElse
      eliminateDomainDictIteration(b, iv) orElse
        eliminateDomainAggregation(b, iv)
  }

  @deprecated
  private def eliminateDomainIfHoisting(b: BagExpr, iv: Set[VarRef]): Option[BagExpr] = b match {
    case ForeachUnion(x, b1, BagIfThenElse(c, b2, None)) => c match {
      case PrimitiveCmp(OpEq, p1: Project, p2: VarRef)
        if !iv.contains(p1.tuple) && iv.contains(p2) =>
        val lbl = NewLabel(Map(p2.name -> ProjectLabelParameter(p1)))
        Some(ForeachUnion(x, b1, addOutputField(KEY_ATTR_NAME -> lbl, b2)))

      case PrimitiveCmp(OpEq, p1: VarRef, p2: Project)
        if !iv.contains(p2.tuple) && iv.contains(p1) =>
        val lbl = NewLabel(Map(p1.name -> ProjectLabelParameter(p2)))
        Some(ForeachUnion(x, b1, addOutputField(KEY_ATTR_NAME -> lbl, b2)))

      case _ => None
    }
    case _ => None
  }

  @deprecated
  private def eliminateDomainDictIteration(b: BagExpr, iv: Set[VarRef]): Option[BagExpr] = b match {
    case ForeachUnion(x, MatDictLookup(l: VarRef, d), b2) if iv == Set(l) =>
      val dictBag = MatDictToBag(d)
      val kv = TupleVarRef(Symbol.fresh(name = "kv"), dictBag.tp.tp)
      val lbl = LabelProject(kv, KEY_ATTR_NAME)
      val attrs = kv.tp.attrTps.keys.filter(_ != KEY_ATTR_NAME).map(k => k -> kv(k)).toMap
      val newLabel = NewLabel(Map(l.name -> ProjectLabelParameter(lbl)))
      Some(
        ForeachUnion(kv, dictBag,
          BagLet(x, Tuple(attrs), addOutputField(KEY_ATTR_NAME -> newLabel, b2))))
    case  _ => None
  }

  @deprecated
  private def eliminateDomainAggregation(b: Expr, iv: Set[VarRef]): Option[BagExpr] = b match {
    case GroupByKey(e, ks, vs, n) =>
      eliminateDomainOld(e) match {
        case Some(e2) => Some(GroupByKey(e2, KEY_ATTR_NAME :: ks, vs, n))
        case _ => None
      }
    case ReduceByKey(e, ks, vs) =>
      eliminateDomainOld(e) match {
        case Some(e2) => Some(ReduceByKey(e2, KEY_ATTR_NAME :: ks, vs))
        case _ => None
      }
    case _ => None
  }

  def createLabelDomain(varRef: VarRef, topLevel: Boolean, field: String): Assignment = {
    val domain = if (topLevel) {
      val bagVarRef = varRef.asInstanceOf[BagVarRef]
      val x = TupleVarRef(Symbol.fresh(), bagVarRef.tp.tp)
      val lbl = LabelProject(x, field)
      DeDup(
        ForeachUnion(x, bagVarRef,
          Singleton(Tuple(LABEL_ATTR_NAME -> lbl)))
      )
    }
    else {
      val matDictVarRef = varRef.asInstanceOf[MatDictVarRef]
      val kvTp =
        TupleType(
          matDictVarRef.tp.valueTp.tp.attrTps +
            (KEY_ATTR_NAME -> matDictVarRef.tp.keyTp))

      val kv = TupleVarRef(Symbol.fresh(name = "kv"), kvTp)
      val lbl = LabelProject(kv, field)
      DeDup(
        ForeachUnion(kv, MatDictToBag(matDictVarRef),
          Singleton(Tuple(LABEL_ATTR_NAME -> lbl)))
      )
    }

    val bagRef = BagVarRef(domainName(Symbol.fresh(field + "_")), domain.tp)
    Assignment(bagRef.name, domain)
  }

}
