package framework.nrc

import framework.utils.Utils.Symbol
import framework.common._

/**
  * Materialization of nested output queries
  */
trait BaseMaterialization {

  val INPUT_BAG_PREFIX: String = "IBag_"

  val INPUT_DICT_PREFIX: String = "IDict_"

  val MAT_BAG_PREFIX: String = "MBag_"

  val MAT_DICT_PREFIX: String = "MDict_"

  val LABEL_DOMAIN_PREFIX: String = "Dom_"

  val UNSHRED_PREFIX: String = "UDict_"

  def inputBagName(name: String): String = INPUT_BAG_PREFIX + name

  def inputDictName(name: String): String = INPUT_DICT_PREFIX + name

  def matBagName(name: String): String = MAT_BAG_PREFIX + name

  def matDictName(name: String): String = MAT_DICT_PREFIX + name

  def domainName(name: String): String = LABEL_DOMAIN_PREFIX + name

  def unshredDictName(name: String): String = UNSHRED_PREFIX + name

}

trait MaterializationContext extends BaseMaterialization with MaterializationDomain with Printer {
  this: MaterializeNRC =>

  class DictInfo(val dict: BagDictExpr, val ref: VarRef, val parent: Option[(BagDictExpr, String)])

  class LabelInfo(val isTopLevel: Boolean)

  class Context(private val dictCtx: Map[BagDictExpr, DictInfo],
                private val labelCtx: Map[LabelExpr, LabelInfo],
                private val scope: Map[String, VarDef]) {

    def this() = this(Map.empty, Map.empty, Map.empty)

    def addDict(dict: BagDictExpr, ref: VarRef, parent: Option[(BagDictExpr, String)]): Context =
      new Context(dictCtx + (dict -> new DictInfo(dict, ref, parent)), labelCtx, scope)

    def addDictAlias(dict: BagDictExpr, alias: BagDictExpr): Context =
      new Context(dictCtx + (alias -> dictCtx(dict)), labelCtx, scope)

    def contains(d: BagDictExpr): Boolean = dictCtx.contains(d)

    def isTopLevel(d: BagDictExpr): Boolean = dictCtx(d).parent.isEmpty

    def matVarRef(d: BagDictExpr): VarRef = dictCtx(d).ref

    def children(d: BagDictExpr): Map[String, BagDictExpr] = {
      val reference = dictCtx(d).dict
      dictCtx.values.collect {
        case i: DictInfo if i.parent.exists(_._1 == reference) =>
          i.parent.get._2 -> i.dict
      }.toMap
    }

    def addLabel(l: LabelExpr, isTopLevel: Boolean): Context =
      new Context(dictCtx, labelCtx + (l -> new LabelInfo(isTopLevel)), scope)

    def contains(l: LabelExpr): Boolean = labelCtx.contains(l)

    def isTopLevel(l: LabelExpr): Boolean = labelCtx(l).isTopLevel

    def addVarDef(v: VarDef): Context =
      new Context(dictCtx, labelCtx, scope + (v.name -> v))

    def removeVarDef(v: VarDef): Context =
      new Context(dictCtx, labelCtx, scope - v.name)

    def contains(n: String): Boolean = scope.contains(n)

    def varDef(n: String): VarDef = scope(n)

    def ++(c: Context): Context =
      new Context(dictCtx ++ c.dictCtx, labelCtx ++ c.labelCtx, scope ++ c.scope)

  }

  def initContext(p: ShredProgram, ctx: Context): Context =
    inputVars(p).filterNot {
      case v: BagDictVarRef => ctx.contains(v)
      case l: LabelVarRef => ctx.contains(l)
      case _ => false
    }.foldLeft (ctx) {
      case (acc, d: BagDictVarRef) =>
        addInputDict(d, acc, None)
      case (acc, l: LabelVarRef) =>
        acc.addLabel(l, isTopLevel = true)
      case (acc, _) => acc
    }

  private def addInputDict(d: BagDictVarRef, ctx: Context, parent: Option[(BagDictExpr, String)]): Context = {
    val matRef = if (parent.isEmpty)
      BagVarRef(inputBagName(d.name), d.tp.flatTp)
    else
      MatDictVarRef(inputDictName(d.name), MatDictType(d.tp.lblTp, d.tp.flatTp))
    val newCtx = ctx.addDict(d, matRef, parent)

    d.tp.dictTp.attrTps.foldLeft (newCtx) {
      case (acc, (n, t: BagDictType)) =>
        val child = BagDictVarRef(d.name + "_" + n, t)
        addInputDict(child, acc, Some(d -> n))
      case (acc, (_, EmptyDictType)) => acc
    }
  }

  def rewriteUsingContext(e0: Expr, ctx0: Context): (Expr, Context) = replace[Context](e0, ctx0, {
    case (v: VarRef, ctx) if ctx.contains(v.name) =>
      (VarRef(ctx.varDef(v.name)), ctx)

    case (Lookup(l, d), ctx) if ctx.isTopLevel(d) =>
      assert(ctx.isTopLevel(l))  // sanity check
      (ctx.matVarRef(d).asInstanceOf[BagVarRef], ctx)

    case (Lookup(l, d), ctx) =>
      assert(ctx.contains(d))  // sanity check
      val dict = ctx.matVarRef(d).asInstanceOf[MatDictExpr]
      val (lbl: LabelExpr, ctx1) = rewriteUsingContext(l, ctx)
      (MatDictLookup(lbl, dict), ctx1)

    case (BagLet(x, TupleDictProject(d), e2), ctx) =>
      assert(ctx.contains(d))  // sanity check
      val newCtx = ctx.children(d).foldLeft (ctx) {
        case (acc, (n, dict)) =>
          val tr = TupleDictVarRef(x.name, x.tp.asInstanceOf[TupleDictType])
          acc.addDictAlias(dict, BagDictProject(tr, n))
      }
      rewriteUsingContext(e2, newCtx)

    case (l: NewLabel, ctx) =>
      val (ps1, ctx1) =
        l.params.foldLeft (Map.empty[String, LabelParameter], ctx) {
          case ((acc, ctx), (_, VarRefLabelParameter(l: LabelExpr)))
            if ctx.contains(l) && ctx.isTopLevel(l) =>
            (acc, ctx)
          case ((acc, ctx), (_, VarRefLabelParameter(d: BagDictExpr))) =>
            assert(ctx.contains(d))  // sanity check
            (acc, ctx)
          case ((acc, ctx), (n, ProjectLabelParameter(d: BagDictExpr))) =>
            assert(ctx.contains(d))  // sanity check
            val ctx1 = ctx.addDictAlias(d, BagDictVarRef(n, d.tp))
            (acc, ctx1)
          case ((acc, ctx), (n, p0)) =>
            val (p1: LabelParameter, ctx1) = rewriteUsingContext(p0, ctx)
            (acc + (n -> p1), ctx1)
        }
      (NewLabel(ps1, l.id), ctx1)

    case (ForeachUnion(x, e1, e2), ctx) =>
      val (r1: BagExpr, ctx1) = rewriteUsingContext(e1, ctx)
      val xd = VarDef(x.name, r1.tp.tp)
      val ctx2 = ctx1.addVarDef(xd)
      val (r2: BagExpr, ctx3) = rewriteUsingContext(e2, ctx2)
      (ForeachUnion(xd, r1, r2), ctx3)

    case (l: Let, ctx) =>
      val (r1, ctx1) = rewriteUsingContext(l.e1, ctx)
      val xd = VarDef(l.x.name, r1.tp)
      val ctx2 = ctx1.addVarDef(xd)
      val (r2, ctx3) = rewriteUsingContext(l.e2, ctx2)
      (Let(xd, r1, r2), ctx3)

    case (x: ExtractLabel, ctx) =>
      val (l: LabelExpr, ctx1) = rewriteUsingContext(x.lbl, ctx)
      val ctx2 = l.tp.attrTps.foldLeft (ctx1) {
        case (acc, (n, t)) => acc.addVarDef(VarDef(n, t))
      }
      val (r, ctx3) = rewriteUsingContext(x.e, ctx2)
      (ExtractLabel(l, r), ctx3)
  })
}

trait Materialization extends MaterializationContext {
  this: MaterializeNRC with Optimizer with Printer =>

  class MaterializedProgram(val program: Program, val ctx: Context) {
    def ++(m: MaterializedProgram): MaterializedProgram =
      new MaterializedProgram(program ++ m.program, ctx ++ m.ctx)
  }

  def materialize(p: ShredProgram, ctx: Context = new Context, eliminateDomains: Boolean = false): MaterializedProgram = {
    Symbol.freshClear()

    // Create initial context with input dictionaries
    val initCtx = initContext(p, ctx)

    // Materialize each statement starting from empty program
    val emptyProgram = new MaterializedProgram(Program(), initCtx)
    p.statements.foldLeft (emptyProgram) { case (acc, s) =>
      val mat = materialize(s, acc.ctx, eliminateDomains)
      new MaterializedProgram(acc.program ++ mat.program, mat.ctx)
    }
  }

  private def materialize(a: ShredAssignment, ctx: Context, eliminateDomains: Boolean): MaterializedProgram =
    a.rhs match {
      case ShredExpr(l: NewLabel, d: BagDict) =>
        assert(l.tp == d.lblTp)   // sanity check
        materializeBagDict(d, a.name, ctx, None, eliminateDomains)
      case _ =>
        sys.error("Materialization not supported for "+ quote(a))
    }

  private def materializeBagDict(dict: BagDict,
                                 name: String,
                                 ctx: Context,
                                 parent: Option[(BagDictExpr, String)],
                                 eliminateDomains: Boolean
                                ): MaterializedProgram =
    dict match {
      case BagDict(tp, ShredUnion(b1, b2), TupleDictUnion(d1, d2)) =>
        val dict1 = BagDict(tp, b1, d1)
        val dict2 = BagDict(tp, b2, d2)
        materializeBagDict(dict1, name, ctx, parent, eliminateDomains) ++
          materializeBagDict(dict2, name, ctx, parent, eliminateDomains)

      case BagDict(lblTp, flat, tupleDict: TupleDictExpr) =>

        parent match {
          case None =>
            // 1. Eliminate symbolic dictionaries from flat expression
            val (flatBag: BagExpr, flatCtx) = rewriteUsingContext(flat, ctx)

            // 2. Create assignment statement
            val suffix = Symbol.fresh(name + "_")
            val bagRef = BagVarRef(matBagName(suffix), flatBag.tp)
            val stmt = Assignment(bagRef.name, flatBag)

            // 3. Extend context
            val dictCtx =
              flatCtx.addDict(dict, bagRef, parent)
                .addDictAlias(dict, BagDictVarRef(dictName(name), dict.tp))
                .addLabel(LabelVarRef(flatName(name), lblTp), isTopLevel = true)

            // 4. Materialize bag dictionary
            val program = new MaterializedProgram(Program(stmt), dictCtx)

            // 5. Materialize children if needed
            tupleDict match {
              case d: TupleDict =>
                program ++ materializeTupleDict(d, suffix, dictCtx, dict, eliminateDomains)
              case _ =>
                program
            }

          case Some((parentDict, field)) =>
            // Get new label type for flat lambda expression
            val matDictTp = ctx.matVarRef(parentDict).tp
            val newLabelTp = if (ctx.isTopLevel(parentDict))
              matDictTp.asInstanceOf[BagType].tp(field).asInstanceOf[LabelType]
            else
              matDictTp.asInstanceOf[MatDictType].valueTp.tp(field).asInstanceOf[LabelType]

            val eliminated = if (eliminateDomains) eliminateDomain(newLabelTp, flat, ctx) else None

            eliminated match {
              case None =>
                // 1. Create label domain
                val domain = createLabelDomain(ctx.matVarRef(parentDict), ctx.isTopLevel(parentDict), field)
                val domainRef = BagVarRef(domain.name, domain.rhs.tp.asInstanceOf[BagType])

                // 2. Create dictionary bag expression
                val tpl = TupleVarRef(Symbol.fresh(name = "l"), domainRef.tp.tp)
                val lbl = LabelProject(tpl, LABEL_ATTR_NAME)
                val dictBag =
                  ForeachUnion(tpl, domainRef,
                    addOutputField(KEY_ATTR_NAME -> lbl, BagExtractLabel(lbl, flat)))
                val (newDictBag: BagExpr, newCtx) =
                  rewriteUsingContext(dictBag, ctx)

                // 3. Create assignment statement
                val suffix = Symbol.fresh(name + "_")
                val matDict = BagToMatDict(newDictBag)
                val matDictRef = MatDictVarRef(matDictName(suffix), matDict.tp)
                val stmt = Assignment(matDictRef.name, matDict)

                // 4. Extend context
                val dictCtx = newCtx.addDict(dict, matDictRef, parent)

                // 5. Materialize bag dictionary
                val program = new MaterializedProgram(Program(domain, stmt), dictCtx)

                // 6. Materialize children if needed
                tupleDict match {
                  case d: TupleDict =>
                    program ++ materializeTupleDict(d, suffix, dictCtx, dict, eliminateDomains)
                  case _ =>
                    program
                }

              case Some((flatBag, flatCtx)) =>
                // 2. Create assignment statement
                val suffix = Symbol.fresh(name + "_")
                val matDict = BagToMatDict(flatBag)
                val matDictRef = MatDictVarRef(matDictName(suffix), matDict.tp)
                val stmt = Assignment(matDictRef.name, matDict)

                // 3. Extend context
                val dictCtx = flatCtx.addDict(dict, matDictRef, parent)

                // 4. Materialize bag dictionary
                val program = new MaterializedProgram(Program(stmt), dictCtx)

                // 5. Materialize children if needed
                tupleDict match {
                  case d: TupleDict =>
                    program ++ materializeTupleDict(d, suffix, dictCtx, dict, eliminateDomains)
                  case _ =>
                    program
                }
            }
        }

      case _ =>
        sys.error("[materializeBagDict] Unexpected dictionary type: " + dict)
    }

  private def materializeTupleDict(dict: TupleDict,
                                   name: String,
                                   ctx: Context,
                                   parentDict: BagDict,
                                   eliminateDomains: Boolean
                                  ): MaterializedProgram =
    dict.fields.foldLeft (new MaterializedProgram(Program(), ctx)) {
      case (acc, (n: String, d: BagDict)) =>
        // Materialize child dictionary
        acc ++ materializeBagDict(d, name + "_" + n, ctx, Some(parentDict -> n), eliminateDomains)
      case (acc, (_, EmptyDict)) =>
        acc
      case (_, (_, d)) =>
        sys.error("[materializeTupleDict] Unsupported dictionary type: " + d)
    }

  def unshred(p: ShredProgram, ctx: Context): Program = {
    Symbol.freshClear()
    Program(p.statements.flatMap(unshred(_, ctx).statements))
  }

  private def unshred(a: ShredAssignment, ctx: Context): Program = a.rhs match {
    case ShredExpr(l: NewLabel, d: BagDict) =>
      assert(l.tp == d.lblTp)   // sanity check
      val (p, b) = unshredBagDict(d, l, ctx, isTopLevel = true)
      Program(p.statements :+ Assignment(a.name, b))
    case _ =>
      sys.error("Unshredding not supported for " + quote(a))
  }

  private def unshredBagDict(dict: BagDict, lbl: LabelExpr, ctx: Context, isTopLevel: Boolean): (Program, BagExpr) =
    dict match {
      case BagDict(tp, ShredUnion(b1, b2), TupleDictUnion(d1, d2)) =>
        val dict1 = BagDict(tp, b1, d1)
        val dict2 = BagDict(tp, b2, d2)
        val (prog1, bag1) = unshredBagDict(dict1, lbl, ctx, isTopLevel)
        val (prog2, bag2) = unshredBagDict(dict2, lbl, ctx, isTopLevel)
        (prog1 ++ prog2, Union(bag1, bag2))

      case BagDict(_, _, tupleDict: TupleDict) if isTopLevel && tupleDict.isEmpty =>
        val bagRef = ctx.matVarRef(dict).asInstanceOf[BagVarRef]
        (Program(), bagRef)

      case BagDict(_, _, tupleDict: TupleDict) if isTopLevel =>
        val bagDictRef = ctx.matVarRef(dict).asInstanceOf[BagVarRef]
        val tupleRef = TupleVarRef(Symbol.fresh(), bagDictRef.tp.tp)

        // 1. Unshred children
        val (childProgram, childTuple) = unshredTupleDict(tupleDict, tupleRef, ctx)

        // 2. Compute nested object
        val nestedBag = ForeachUnion(tupleRef.varDef, bagDictRef, Singleton(childTuple))

        (Program(childProgram.statements), nestedBag)

      case BagDict(_, _, tupleDict: TupleDict) if tupleDict.isEmpty =>
        val matDictRef = ctx.matVarRef(dict).asInstanceOf[MatDictVarRef]
        (Program(), MatDictLookup(lbl, matDictRef))

      case BagDict(_, _, tupleDict: TupleDict) =>
        val matDictRef = ctx.matVarRef(dict).asInstanceOf[MatDictVarRef]
        val kvDict = MatDictToBag(matDictRef)
        val kvRef = TupleVarRef(Symbol.fresh(name = "kv"), kvDict.tp.tp)

        // 1. Unshred children
        val (childProgram, childTuple) = unshredTupleDict(tupleDict, kvRef, ctx)

        // 2. Compute nested object
        val key = LabelProject(kvRef, KEY_ATTR_NAME)
        val kvPairsNested =
          BagToMatDict(
            ForeachUnion(kvRef.varDef, kvDict,
              Singleton(Tuple(childTuple.fields + (KEY_ATTR_NAME -> key)))))

        // 3. Materialize unshredded dictionary
        val uName = matDictRef.name.replace(MAT_DICT_PREFIX, UNSHRED_PREFIX)
        val uDict = MatDictVarRef(uName, kvPairsNested.tp)
        val uStmt = Assignment(uDict.name, kvPairsNested)

        (Program(childProgram.statements :+ uStmt), MatDictLookup(lbl, uDict))

      case _ =>
        sys.error("[unshredBagDict] Unexpected dictionary type: " + dict)
    }

  private def unshredTupleDict(dict: TupleDict, tuple: TupleVarRef, ctx: Context): (Program, Tuple) =
    dict.fields.foldLeft (Program(), Tuple()) {
      case ((prog, tpl), (n, d: BagDict)) =>
        val lbl = LabelProject(tuple, n)
        val (p1, b1) = unshredBagDict(d, lbl, ctx, isTopLevel = false)
        (prog ++ p1, Tuple(tpl.fields + (n -> b1)))

      case ((prog, tpl), (n, EmptyDict)) =>
        (prog, Tuple(tpl.fields + (n -> tuple(n))))

      case (_, (n, d)) =>
        sys.error("Unexpected dictionary " + n + " = " + d)
    }

}
