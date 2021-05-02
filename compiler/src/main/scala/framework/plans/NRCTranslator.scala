package framework.plans

import framework.common._
import framework.nrc.{Printer => NRCPrinter, MaterializeNRC}

/** Translate NRC from standard and shredded pipeline into 
  * a comprehension language that supports normalization. 
  */
trait NRCTranslator extends MaterializeNRC with NRCPrinter {
  val compiler = new BaseCompiler{}
  import compiler._

  def translate(e: Type): Type = e match {
    case MatDictType(lbl, dict) => 
      MatDictCType(translate(lbl).asInstanceOf[LabelType], translate(dict).asInstanceOf[BagCType])
    case BagType(t) => BagCType(translate(t))
    case LabelType(fs) => LabelType(fs.map(f => f._1 -> translate(f._2)))
    case TupleType(fs) if fs.isEmpty => EmptyCType
    case TupleType(fs) => RecordCType(fs.map(f => f._1 -> translate(f._2)))
    case _ => e
  }
  
  /** Translate condition into corresponding calculus expression. 
    * 
    * @param e conditional NRC expression
    * @return corresponding calculus of input expression
    */
  def translate(e: CondExpr): CExpr = e match {
    case cmp: Cmp => cmp.op match {
      case OpEq => 
        compiler.equals(translate(cmp.e1), translate(cmp.e2))
      case OpNe => not(compiler.equals(translate(cmp.e1), translate(cmp.e2)))
      case OpGt => (translate(cmp.e1), translate(cmp.e2)) match {
        case (te1 @ Constant(_), te2:CExpr) =>  lt(te2, te1) // 5 > x
        case (te1:CExpr, te2:CExpr) => gt(te1, te2)
      }
      case OpGe => (translate(cmp.e1), translate(cmp.e2)) match {
        case (te1 @ Constant(_), te2:CExpr) => lte(te2, te1)
        case (te1:CExpr, te2:CExpr) => gte(te1, te2)
      }
    }
    case And(e1, e2) => and(translate(e1), translate(e2))
    case Or(e1, e2) => or(translate(e1), translate(e2))
    case Not(e1) => not(translate(e1))
  }

  def translate(v: VarDef): CExpr = Variable(v.name, translate(v.tp))
  def translateVar(v: VarRef): CExpr = translate(v.varDef)
  
  /** Translate NRC expressions into calculus style suitable for normalizatin
    *
    * @param e input NRC expression or sequence of materialized NRC expressions
    * @return corresponding calculus 
    */
  def translate(e: Expr): CExpr = e match {
    case c: Const => constant(c.v)
    case v: VarRef => translateVar(v)
    case ArithmeticExpr(op, e1, e2) => MathOp(op, translate(e1), translate(e2))
    case Singleton(Tuple(fs)) if fs.isEmpty => emptysng
    case Singleton(e1) => sng(translate(e1))
    case Tuple(fs) if fs.isEmpty => unit
    case Tuple(fs) => record(fs.map(f => f._1 -> translate(f._2)))
    case p: Project => project(translate(p.tuple), p.field)
	  case Udf(n, e1, tp) => CUdf(n, translate(e1), translate(tp))
	  case ift: IfThenElse => ift.e2 match {
      case Some(a) => ifthen(translate(ift.cond), translate(ift.e1), Option(translate(a)))
      case _ => ifthen(translate(ift.cond), translate(ift.e1))
    }
    case Union(e1, e2) => merge(translate(e1), translate(e2))
    case ForeachUnion(x, e1, e2) => translate(e2) match {
	    case If(cond, e3, e4 @ None) => 
        Comprehension(translate(e1), translate(x).asInstanceOf[Variable], cond, e3)
	    case te2 => 
        Comprehension(translate(e1), translate(x).asInstanceOf[Variable], constant(true), te2)
    }
    case l:Let => Bind(translate(l.x), translate(l.e1), translate(l.e2))
    case GroupByKey(be, keys, values, gname) => 
      val bagExpr = translate(be)
      val v = Variable.freshFromBag(bagExpr.tp)
      CGroupBy(bagExpr, v, keys, values, gname)
    case ReduceByKey(be, keys, values) => 
      val bagExpr = translate(be)
      val v = Variable.freshFromBag(bagExpr.tp)
      CReduceBy(bagExpr, v, keys, values) 
    case v: VarRefLabelParameter => translateVar(v.e)
    case l: NewLabel =>
      label(l.params.map {
        case (n, v2: VarRefLabelParameter) => n -> translateVar(v2.e)
        case (n, v2: ProjectLabelParameter) => n -> translate(v2.e)
      })
    case e:ExtractLabel =>  
      val lbl = translate(e.lbl)
      val bindings = e.lbl.tp.attrTps.map(k => 
        Variable(k._1, translate(k._2)) -> project(lbl, k._1)).toSeq
      bindings.foldRight(translate(e.e))((cur, acc) => Bind(cur._1, cur._2, acc))
    case Lookup(lbl, dict) => CLookup(translate(lbl), translate(dict)) 
    case Count(e1) => comprehension(translate(e1), x => constant(true), (i: CExpr) => constant(1.0))
    case DeDup(e1) => CDeDup(translate(e1)) 
    case Get(e1) => CGet(translate(e1))
    
    // new flexible dictionary handling
    case MatDictLookup(lbl, dict) => CLookup(translate(lbl), translate(dict))
    case MatDictToBag(bd) => FlatDict(translate(bd))
    case BagToMatDict(bd) => GroupDict(translate(bd))

    // catch existing dictionary types that may persist in NRC, 
    // these are likely deprecated cases that no longer exist with new 
    // materialization method
    case EmptyDict => emptydict
    case BagDict(ltp, flat, dict) => BagCDict(ltp, translate(flat), translate(dict))
    case TupleDict(fs) => TupleCDict(fs.map(f => f._1 -> translate(f._2)))
    case TupleDictProject(dict) => project(translate(dict), "_2")
    case d: DictUnion => DictCUnion(translate(d.dict1), translate(d.dict2))

    case _ => sys.error("cannot translate "+quote(e))
  }

  def translate(a: Assignment): CExpr = CNamed(a.name, translate(a.rhs))
  def translate(p: Program): LinearCSet = LinearCSet(p.statements.map(translate))

}
