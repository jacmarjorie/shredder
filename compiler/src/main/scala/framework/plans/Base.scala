package framework.plans

import framework.common._
import scala.collection.mutable.HashMap
import framework.utils.Utils.ind

/**
  * Base structure for comprehension compilers.
  */
trait Base {
  type Rep
  def inputref(x: String, tp:Type): Rep
  def input(x: List[Rep]): Rep 
  def constant(x: Any): Rep
  def udf(n: String, e1: Rep, tp: Type): Rep
  def emptysng: Rep
  def unit: Rep 
  def cnull: Rep
  def sng(x: Rep): Rep
  def get(x: Rep): Rep
  def tuple(fs: List[Rep]): Rep
  def record(fs: Map[String, Rep]): Rep
  def label(fs: Map[String, Rep]): Rep 
  def mathop(op: OpArithmetic, e1: Rep, e2: Rep): Rep
  def equals(e1: Rep, e2: Rep): Rep
  def lt(e1: Rep, e2: Rep): Rep
  def gt(e1: Rep, e2: Rep): Rep
  def lte(e1: Rep, e2: Rep): Rep
  def gte(e1: Rep, e2: Rep): Rep
  def and(e1: Rep, e2: Rep): Rep
  def not(e1: Rep): Rep
  def or(e1: Rep, e2: Rep): Rep
  def project(e1: Rep, field: String): Rep
  def ifthen(cond: Rep, e1: Rep, e2: Option[Rep] = None): Rep
  def merge(e1: Rep, e2: Rep): Rep
  def comprehension(e1: Rep, p: Rep => Rep, e: Rep => Rep): Rep
  def dedup(e1: Rep): Rep
  def bind(e1: Rep, e: Rep => Rep): Rep 
  def groupby(e1: Rep, g: List[String], v: List[String], gname: String): Rep
  def reduceby(e1: Rep, g: List[String], v: List[String]): Rep
  def named(n: String, e: Rep): Rep
  def linset(e: List[Rep]): Rep

  // NRC lbl+lambda
  def lookup(lbl: Rep, dict: Rep): Rep
  def emptydict: Rep
  def bagdict(lblTp: LabelType, flat: Rep, dict: Rep): Rep
  def tupledict(fs: Map[String, Rep]): Rep
  def dictunion(d1: Rep, d2: Rep): Rep

  // plan operators
  def select(x: Rep, p: Rep => Rep): Rep
  def addindex(in: Rep, name: String): Rep 
  def projection(in: Rep, filter: Rep => Rep, fields: List[String]): Rep
  def unnest(in: Rep, path: String, filter: Rep => Rep, fields: List[String]): Rep
  def ounnest(in: Rep, path: String, filter: Rep => Rep, fields: List[String]): Rep
  def join(left: Rep, right: Rep, cond: (Rep, Rep) => Rep, fields: List[String]): Rep
  def ojoin(left: Rep, right: Rep, cond: (Rep, Rep) => Rep, fields: List[String]): Rep
  def nest(in: Rep, key: List[String], value: Rep => Rep, filter: Rep => Rep, nulls: List[String], ctag: String): Rep
  def reduce(in: Rep, key: List[String], values: List[String]): Rep

  // bag to dict casts
  def flatdict(e1: Rep): Rep
  def groupdict(e1: Rep): Rep
}

/** Base compiler for string types.
  * This works bottom-up and is not often used. 
  * The primary printer, which works top-down, is in Printer.scala
  */
trait BaseStringify extends Base{
  type Rep = String
  def inputref(x: String, tp: Type): Rep = x
  def input(x: List[Rep]): Rep = s"{${x.mkString(",")}}"
  def udf(n: String, e1: Rep, tp: Type): Rep = s"$n($e1)"
  def constant(x: Any): Rep = x.toString
  def emptysng: Rep = "{}"
  def cnull: Rep = "null"
  def unit: Rep = "()"
  def sng(x: Rep): Rep = s"{ $x }"
  def get(x: Rep): Rep = s"get($x)"
  def tuple(fs: List[Rep]) = s"(${fs.mkString(",")})"
  def record(fs: Map[String, Rep]): Rep = 
    s"(${fs.map(f => f._1 + " := " + f._2).mkString(",")})"
  def label(fs: Map[String, Rep]): Rep = 
    s"Label(${fs.map(f => f._1 + " := " + f._2).mkString(",")})"
  def mathop(op: OpArithmetic, e1: Rep, e2: Rep): Rep = s"$e1 $op $e2"
  def equals(e1: Rep, e2: Rep): Rep = s"${e1} == ${e2}"
  def lt(e1: Rep, e2: Rep): Rep = s"${e1} < ${e2}"
  def gt(e1: Rep, e2: Rep): Rep = s"${e1} > ${e2}"
  def lte(e1: Rep, e2: Rep): Rep = s"${e1} <= ${e2}"
  def gte(e1: Rep, e2: Rep): Rep = s"${e1} >= ${e2}"
  def and(e1: Rep, e2: Rep): Rep = s"${e1}, ${e2}"
  def not(e1: Rep): Rep = s"!(${e1})"
  def or(e1: Rep, e2: Rep): Rep = s"${e1} || ${e2}"
  def project(e1: Rep, field: String): Rep = s"${e1}.${field}"
  def ifthen(cond: Rep, e1: Rep, e2: Option[Rep]): Rep = e2 match {
    case Some(a) => s"if (${cond}) then ${e1} else ${a}"
    case _ => s"if (${cond}) then ${e1}"
  }
  def merge(e1: Rep, e2: Rep): Rep = s"${e1} U ${e2}"
  def bind(e1: Rep, e: Rep => Rep): Rep = {
    val x = Variable.fresh(StringType)
    s"{ ${e(x.quote)} | ${x.quote} := ${e1} }"
  }
  def groupby(e1: Rep, g: List[String], v: List[String], gname: String): Rep = {
    val v2 = Variable.fresh(StringType)
    s"""(${e1}).groupBy(${g.mkString(",")}, ${v.mkString(",")}, "$gname")"""
  }
  def reduceby(e1: Rep, g: List[String], v: List[String]): Rep = {
    val v2 = Variable.fresh(StringType)
    s"(${e1}).reduceBy(${g.mkString(",")}, ${v.mkString(",")})"
  }
  def comprehension(e1: Rep, p: Rep => Rep, e: Rep => Rep): Rep = { 
    val x = Variable.fresh(StringType)
    p(x.quote) match {
      case "true" => s"{ ${e(x.quote)} | ${x.quote} <- ${e1} }"
      case px => s"{ ${e(x.quote)} | ${x.quote} <- ${e1}, ${px} }"
    } 
  }
  def dedup(e1: Rep): Rep = s"DeDup(${e1})"
  def named(n: String, e: Rep): Rep = s"${n} := ${e}"
  def linset(e: List[Rep]): Rep = e.mkString("\n\n")

  def lookup(lbl: Rep, dict: Rep): Rep = s"Lookup(${lbl}, ${dict})"
  def emptydict: Rep = s"Nil"
  def bagdict(lblTp: LabelType, flat: Rep, dict: Rep): Rep = s"(${lblTp} -> ${flat}, ${dict})"
  def tupledict(fs: Map[String, Rep]): Rep =
    s"(${fs.map(f => f._1 + " := " + f._2).mkString(",")})"
  def dictunion(d1: Rep, d2: Rep): Rep = s"${d1} U ${d2}"
  
  def select(x: Rep, p: Rep => Rep): Rep = { 
    val v = Variable.fresh(StringType).quote
    s""" | SELECT[ ${p(v)} ]( ${x} )""".stripMargin
  }
  def addindex(in: Rep, name: String): Rep = "/** Top-down, see Printer.scala **/"
  def projection(in: Rep, filter: Rep => Rep, fields: List[String]): Rep = ""
  def unnest(in: Rep, path: String, filter: Rep => Rep, fields: List[String]): Rep = "/** Top-down, see Printer.scala **/"
  def ounnest(in: Rep, path: String, filter: Rep => Rep, fields: List[String]): Rep = "/** Top-down, see Printer.scala **/"
  def join(left: Rep, right: Rep, cond: (Rep, Rep) => Rep, fields: List[String]): Rep = "/** Top-down, see Printer.scala **/"
  def ojoin(left: Rep, right: Rep, cond: (Rep, Rep) => Rep, fields: List[String]): Rep = "/** Top-down, see Printer.scala **/"
  def nest(in: Rep, key: List[String], value: Rep => Rep, filter: Rep => Rep, nulls: List[String], ctag: String): Rep = "/** Top-down, see Printer.scala **/"
  def reduce(in: Rep, key: List[String], values: List[String]): Rep = ""

  def flatdict(e1: Rep): Rep = s"FLAT($e1)"
  def groupdict(e1: Rep): Rep = s"GROUP($e1)"
}

/** Base compiler for generating nodes defined in the 
  * comprehension representation (Expr.scala)
  */
trait BaseCompiler extends Base {

  val ext = new Extensions{}
  type Rep = CExpr 

  def inputref(x: String, tp: Type): Rep = InputRef(x, tp)
  def input(x: List[Rep]): Rep = Input(x)
  def constant(x: Any): Rep = Constant(x)
  def udf(n: String, e1: Rep, tp: Type): Rep = CUdf(n, e1, tp)
  def emptysng: Rep = EmptySng
  def cnull: Rep = Null
  def unit: Rep = CUnit
  def sng(x: Rep): Rep = Sng(x)
  def get(x: Rep): Rep = CGet(x)
  def tuple(fs: List[Rep]): Rep = Tuple(fs)
  def record(fs: Map[String, Rep]): Rep = Record(fs)
  def label(fs: Map[String, Rep]): Rep = Label(fs)
  def mathop(op: OpArithmetic, e1: Rep, e2: Rep): Rep = MathOp(op, e1, e2)
  def equals(e1: Rep, e2: Rep): Rep = Equals(e1, e2)
  def lt(e1: Rep, e2: Rep): Rep = Lt(e1, e2)
  def gt(e1: Rep, e2: Rep): Rep = Gt(e1, e2)
  def lte(e1: Rep, e2: Rep): Rep = Lte(e1, e2)
  def gte(e1: Rep, e2: Rep): Rep = Gte(e1, e2)
  def and(e1: Rep, e2: Rep): Rep = And(e1, e2)
  def not(e1: Rep): Rep = Not(e1)
  def or(e1: Rep, e2: Rep): Rep = Or(e1, e2)
  def project(e1: Rep, f: String): Rep = e1 match {
    case InputRef(n, _) if n.contains("Dict") => e1
    case _ => Project(e1, f)
  }
  def ifthen(cond: Rep, e1: Rep, e2: Option[Rep]): Rep = If(cond, e1, e2)
  def merge(e1: Rep, e2: Rep): Rep = Merge(e1, e2)
  def comprehension(e1: Rep, p: Rep => Rep, e: Rep => Rep): Rep = {
    val v = Variable.fresh(e1.tp.asInstanceOf[BagCType].tp)
    Comprehension(e1, v, p(v), e(v))
  }
  def dedup(e1: Rep): Rep = {
    assert(!e1.tp.isInstanceOf[PrimitiveType])
    CDeDup(e1)
  }
  def bind(e1: Rep, e: Rep => Rep): Rep = {
      val v = Variable.fresh(e1.tp)
      Bind(v, e1, e(v)) 
  }
  def groupby(e1: Rep, g: List[String], v: List[String], gname: String): Rep = {
    val v2 = Variable.freshFromBag(e1.tp)
    CGroupBy(e1, v2, g, v, gname) 
  }
  def reduceby(e1: Rep, g: List[String], v: List[String]): Rep = {
    val v2 = Variable.freshFromBag(e1.tp)
    CReduceBy(e1, v2, g, v) 
  }
  def named(n: String, e: Rep): Rep = CNamed(n, e)
  def linset(e: List[Rep]): Rep = LinearCSet(e)

  def lookup(lbl: Rep, dict: Rep): Rep = CLookup(lbl, dict)
  def emptydict: Rep = EmptyCDict
  def bagdict(lblTp: LabelType, flat: Rep, dict: Rep): Rep = BagCDict(lblTp, flat, dict)
  def tupledict(fs: Map[String, Rep]): Rep = TupleCDict(fs)
  def dictunion(d1: Rep, d2: Rep): Rep = DictCUnion(d1, d2)

  def select(x: Rep, p: Rep => Rep): Rep = {
    val v = Variable.freshFromBag(x.tp)
    p(v) match {
      case Constant(true) => x
      case pv => Select(x, v, pv)
    }
  }

  def addindex(in: Rep, name: String): Rep = in match {
    case AddIndex(in1, _) => AddIndex(in, name)
    case _ => AddIndex(in, name)  
  }
  def projection(in: Rep, filter: Rep => Rep, fields: List[String]): Rep = {
    val v1 = Variable.freshFromBag(in.tp)
    val nr = filter(v1)
    val fs = ext.collect(nr)
    Projection(in, v1, nr, fs.toList)
  }
  def unnest(in: Rep, path: String, filter: Rep => Rep, fields: List[String]): Rep = {
    val v = Variable.freshFromBag(in.tp)
    val v2 = Variable.freshFromBag(v.tp.attrs(path))
    Unnest(in, v, path, v2, filter(v2), fields)
  }
  def ounnest(in: Rep, path: String, filter: Rep => Rep, fields: List[String]): Rep = {
    val v = Variable.freshFromBag(in.tp)
    val v2 = Variable.freshFromBag(v.tp.attrs(path))//.outer)
    OuterUnnest(in, v, path, v2, filter(v2), fields)  
  }
  def join(left: Rep, right: Rep, cond: (Rep, Rep) => Rep, fields: List[String]): Rep = {
    val v = Variable.freshFromBag(left.tp)
    val v2 = Variable.freshFromBag(right.tp)
    Join(left, v, right, v2, cond(v, v2), fields)
  }
  def ojoin(left: Rep, right: Rep, cond: (Rep, Rep) => Rep, fields: List[String]): Rep = {
    val v = Variable.freshFromBag(left.tp)
    val v2 = Variable.freshFromBag(right.tp)
    OuterJoin(left, v, right, v2, cond(v, v2), fields)
  }
  def nest(in: Rep, key: List[String], value: Rep => Rep, filter: Rep => Rep, nulls: List[String], ctag: String): Rep = {
    val v = Variable.freshFromBag(in.tp)
    val nr = value(v)
    Nest(in, v, key, nr, filter(v), ext.collect(nr).toList, ctag)
  }
  def reduce(in: Rep, key: List[String], values: List[String]): Rep = {
    val v = Variable.freshFromBag(in.tp)
    Reduce(in, v, key, values)
  }

  def flatdict(e1: Rep): Rep = FlatDict(e1)
  def groupdict(e1: Rep): Rep = GroupDict(e1)

}

trait ShredOptimizer extends BaseCompiler {
  override def addindex(in: Rep, name: String): Rep = in
}

/**
  * Simplistic version of ANF compiler that only 
  * works for plan operators. This is more compatible 
  * with a relational representation (ie. Datasets)
  */
trait BaseOperatorANF extends BaseANF {

  override val compiler = new BaseCompiler {}

  override implicit def exprToDef(e: CExpr): Def = {
    state.get(e) match {
      case Some(v) => Def(v)
      case None =>
        // arbitrary CSE call
        def updateState(c: CExpr): Def = {
          val v = Variable.fresh(e.tp)
          vars = vars :+ v
          state = state + (e -> v)
          stateInv = stateInv + (v -> e)
          Def(v)
        }
        // make these operator types
        e match {
          case o:CNamed => updateState(o)
          case o:Select => updateState(o)
          case o:Projection => updateState(o)
          case o:Unnest => updateState(o)
          case o:OuterUnnest => updateState(o)
          case o:Join => updateState(o)
          case o:OuterJoin => updateState(o)
          case o:Nest => updateState(o)
          case o:Reduce => updateState(o)
          case o:AddIndex => updateState(o)
          case o:CDeDup => updateState(o)
          case o:CReduceBy => updateState(o)
          case _ => Def(e)
      }
    }
  }

}

/**
  * ANF compiler for generating scala and Spark RDD code,
  * uses common subexpression elimination (CSE).
  */
trait BaseANF extends Base {

  val compiler = new BaseCompiler {}

  case class Def(e: CExpr) 

  type Rep = Def

  implicit def defToExpr(d: Def): CExpr = {
    d.e
  }
  
  //implicit def lstToDef(e: List[CExpr]): List[Def] = e.map(Def(_))

  implicit def funcDefToExpr(fd: Def => Def): CExpr => CExpr = {
    (x: CExpr) => reifyBlock { fd(Def(x)) }
  }

  implicit def funcTupDefToExpr(fd: (Def, Def) => Def): (CExpr, CExpr) => CExpr = {
    (x: CExpr, y: CExpr) => reifyBlock { fd(Def(x), Def(y)) }
  }

  implicit def funcDefsToExpr(fd: List[Def] => Def): List[CExpr] => CExpr = {
    (x: List[CExpr]) => reifyBlock { fd(x.map(x2 => Def(x2))) }
  }

  implicit def exprToDef(e: CExpr): Def = {
    state.get(e) match {
      case Some(v) => // CSE!
        Def(v)
      case None =>
        e match {
          case Constant(_) | InputRef(_, _) => Def(e)
	        case _ => 
            val v = Variable.fresh(e.tp)
            vars = vars :+ v
            state = state + (e -> v)
            stateInv = stateInv + (v -> e)
            Def(v)
        }
    }
  }

  var state: Map[CExpr, Variable] = Map()
  var stateInv: Map[Variable, CExpr] = Map()
  var vars: Seq[Variable] = Seq()
  var varMaps: Map[String, Variable] = Map()

  def reset = {
    state = Map()
    stateInv = Map()
    vars = Seq()
    varMaps = Map()
  }

  // TODO: CSE doesn't go beyond a scope.
  def reifyBlock(b: => Rep): CExpr = {
    val oldState = state
    val oldStateInv = stateInv
    val oldVars = vars
    state = Map()
    stateInv = Map()
    vars = Seq()
    val e = b.e
    val res = vars.foldRight(e)((cur, acc) => Bind(cur, stateInv(cur), acc))
    state = oldState
    stateInv = oldStateInv
    vars = oldVars
    res
  }

  def anf(d: Rep): CExpr = 
    vars.foldRight(d.e)((cur, acc) => Bind(cur, stateInv(cur), acc))

  def inputref(x: String, tp:Type): Rep = compiler.inputref(x, tp)
  def input(x: List[Rep]): Rep = ??? 
  def constant(x: Any): Rep = compiler.constant(x)
  def udf(n: String, e1: Rep, tp: Type): Rep = compiler.udf(n, e1, tp)
  def emptysng: Rep = compiler.emptysng
  def cnull: Rep = compiler.cnull
  def unit: Rep = compiler.unit
  def sng(x: Rep): Rep = compiler.sng(x)
  def get(x: Rep): Rep = compiler.get(x)
  def tuple(fs: List[Rep]): Rep = compiler.tuple(fs.map(defToExpr(_)))
  def record(fs: Map[String, Rep]): Rep = compiler.record(fs.map(x => (x._1, defToExpr(x._2))))
  def label(fs: Map[String, Rep]): Rep = compiler.label(fs.map(x => (x._1, defToExpr(x._2))))
  def mathop(op: OpArithmetic, e1: Rep, e2: Rep) = compiler.mathop(op, e1, e2)
  def equals(e1: Rep, e2: Rep): Rep = compiler.equals(e1, e2)
  def lt(e1: Rep, e2: Rep): Rep = compiler.lt(e1, e2)
  def gt(e1: Rep, e2: Rep): Rep = compiler.gt(e1, e2)
  def lte(e1: Rep, e2: Rep): Rep = compiler.lte(e1, e2)
  def gte(e1: Rep, e2: Rep): Rep = compiler.gte(e1, e2)
  def and(e1: Rep, e2: Rep): Rep = compiler.and(e1, e2)
  def not(e1: Rep): Rep = compiler.not(e1)
  def or(e1: Rep, e2: Rep): Rep = compiler.or(e1, e2)
  def project(e1: Rep, field: String): Rep = {
    compiler.project(e1, field)
  }
  def ifthen(cond: Rep, e1: Rep, e2: Option[Rep] = None): Rep = e2 match {
    case Some(a) => compiler.ifthen(cond, e1, Some(a)) 
    case _ => compiler.ifthen(cond, e1, None)
  }
  def merge(e1: Rep, e2: Rep): Rep = compiler.merge(e1, e2)
  def comprehension(e1: Rep, p: Rep => Rep, e: Rep => Rep): Rep = compiler.comprehension(e1, p, e)
  def dedup(e1: Rep): Rep = compiler.dedup(e1)
  def bind(e1: Rep, e: Rep => Rep): Rep = compiler.bind(e1, e)
  def groupby(e1: Rep, g: List[String], v: List[String], gname: String): Rep = compiler.groupby(e1, g, v, gname)
  def reduceby(e1: Rep, g: List[String], v: List[String]): Rep = compiler.reduceby(e1, g, v)
  def named(n: String, e: Rep): Rep = {
    val d = compiler.named(n, e)
    varMaps = varMaps + (n -> d.e.asInstanceOf[Variable])
    d
  }
  def linset(e: List[Rep]): Rep = compiler.linset(e.map(defToExpr(_)))

  def lookup(lbl: Rep, dict: Rep): Rep = compiler.lookup(lbl, dict)
  def emptydict: Rep = compiler.emptydict
  def bagdict(lblTp: LabelType, flat: Rep, dict: Rep): Rep = compiler.bagdict(lblTp, flat, dict)
  def tupledict(fs: Map[String, Rep]): Rep = compiler.tupledict(fs.map(f => (f._1, defToExpr(f._2))))
  def dictunion(d1: Rep, d2: Rep): Rep = compiler.dictunion(d1, d2)

  def select(x: Rep, p: Rep => Rep): Rep = compiler.select(x, p)
  
  def addindex(in: Rep, name: String) = compiler.addindex(in, name)
  def projection(in: Rep, filter: Rep => Rep, fields: List[String]): Rep = 
    compiler.projection(in, filter, fields)
  def unnest(in: Rep, path: String, filter: Rep => Rep, fields: List[String]): Rep = 
    compiler.unnest(in, path, filter, fields)
  def ounnest(in: Rep, path: String, filter: Rep => Rep, fields: List[String]): Rep = 
    compiler.ounnest(in, path, filter, fields)
  def join(left: Rep, right: Rep, cond: (Rep, Rep) => Rep, fields: List[String]): Rep = 
    compiler.join(left, right, cond, fields)
  def ojoin(left: Rep, right: Rep, cond: (Rep, Rep) => Rep, fields: List[String]): Rep = 
    compiler.ojoin(left, right, cond, fields)
  def nest(in: Rep, key: List[String], value: Rep => Rep, filter: Rep => Rep, nulls: List[String], ctag: String): Rep = 
    compiler.nest(in, key, value, filter, nulls, ctag)
  def reduce(in: Rep, key: List[String], values: List[String]): Rep = 
    compiler.reduce(in, key, values)

  def flatdict(e1: Rep): Rep = compiler.flatdict(e1)
  def groupdict(e1: Rep): Rep = compiler.groupdict(e1)

}

/** Finalizes a comprehension expression for a specific compiler.
  *
  */
class Finalizer(val target: Base){

  val letOpt = target match {
    case n:BaseNormalizer => n.letOpt
    case _ => false
  }
  
  var variableMap: Map[CExpr, target.Rep] = Map[CExpr, target.Rep]()
  
  def withMap[T](m: (CExpr, target.Rep))(f: => T): T = {
    val old = variableMap
    variableMap = variableMap + m
    val res = f
    variableMap = old
    res
  }

  def withMapList[T](m: List[(CExpr, target.Rep)])(f: => T): T = {
    val old = variableMap
    variableMap = variableMap ++ m
    val res = f
    variableMap = old
    res
  }

  def finalize(e: CExpr): target.Rep = e match {
    /** Let optimization specific **/
    // let optimization within a named expression
    case CNamed(n1, Bind(x, e1, e)) if letOpt =>
      val name = x match { case v:Variable => v.name; case _ => ??? }
      target.linset(List(target.named(name, finalize(e1)), target.named(n1, finalize(e))))
    // not in a named expression
    case Bind(x, e1, e) if letOpt =>
      val name = x match { case v:Variable => v.name; case _ => ??? }
      target.linset(List(target.named(name, finalize(e1)), finalize(e)))

    case InputRef(x, tp) => target.inputref(x, tp)
    case Input(x) => target.input(x.map(finalize(_)))
    case Constant(x) => target.constant(x)
   	case CUdf(n, e1, tp) => target.udf(n, finalize(e1), tp)
      case EmptySng => target.emptysng
      case Null => target.cnull
  	case CUnit => target.unit
      case Sng(x) => x.tp match {
        case EmptyCType => target.emptysng
        case _ => target.sng(finalize(x))
      }
    case CGet(x) => target.get(finalize(x))
    case Tuple(fs) if fs.isEmpty => target.unit
    case Tuple(fs) => target.tuple(fs.map(f => finalize(f)))
    case Record(fs) if fs.isEmpty => target.unit
    case Record(fs) => target.record(fs.map(f => f._1 -> finalize(f._2)))
    case Label(fs) => target.label(fs.map(f => f._1 -> finalize(f._2)))
    case MathOp(op, e1, e2) => target.mathop(op, finalize(e1), finalize(e2))
    case Equals(e1, e2) => target.equals(finalize(e1), finalize(e2))
    case Lt(e1, e2) => target.lt(finalize(e1), finalize(e2))
    case Gt(e1, e2) => target.gt(finalize(e1), finalize(e2))
    case Lte(e1, e2) => target.lte(finalize(e1), finalize(e2))
    case Gte(e1, e2) => target.gte(finalize(e1), finalize(e2))
    case And(e1, e2) => target.and(finalize(e1), finalize(e2))
    case Not(e1) => target.not(finalize(e1))
    case Or(e1, e2) => target.or(finalize(e1), finalize(e2))
    case If(cond, e1, e2) => e2 match {
      case Some(a) => target.ifthen(finalize(cond), finalize(e1), Option(finalize(a)))
      case _ => target.ifthen(finalize(cond), finalize(e1))
    }
    case Merge(e1, e2) => target.merge(finalize(e1), finalize(e2))
    case Project(e1, pos) => target.project(finalize(e1), pos)
    case Comprehension(e1, v, p, e) =>
      target.comprehension(finalize(e1), (r: target.Rep) => withMap(v -> r)(finalize(p)), 
        (r: target.Rep) => withMap(v -> r)(finalize(e)))
    case CDeDup(e1) => target.dedup(finalize(e1))

    case Bind(x, e1, e) =>
      target.bind(finalize(e1), (r: target.Rep) => withMap(x -> r)(finalize(e)))
    case CGroupBy(e1, v2, g, v, gname) => target.groupby(finalize(e1), g, v, gname)
    case CReduceBy(e1, v2, g, v) => target.reduceby(finalize(e1), g, v)
    case CNamed(n, e) => target.named(n, finalize(e))
    case LinearCSet(exprs) => 
      target.linset(exprs.map(finalize(_)))

    case CLookup(l, d) => 
      target.lookup(finalize(l), finalize(d))
    case EmptyCDict => target.emptydict
    case BagCDict(l, f, d) => 
      target.bagdict(l, finalize(f), finalize(d))
    case TupleCDict(fs) => target.tupledict(fs.map(f => f._1 -> finalize(f._2)))
    case DictCUnion(d1, d2) => target.dictunion(finalize(d1), finalize(d2))

    case Select(x, v, p) =>
      target.select(finalize(x), (r: target.Rep) => withMap(v -> r)(finalize(p)))

    case AddIndex(in, name) => target.addindex(finalize(in), name)    
      
    case Projection(in, v, filter, fields) => 
      target.projection(finalize(in), (r: target.Rep) => withMap(v -> r)(finalize(filter)), fields)
    case Unnest(in, v, path, v2, filter, fields) => 
      target.unnest(finalize(in), path, (r: target.Rep) => withMap(v2 -> r)(finalize(filter)), fields)
    case OuterUnnest(in, v, path, v2, filter, fields) => 
      target.ounnest(finalize(in), path, (r: target.Rep) => withMap(v2 -> r)(finalize(filter)), fields)
    case Join(left, v, right, v2, cond, fields) =>
      target.join(finalize(left), finalize(right), 
        (r: target.Rep, s: target.Rep) => withMapList(List((v,r), (v2,s)))(finalize(cond)), fields)
    case OuterJoin(left, v, right, v2, cond, fields) =>
      target.ojoin(finalize(left), finalize(right), 
        (r: target.Rep, s: target.Rep) => withMapList(List((v,r), (v2,s)))(finalize(cond)), fields)
    case Nest(in, v, key, value, filter, nulls, ctag) =>
      target.nest(finalize(in), key, (r: target.Rep) => withMap(v -> r)(finalize(value)), 
        (r: target.Rep) => withMap(v -> r)(finalize(filter)), nulls, ctag)
    case Reduce(in, v, keys, values) => 
      target.reduce(finalize(in), keys, values)

    case FlatDict(e1) => target.flatdict(finalize(e1))
    case GroupDict(e1) => target.groupdict(finalize(e1))

    case v @ Variable(_, _) => variableMap.getOrElse(v, target.inputref(v.name, v.tp))
  }

}

