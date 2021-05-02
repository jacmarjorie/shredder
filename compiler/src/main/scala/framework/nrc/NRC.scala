package framework.nrc

import framework.common._
import framework.utils.Utils

/**
  * Base NRC expressions
  */
trait BaseExpr {

  trait Expr {
    def tp: Type
  }

  trait TupleAttributeExpr extends Expr {
    def tp: TupleAttributeType
  }

  trait PrimitiveExpr extends TupleAttributeExpr {
    def tp: PrimitiveType
  }

  trait NumericExpr extends PrimitiveExpr {
    def tp: NumericType
  }

  trait CondExpr extends PrimitiveExpr {
    def tp: PrimitiveType = BoolType
  }

  trait BagExpr extends TupleAttributeExpr {
    def tp: BagType
  }

  trait AbstractTuple

  trait TupleExpr extends Expr with AbstractTuple {
    def tp: TupleType
  }

}

/**
  * NRC constructs
  */
trait NRC extends BaseExpr {

  val GROUP_ATTR_NAME: String = "_GROUP"

  sealed trait Const {
    def v: Any

    def tp: PrimitiveType
  }

  final case class NumericConst(v: AnyVal, tp: NumericType) extends NumericExpr with Const

  final case class PrimitiveConst(v: Any, tp: PrimitiveType) extends PrimitiveExpr with Const

  trait VarRef {

    def varDef: VarDef = VarDef(name, tp)

    def name: String

    def tp: Type

  }

  final case class NumericVarRef(name: String, tp: NumericType) extends NumericExpr with VarRef

  final case class PrimitiveVarRef(name: String, tp: PrimitiveType) extends PrimitiveExpr with VarRef

  final case class BagVarRef(name: String, tp: BagType) extends BagExpr with VarRef { self =>
    def union(in: (String, TupleAttributeExpr)*): (BagVarRef, BagExpr) = (self, Singleton(Tuple(in:_*)))
    def union(in: BagIfThenElse): (BagVarRef, BagIfThenElse) = (self, in)
  }

  final case class TupleVarRef(name: String, tp: TupleType) extends TupleExpr with VarRef { self => 
    def in(in: (BagVarRef, BagExpr)): ForeachUnion = ForeachUnion(self.varDef, in._1, in._2)
    def <--(in: BagVarRef): ForeachUnion = ForeachUnion(self.varDef, in, Singleton(self))
  }

  final case class Udf(name: String, in: PrimitiveExpr, tp: NumericType) extends NumericExpr 

  trait Project {
    def tuple: VarRef with Expr

    def field: String

    def tp: Type
  }

  trait TupleProject extends Project {
    def tuple: TupleVarRef

    def field: String

    def tp: TupleAttributeType = tuple.tp(field)
  }

  final case class NumericProject(tuple: TupleVarRef, field: String) extends NumericExpr with TupleProject {
    override def tp: NumericType = super.tp.asInstanceOf[NumericType]
  }

  final case class PrimitiveProject(tuple: TupleVarRef, field: String) extends PrimitiveExpr with TupleProject {
    override def tp: PrimitiveType = super.tp.asInstanceOf[PrimitiveType]
  }

  final case class BagProject(tuple: TupleVarRef, field: String) extends BagExpr with TupleProject {
    override def tp: BagType = super.tp.asInstanceOf[BagType]
  }

  final case class ForeachUnion(x: VarDef, e1: BagExpr, e2: BagExpr) extends BagExpr {
	assert(x.tp == e1.tp.tp)

    val tp: BagType = e2.tp
  }

  final case class Union(e1: BagExpr, e2: BagExpr) extends BagExpr {
    assert(e1.tp == e2.tp)

    val tp: BagType = e1.tp
  }

  final case class Singleton(e: TupleExpr) extends BagExpr {
    val tp: BagType = BagType(e.tp)
  }

  final case class Get(e: BagExpr) extends TupleExpr {
    val tp: TupleType = e.tp.tp
  }

  final case class DeDup(e: BagExpr) extends BagExpr {
    val tp: BagType = e.tp
  }

  final case class Tuple(fields: Map[String, TupleAttributeExpr]) extends TupleExpr {
    val tp: TupleType = TupleType(fields.map(f => f._1 -> f._2.tp))
  }

  object Tuple {
    def apply(fs: (String, TupleAttributeExpr)*): Tuple = Tuple(Map(fs: _*))
  }

  trait Let extends Expr {
    def x: VarDef

    def e1: Expr

    def e2: Expr

    def tp: Type
  }

  final case class NumericLet(x: VarDef, e1: Expr, e2: NumericExpr) extends NumericExpr with Let {
    assert(x.tp == e1.tp)

    val tp: NumericType = e2.tp
  }

  final case class PrimitiveLet(x: VarDef, e1: Expr, e2: PrimitiveExpr) extends PrimitiveExpr with Let {
    assert(x.tp == e1.tp)

    val tp: PrimitiveType = e2.tp
  }

  final case class BagLet(x: VarDef, e1: Expr, e2: BagExpr) extends BagExpr with Let {
    assert(x.tp == e1.tp)

    val tp: BagType = e2.tp
  }

  final case class TupleLet(x: VarDef, e1: Expr, e2: TupleExpr) extends TupleExpr with Let {
    assert(x.tp == e1.tp)

    val tp: TupleType = e2.tp
  }

  sealed trait Cmp {
    def op: OpCmp

    def e1: Expr

    def e2: Expr
  }

  final case class PrimitiveCmp(op: OpCmp, e1: PrimitiveExpr, e2: PrimitiveExpr) extends CondExpr with Cmp {
    assert(e1.tp == e2.tp || (e1.tp.isInstanceOf[NumericType] && e2.tp.isInstanceOf[NumericType]))
  }

  final case class And(e1: CondExpr, e2: CondExpr) extends CondExpr

  final case class Or(e1: CondExpr, e2: CondExpr) extends CondExpr

  final case class Not(c: CondExpr) extends CondExpr

  trait IfThenElse {
    def cond: CondExpr

    def e1: Expr

    def e2: Option[Expr]
  }

  final case class NumericIfThenElse(cond: CondExpr, e1: NumericExpr, p2: NumericExpr) extends NumericExpr with IfThenElse {
    assert(e1.tp == p2.tp)

    val tp: NumericType = e1.tp

    def e2: Option[NumericExpr] = Some(p2)
  }

  final case class PrimitiveIfThenElse(cond: CondExpr, e1: PrimitiveExpr, p2: PrimitiveExpr) extends PrimitiveExpr with IfThenElse {
    assert(e1.tp == p2.tp)

    val tp: PrimitiveType = e1.tp

    def e2: Option[PrimitiveExpr] = Some(p2)
  }

  final case class BagIfThenElse(cond: CondExpr, e1: BagExpr, e2: Option[BagExpr]) extends BagExpr with IfThenElse {
    assert(e2.isEmpty || e1.tp == e2.get.tp)

    val tp: BagType = e1.tp

  }

  final case class TupleIfThenElse(cond: CondExpr, e1: TupleExpr, t2: TupleExpr) extends TupleExpr with IfThenElse {
    assert(e1.tp == t2.tp)

    val tp: TupleType = e1.tp

    def e2: Option[TupleExpr] = Some(t2)
  }

  final case class ArithmeticExpr(op: OpArithmetic, e1: NumericExpr, e2: NumericExpr) extends NumericExpr {
    val tp: NumericType = NumericType.resolve(e1.tp, e2.tp)
  }

  final case class Count(e: BagExpr) extends NumericExpr {
    val tp: NumericType = IntType
  }

  final case class Sum(e: BagExpr, fields: List[String]) extends TupleExpr {
    assert(fields.forall(n => e.tp.tp(n).isInstanceOf[NumericType]),
      "Sum over non-numeric attributes")

    val tp: TupleType =
      TupleType(fields.map(n => n -> e.tp.tp(n)).toMap)
  }

  trait GroupByExpr extends BagExpr {
    def e: BagExpr

    def keys: List[String]

    def keysTp: TupleType

    def values: List[String]

    def valuesTp: Type
  }

  final case class GroupByKey(e: BagExpr,
                              keys: List[String],
                              values: List[String],
                              groupAttrName: String = GROUP_ATTR_NAME
                             ) extends GroupByExpr {
    assert(keys.size == keys.distinct.size, "Duplicated group-by keys")

    val keysTp: TupleType =
      TupleType(keys.map(n => n -> e.tp.tp(n)).toMap)

    val valuesTp: BagType =
      BagType(TupleType(values.map(n => n -> e.tp.tp(n)).toMap))

    val tp: BagType =
      BagType(TupleType(keysTp.attrTps ++ Map(groupAttrName -> valuesTp)))
  }

  final case class ReduceByKey(e: BagExpr, keys: List[String], values: List[String]) extends GroupByExpr {
    assert(values.forall(n => e.tp.tp(n).isInstanceOf[ReducibleType]),
      "ReduceByKey over non-reducible attributes")
    assert(keys.size == keys.distinct.size, "Duplicated reduce-by keys")

    val keysTp: TupleType =
      TupleType(keys.map(n => n -> e.tp.tp(n)).toMap)

    val valuesTp: TupleType =
      TupleType(values.map(n => n -> e.tp.tp(n)).toMap)

    val tp: BagType =
      BagType(TupleType(keysTp.attrTps ++ valuesTp.attrTps))
  }

  final case class Assignment(name: String, rhs: Expr)

  final case class Program(statements: List[Assignment]) {
    assert(statements.map(_.name).distinct.size == statements.map(_.name).size)

    def ++(p: Program): Program = Program(statements ++ p.statements)

    def append(a: Assignment): Program = Program(statements :+ a)

    def get(n: String): Option[Assignment] = statements.filter(_.name == n) match {
      case Nil => None
      case hd :: Nil => Some(hd)
      case _ => sys.error("Multiple assignments with the same name")
    }

    def apply(n: String): Assignment = get(n) match {
      case Some(a) => a
      case None => throw new NoSuchElementException
    }
  }

  object Program {
    def apply(statements: Assignment*): Program = Program(List(statements: _*))

    def apply(name: String, e: Expr): Program = Program(List(Assignment(name, e)))
  }

}
