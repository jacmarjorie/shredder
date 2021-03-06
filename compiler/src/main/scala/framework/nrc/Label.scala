package framework.nrc

import framework.common._

/**
  * Label extensions
  */
trait Label {
  this: NRC =>

  sealed trait LabelExpr extends TupleAttributeExpr {
    def tp: LabelType
  }

  final case class LabelVarRef(name: String, tp: LabelType) extends LabelExpr with VarRef

  final case class LabelProject(tuple: TupleVarRef, field: String) extends LabelExpr with TupleProject {
    override def tp: LabelType = super.tp.asInstanceOf[LabelType]
  }

  final case class LabelLet(x: VarDef, e1: Expr, e2: LabelExpr) extends LabelExpr with Let {
    assert(x.tp == e1.tp)

    val tp: LabelType = e2.tp
  }

  final case class LabelIfThenElse(cond: CondExpr, e1: LabelExpr, e2: Option[LabelExpr]) extends LabelExpr with IfThenElse {
    assert(e2.isEmpty || e1.tp == e2.get.tp)

    val tp: LabelType = e1.tp
  }

  sealed trait ExtractLabel {
    def lbl: LabelExpr

    def e: Expr
  }

  final case class NumericExtractLabel(lbl: LabelExpr, e: NumericExpr) extends NumericExpr with ExtractLabel {
    def tp: NumericType = e.tp
  }

  final case class PrimitiveExtractLabel(lbl: LabelExpr, e: PrimitiveExpr) extends PrimitiveExpr with ExtractLabel {
    def tp: PrimitiveType = e.tp
  }

  final case class BagExtractLabel(lbl: LabelExpr, e: BagExpr) extends BagExpr with ExtractLabel {
    def tp: BagType = e.tp
  }

  final case class TupleExtractLabel(lbl: LabelExpr, e: TupleExpr) extends TupleExpr with ExtractLabel {
    def tp: TupleType = e.tp
  }

  final case class LabelExtractLabel(lbl: LabelExpr, e: LabelExpr) extends LabelExpr with ExtractLabel {
    def tp: LabelType = e.tp
  }

  sealed trait LabelParameter extends Expr {
    def e: Expr

    def name2: String

    def tp: Type = e.tp
  }

  final case class VarRefLabelParameter(e: Expr with VarRef) extends LabelParameter {
    def name2: String = e.name
  }

  final case class ProjectLabelParameter(e: Expr with Project) extends LabelParameter {
    def name2: String = e.tuple.name + "_" + e.field
  }

  object NewLabel {
    private var currId = 0

    def getNextId: Int = {
      currId += 1
      currId
    }

    def apply(params: LabelParameter*): NewLabel =
      NewLabel(params.map(p => p.name2 -> p).toMap)

    def apply(params: Set[LabelParameter]): NewLabel =
      NewLabel(params.map(p => p.name2 -> p).toMap)

    implicit def orderingById: Ordering[NewLabel] = Ordering.by(e => e.id)
  }

  final case class NewLabel(params: Map[String, LabelParameter], id: Int = NewLabel.getNextId) extends LabelExpr {
    val tp: LabelType = LabelType(params.map(x => x._1 -> x._2.tp))

    override def equals(that: Any): Boolean = that match {
      case that: NewLabel => this.id == that.id
      case _ => false
    }

    override def hashCode: Int = id.hashCode()

    override def toString: String =
      s"Label(${(id :: params.keys.toList).mkString(", ")}"
  }

}
