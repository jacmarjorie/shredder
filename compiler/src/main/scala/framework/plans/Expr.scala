package framework.plans

import framework.common._

/**
  * Plan Operators and Calculus
  */

trait CExpr { self =>

  def tp: Type
  def vstr: String = self.toString
  val isCacheUnfriendly: Boolean = false

}

case class InputRef(data: String, tp: Type) extends CExpr {
  override def vstr: String = data
  override val isCacheUnfriendly: Boolean = true
}

case class Input(data: List[CExpr]) extends CExpr{
  def tp: BagCType = data match {
    case Nil => BagCType(EmptyCType)
    case _ => BagCType(data.head.tp) 
  }
}

case class Constant(data: Any) extends CExpr{
  def tp: PrimitiveType = data match {
    case _:Double => DoubleType
    case _:Long => LongType
    case _:Int => IntType
    case _:String => StringType
    case _:Boolean => BoolType
  }
  override def vstr: String = s"$data"
}

case class CUdf(name: String, in: CExpr, tp: Type) extends CExpr

case object Index extends CExpr {
  def tp: Type = LongType
}

case object Null extends CExpr {
  def tp: Type = EmptyCType
}

case object EmptySng extends CExpr {
  def tp: BagCType = BagCType(EmptyCType)
}

case class Sng(e1: CExpr) extends CExpr {
  def tp: BagCType = BagCType(e1.tp)
}

case object CUnit extends CExpr {
  def tp: Type = EmptyCType
}

case class Label(fields: Map[String, CExpr]) extends CExpr{
  def tp: LabelType = LabelType(fields.map(f => f._1 -> f._2.tp))
  def apply(n: String) = fields(n)
}

case class Record(fields: Map[String, CExpr]) extends CExpr{
  
  def tp: RecordCType = RecordCType(fields.map(f => f._1 -> f._2.tp))
  def apply(n: String) = fields(n)
  def project(n: List[String]) = Record(fields.filter(f => n.contains(f._1)))

  override def vstr: String = fields.map(f => f._1).mkString(",")

}

case class Tuple(fields: List[CExpr]) extends CExpr {
  def tp: TTupleType = TTupleType(fields.map(_.tp))
  def apply(n: String) = n match {
    case "_1" => fields(0)
    case "_2" => fields(1) 
  } 
  def apply(n: Int) = fields(n)
}

case class CGet(e1: CExpr) extends CExpr {
  def tp: Type = e1.tp match {
    case BagCType(ttp) => ttp
    case _ => ???
  }
}

case class Equals(e1: CExpr, e2: CExpr) extends CExpr {
  def tp: PrimitiveType = BoolType
  override def vstr: String = e1.vstr+"="+e2.vstr
}

case class Lt(e1: CExpr, e2: CExpr) extends CExpr {
  def tp: PrimitiveType = BoolType
  override def vstr: String = e1.vstr+"<"+e2.vstr
}

case class Lte(e1: CExpr, e2: CExpr) extends CExpr{
  def tp: PrimitiveType = BoolType
  override def vstr: String = e1.vstr+"<="+e2.vstr
}

case class Gt(e1: CExpr, e2: CExpr) extends CExpr {
  def tp: PrimitiveType = BoolType
  override def vstr: String = e1.vstr+">"+e2.vstr
}

case class Gte(e1: CExpr, e2: CExpr) extends CExpr {
  def tp: PrimitiveType = BoolType
  override def vstr: String = e1.vstr+">="+e2.vstr
}

case class And(e1: CExpr, e2: CExpr) extends CExpr {
  def tp: PrimitiveType = BoolType
  override def vstr: String = e1.vstr+"&&"+e2.vstr
}

case class Not(e1: CExpr) extends CExpr{
  def tp: PrimitiveType = BoolType
  override def vstr: String = "!"+e1.vstr
}

case class Or(e1: CExpr, e2: CExpr) extends CExpr{
  def tp: PrimitiveType = BoolType
  override def vstr: String = e1.vstr+"||"+e2.vstr
}

case class MathOp(op: OpArithmetic, e1: CExpr, e2: CExpr) extends CExpr {
  def tp: NumericType = NumericType.resolve(e1.tp, e2.tp)
  override def vstr: String = e1.vstr+op+e2.vstr
}

case class Project(e1: CExpr, field: String) extends CExpr { //self =>
  def tp: Type = e1.tp match {
    case t:RecordCType => t.attrTps get field match {
      case Some(fi) => fi
      case _ => sys.error(s"$field not found in $t")
    }
    case t:TTupleType => field match {
      case "_1" => t(0)
      case "_2" => t(1)
      case  _ => t(field.toInt)
    }
    case t:LabelType => t(field)
    case t:TupleDictCType => t(field)
    case t:BagDictCType => t(field)
    case _ => sys.error(s"unsupported projection index $field in $e1")
  }
  override def vstr: String = field

}

case class If(cond: CExpr, e1: CExpr, e2: Option[CExpr]) extends CExpr {
  assert(cond.tp == BoolType)
  val tp: Type = e1.tp
}

case class Merge(e1: CExpr, e2: CExpr) extends CExpr {
  def tp: BagCType = e1.tp.asInstanceOf[BagCType]  //disjoint types?
}

case class Comprehension(e1: CExpr, v: Variable, p: CExpr, e: CExpr) extends CExpr {
  def tp: Type = e.tp match {
    case BagCType(tup) => e.tp
    case t => BagCType(t)
  }
}

case class CDeDup(in: CExpr) extends CExpr with UnaryOp {
  def tp: BagCType = in.tp.asInstanceOf[BagCType]
  override def vstr: String = s"DeDup(${in.vstr})"
}

// replace all occurences of x with e1 in e1
case class Bind(x: CExpr, e1: CExpr, e: CExpr) extends CExpr {
  def tp: Type = e.tp
}

trait CombineOp extends CExpr {
  def tp: BagCType
  def e1: CExpr
  def v1: Variable
  def keys: List[String]
  def values: List[String]
  def valuesTp: Type
}

case class CReduceBy(e1: CExpr, v1: Variable, keys: List[String], values: List[String]) extends CExpr with CombineOp {
  val keysTp: RecordCType = RecordCType(keys.map(n => n -> v1.tp.attrs(n)).toMap)
  val valuesTp: RecordCType = RecordCType(values.map(n => n -> DoubleType).toMap)//v1.tp.attrs(n)).toMap)
  def tp: BagCType = v1.tp.attrs get "_1" match {
    case Some(TTupleType(fs)) => 
      BagCType(TTupleType(fs :+ RecordCType((v1.tp.attrs - "_1") ++ valuesTp.attrTps)))
    case _ => BagCType(RecordCType(keysTp.attrTps ++ valuesTp.attrTps))
  }

}

case class CGroupBy(e1: CExpr, v1: Variable, keys: List[String], values: List[String], groupName: String = "_2") extends CExpr with CombineOp {
  val e1Tp: RecordCType = v1.tp.asInstanceOf[RecordCType]
  val keysTp: RecordCType = RecordCType(keys.map(n => n -> e1Tp(n)).toMap)
  val valuesTp: BagCType = BagCType(RecordCType(values.map(n => n -> e1Tp(n)).toMap))
  def tp: BagCType = BagCType(RecordCType(keysTp.attrTps ++ Map(groupName -> valuesTp)))
}

case class CNamed(name: String, e: CExpr) extends CExpr {
  def tp: Type = e.tp
  override def vstr: String = e.vstr
}

case class LinearCSet(exprs: List[CExpr]) extends CExpr {
  def tp: Type = EmptyCType
  def getTypeMap: Map[Type, String] = exprs.map{ e => e match {
    case CNamed(n, e1) => (e1.tp.asInstanceOf[BagCType].tp -> s"Rec$n")
    case e1 => (e1.tp.asInstanceOf[BagCType].tp -> s"Record${Variable.newId}")
  }}.toMap
}

/** Extensions for intermediate NRC used in shredding */

case class CLookup(lbl: CExpr, dict: CExpr) extends CExpr {
  def tp: BagCType = dict.tp match {
    case MatDictCType(lbl, dict) => dict
    case _ => sys.error(s"not supported ${dict.tp}")
  }
}

case class FlatDict(in: CExpr) extends CExpr with UnaryOp {
  def tp: BagCType = in.tp match {
    case MatDictCType(lbl, BagCType(RecordCType(ms))) => 
      BagCType(RecordCType(ms + ("_1" -> lbl)))
    case _ => sys.error(s"unsupported type ${in.tp}")
  }
}

case class GroupDict(in: CExpr) extends CExpr with UnaryOp {

  def tp: MatDictCType = in.tp match {

    case BagCType(RecordCType(ms)) => 
      val lbl = ms get "_1" match {
        case Some(l:LabelType) => l
        case _ => sys.error("invalid bag")
      }
      MatDictCType(lbl, BagCType(RecordCType(ms - "_1")))

    case BagCType(TTupleType(ms)) if ms.size == 2 =>
      val lbl = ms.head match {
        case l:LabelType => l
        case _ => sys.error("invalid bag")
      }
      val bag = ms.last match {
        case bt:BagCType => bt
        case bt => BagCType(bt)
      }
      MatDictCType(lbl, bag)

    case _ => sys.error(s"unsupported type ${in.tp}")
  }

}

case object EmptyCDict extends CExpr {
  def tp: TDict = EmptyDictCType
}

case class BagCDict(lblTp: LabelType, flat: CExpr, dict: CExpr) extends CExpr {
  def tp: BagDictCType = 
    BagDictCType(BagCType(TTupleType(List(lblTp, flat.tp))), dict.tp.asInstanceOf[TTupleDict])
  def apply(n: String) = n match {
    case "flat" => flat
    case "_2" => dict
  }
  def _2 = dict
}

case class TupleCDict(fields: Map[String, CExpr]) extends CExpr {
  def tp: TupleDictCType = TupleDictCType(fields.map(f => f._1 -> f._2.tp.asInstanceOf[TDict]))
  def apply(n: String) = fields(n)
}

object TupleCDict {
  def apply(fields: (String, CExpr)*): TupleCDict = TupleCDict(Map(fields:_*))
}

// turn into a comprehension?
case class DictCUnion(d1: CExpr, d2: CExpr) extends CExpr {
  def tp: BagDictCType = d1.asInstanceOf[BagDictCType]
}

case class Variable(name: String, override val tp: Type) extends CExpr { self =>
  
  // equals with a label check
  def lequals(that: CExpr): Boolean = that match {
    case that: Variable => this.equals(that)
    case Project(v, f) => this.lequals(v)
    case t if that.tp.isInstanceOf[LabelType] =>
      that.tp.asInstanceOf[LabelType].attrTps.keys.toList.contains(this.name)
    case t if that.tp.isInstanceOf[RecordCType] => // new label representation
      that.tp.asInstanceOf[RecordCType].attrTps.keys.toList.contains(this.name)
    case _ => false  
  }

  override def hashCode: Int = (name, tp).hashCode()
  def quote: String = self.name
  override def vstr: String = self.name

}

object Variable {

  private var lastId = 1

  def fresh(tp: Type): Variable = {
    val id = newId()
    Variable(s"x$id", tp)
  }

  def fresh(key: Type, value: Type): Variable = {
    fresh(TTupleType(List(key, value)))
  }

  def freshFromBag(tp: Type, index: String = ""): Variable = {
    val id = newId()
    tp match {
      case OptionType(bag) => freshFromBag(bag)
      case BagDictCType(BagCType(TTupleType(List(EmptyCType, BagCType(tup)))), tdict) => Variable(s"x$id", tup)
      case BagCType(TTupleType(List(EmptyCType, BagCType(tup)))) =>  Variable(s"x$id", tup)
      case BagDictCType(flat, dict) => Variable(s"x$id",flat.tp)
      case BagCType(RecordCType(ms)) if index != "" => Variable(s"x$id", RecordCType(ms ++ Map(index -> LongType)))
      case BagCType(tup) => Variable(s"x$id", tup)
      case MatDictCType(key, value:BagCType) => Variable(s"x$id", RecordCType(value.tp.attrs + ("_1" -> key)))
      case _ => Variable(s"x$id", tp)
    }
  }

  def fromBag(name: String, tp: Type): Variable = Variable(name, RecordCType(tp.attrs))

  def fresh(n: String = "x"): String = s"$n${newId()}"
  def newId(): Int = {
    val id = lastId
    lastId += 1
    id
  }
  
}


