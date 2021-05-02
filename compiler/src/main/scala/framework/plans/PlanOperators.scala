package framework.plans

import framework.common._

trait UnaryOp{ 
  val in: CExpr
}

case class COption(e: CExpr) extends CExpr {
  def tp: OptionType = OptionType(e.tp)
}

/** Operators of the plan language **/ 

case class Select(x: CExpr, v: Variable, p: CExpr) extends CExpr with UnaryOp { self =>

  def tp:Type = x.tp 

  val in: CExpr = x
  override def vstr: String = s"""Select(${x.vstr}, ${p.vstr})"""

}

case class AddIndex(e: CExpr, name: String) extends CExpr with UnaryOp {
  def tp: BagCType = BagCType(RecordCType(e.tp.attrs ++ Map(name -> LongType)))
  val in: CExpr = e
  override def vstr: String = s"AddIndex(${e.vstr}, $name)" 
  override val isCacheUnfriendly: Boolean = true
}

// rename filter
case class Projection(in: CExpr, v: Variable, filter: CExpr, fields: List[String]) extends CExpr with UnaryOp {

  def tp: BagCType = BagCType(filter.tp)

  override def vstr: String = 
    s"""Projection(${in.vstr}, {${filter.vstr}}, ${fields.toSet.mkString(",")})""" 

}

/** Unnest operators **/

trait UnnestOp extends CExpr with UnaryOp {

  def tp: BagCType
  val in: CExpr
  val v: Variable
  val path: String 
  val v2: Variable
  val filter: CExpr
  val fields: List[String]

  val outer: Boolean 

  val topAttrs: Map[String, CExpr] = {
    v.tp.project(fields).attrs.map(f => f._1 -> Project(v, f._1)) - path
  }

  val nextAttrs: Map[String, CExpr] = {
    v2.tp.project(fields).attrs.map(f => f._1 -> Project(v2, f._1))
  }

  override def vstr: String = {
    val lbl = if (outer) "Outer" else "" 
    s"""Unnest$lbl(${in.vstr}, $path, ${fields.toSet.mkString(",")})""" 
  }


}

case class Unnest(in: CExpr, v: Variable, path: String, v2: Variable, filter: CExpr, fields: List[String]) extends UnnestOp {
  def tp: BagCType = BagCType(v.tp.merge(v2.tp).project(fields))
  val outer: Boolean = false
}

case class OuterUnnest(in: CExpr, v: Variable, path: String, v2: Variable, filter: CExpr, fields: List[String]) extends UnnestOp {
  
  val index = Map(path+"_index" -> LongType)
  
  def tp: BagCType = 
    BagCType(RecordCType((v.tp.attrs - path) ++ index).merge(v2.tp.outer).project(fields))
  
  val outer: Boolean = true
  
  override val nextAttrs: Map[String, CExpr] = 
    v2.tp.project(fields).attrs.map(f => f._1 -> COption(Project(v2, f._1)))

}

/** Join operators **/

trait JoinOp extends CExpr { 

  def tp: BagCType

  val left: CExpr
  val v: Variable

  val ext = new Extensions{}
  // val ps = ext.collect(cond)
  val ps = ext.collectFromAnd(cond)

  // get around join with domain
  def p1s: Set[String] = v.tp match {
    case RecordCType(ms) => ms.get("_LABEL") match {
      case Some(LabelType(fs)) =>
        val keys = fs.keySet 
        ps.filter(s => keys(s))
      case _ => ps.filter(s => v.tp.attrs.contains(s))
    }
    case _ => ps.filter(s => v.tp.attrs.contains(s))
  }

  def p1: String = p1s.mkString(",")

  val right: CExpr
  val v2: Variable

  def p2s: Set[String] = ps.filter(s => v2.tp.attrs.contains(s))
  def p2: String = p2s.mkString(",")

  val cond: CExpr

  val fields: List[String]
  
  val jtype: String

  val isEquiJoin: Boolean = ps.nonEmpty && (p1s.size == p2s.size)

  // this needs fixed
  override def vstr: String = s"Join$jtype(${left.vstr}, ${right.vstr}, $p1=$p2)"
  override val isCacheUnfriendly: Boolean = true

}

case class Join(left: CExpr, v: Variable, right: CExpr, v2: Variable, cond: CExpr, fields: List[String]) extends JoinOp {
  def tp: BagCType = BagCType(v.tp.merge(v2.tp).project(fields))
  val jtype = "inner"

}

case class OuterJoin(left: CExpr, v: Variable, right: CExpr, v2: Variable, cond: CExpr, fields: List[String]) extends JoinOp {
  def tp: BagCType = { (cond, right.tp.isDict) match {
    case (Equals(Project(_, p1), Project(_, p2 @ "_1")), true) =>
      val nvtp = RecordCType(v.tp.attrs - p1)
      val nv2tp = RecordCType(v2.tp.attrs - "_1")
      BagCType(nvtp.merge(nv2tp).project(fields))
    case _ => 
      BagCType(v.tp.merge(v2.tp.outer).project(fields))
    }
  }
  val jtype = "left_outer"

}

case class Nest(in: CExpr, v: Variable, key: List[String], value: CExpr, filter: CExpr, nulls: List[String], ctag: String) extends CExpr { //with UnaryOp {
  def tp: BagCType = value.tp match {
    case _:NumericType => BagCType(RecordCType(v.tp.project(key).attrTps ++ Map(ctag -> DoubleType)))
    case _ => BagCType(RecordCType(v.tp.project(key).attrTps ++ Map(ctag -> BagCType(value.tp.unouter))))
  }

  override def vstr: String = 
    s"""Nest(${in.vstr}, key = ${key.mkString(",")}, value = ${key.mkString(",")})""" 

  override val isCacheUnfriendly: Boolean = true

}

case class Reduce(in: CExpr, v: Variable, keys: List[String], values: List[String]) extends CExpr with UnaryOp {
  def tp: BagCType = BagCType(v.tp.project(keys).merge(v.tp.project(values)))
  override def vstr: String = 
    s"""Reduce(${in.vstr}, keys = ${keys.mkString(",")}, values = ${values.mkString(",")})""" 
}








