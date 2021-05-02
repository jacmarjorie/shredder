package framework.plans

import framework.common._
import framework.utils.Utils.ind

/**
  * Top down printer (preserves variable names through transformations)
  * for bottom up printer see BaseStringify in Base.scala
  */

object Printer {
  val compiler = new BaseStringify{}
  import compiler._
  def quote(e: CExpr): String = e match {
    case InputRef(d, t) => inputref(d,t)
    case Input(d) => input(d.map(quote(_)))
    case Constant(d) => constant(d)
  	case CUdf(n, e1, tp) => udf(n, quote(e1), tp)
      case EmptySng => emptysng
      case CUnit => unit
  	case Null => "null"
  	case Index => "index"
    case Sng(e1) => sng(quote(e1))
    case Tuple(fs) => tuple(fs.map(quote(_)))
    case Record(fs) => record(fs.map(f => f._1 -> quote(f._2)))
    case Label(fs) => label(fs.map(f => f._1 -> quote(f._2)))
    case MathOp(op, e1, e2) => compiler.mathop(op, quote(e1), quote(e2))
    case Equals(e1, e2) => compiler.equals(quote(e1), quote(e2))
    case Lt(e1, e2) => lt(quote(e1), quote(e2))
    case Lte(e1, e2) => lte(quote(e1), quote(e2))
    case Gt(e1, e2) => gt(quote(e1), quote(e2))
    case Gte(e1, e2) => gte(quote(e1), quote(e2))
    case And(e1, e2) => and(quote(e1), quote(e2))
    case Not(e1) => not(quote(e1))
    case Or(e1, e2) => or(quote(e1), quote(e2))
    case Project(e1, f) => project(quote(e1), f)
    case CGet(e1) => s"get(${quote(e1)})"
    case If(c, e1, e2) => e2 match {
      case Some(a) => ifthen(quote(c), quote(e1), Some(quote(a)))
      case _ => ifthen(quote(c), quote(e1), None)
    }
    case Merge(e1, e2) => merge(quote(e1), quote(e2))
    case Comprehension(e1, v, p, e) => p match {
      case Constant(true) => s"{ ${quote(e)} | ${v.quote} <- ${quote(e1)} }"
      case px => s"{ ${quote(e)} | ${v.quote} <- ${quote(e1)}, ${quote(px)} }"
    }
    case Bind(x, e1, e2) => s"{ ${quote(e2)} | ${quote(x)} := ${quote(e1)} }"
    case CDeDup(e1) => s"DeDup(${quote(e1)})"
    case CGroupBy(e1, v, grp, value, gname) => s"""(${quote(e1)}).groupBy(${grp.mkString(",")}; ${value.mkString(",")}, "$gname")"""
    case CReduceBy(e1, v, grp, value) => s"(${quote(e1)}).reduceBy(${grp.mkString(",")}; ${value.mkString(",")})"
    case CNamed(n, e) => named(n, quote(e))
    case LinearCSet(exprs) => linset(exprs.map(quote(_)))

    case CLookup(lbl, dict) => lookup(quote(lbl), quote(dict))
    case EmptyCDict => emptydict
    case BagCDict(lblTp, flat, dict) => bagdict(lblTp, quote(flat), quote(dict))
    case TupleCDict(fs) => tupledict(fs.map(f => f._1 -> quote(f._2)))
    case DictCUnion(e1, e2) => dictunion(quote(e1), quote(e2))

    case Select(x, v, p) => 
      s"""| SELECT[ ${quote(p)} ](${quote(x)})
          | """.stripMargin
    case AddIndex(e1, name) => s"INDEX(${quote(e1)})"
    case Projection(e1, v, p, fields) => 
      s"""|PROJECT[${fields.mkString(",")}, ${quote(p)}](${quote(e1)})
          |""".stripMargin
    case Nest(e1, v, keys, value, filter, nulls, ctag) =>
      s"""|NEST^{${quote(value)}, ${quote(filter)}, $ctag}_{U, ${keys.mkString(",")} / ${nulls.mkString(",")}}(${quote(e1)})
          |""".stripMargin
    case Unnest(e1, v, path, v2, filter, fields) =>
      s"""|UNNEST[${quote(v)}.$path, ${quote(filter)}, ${fields.mkString(",")}](${quote(e1)})
          |""".stripMargin
    case OuterUnnest(e1, v, path, v2, filter, fields) =>
      s"""|OUTERUNNEST[${quote(v)}.$path, ${quote(filter)}, ${fields.mkString(",")}](${quote(e1)})
          |""".stripMargin
    case Join(left, v1, right, v2, cond, fields) => 
      s"""|${quote(left)} JOIN [${quote(cond)}, ${fields.mkString(",")}] ${quote(right)}
          |""".stripMargin
    case OuterJoin(left, v1, right, v2, cond, fields) => 
      s"""|${quote(left)} OUTERJOIN [${quote(cond)}, ${fields.mkString(",")}] ${quote(right)}
          |""".stripMargin
    case Reduce(e1, v, grp, value) => 
      s"""| REDUCE[ keys = {${grp.mkString(",")}} values = {${value.mkString(",")}} ](${quote(e1)})
          | """.stripMargin
    
    case FlatDict(e1) => flatdict(quote(e1))
    case GroupDict(e1) => groupdict(quote(e1))

    case Variable(n, tp) => n
  }
}
