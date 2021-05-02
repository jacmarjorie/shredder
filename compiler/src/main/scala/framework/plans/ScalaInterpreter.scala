package framework.plans

import framework.common._
import scala.collection.mutable.HashMap
import framework.utils.Utils.ind

/** Scala evaluator
  * @deprecated this was used primarily for the inital development of the operators, 
  * and has not been used since focus has shifted to code generators
  */
trait BaseScalaInterp extends Base{
  type Rep = Any
  val ctx = scala.collection.mutable.Map[Any, Any]()
  var doteq = true
  def inputref(x: String, tp: Type): Rep = ctx(x)
  def input(x: List[Rep]): Rep = x
  def constant(x: Any): Rep = x
  def emptysng: Rep = Nil
  def cnull: Rep = null
  def unit: Rep = ()
  def sng(x: Rep): Rep = List(x)
  def get(x: Rep): Rep = x.asInstanceOf[List[Rep]].head
  def tuple(x: List[Rep]): Rep = x
  def record(fs: Map[String, Rep]): Rep = {
    if (doteq) RecordValue(fs.asInstanceOf[Map[String, Rep]], RecordValue.newId)
    else Rec(fs.asInstanceOf[Map[String, Rep]])
  }
  def label(fs: Map[String, Rep]): Rep = Rec(fs.asInstanceOf[Map[String, Rep]])
  def mult(e1: Rep, e2: Rep): Rep = e1.asInstanceOf[Double] * e2.asInstanceOf[Double]
  def equals(e1: Rep, e2: Rep): Rep = e1 == e2
  def lt(e1: Rep, e2: Rep): Rep = e1.asInstanceOf[Int] < e2.asInstanceOf[Int]
  def gt(e1: Rep, e2: Rep): Rep = e1.asInstanceOf[Int] > e2.asInstanceOf[Int]
  def lte(e1: Rep, e2: Rep): Rep = e1.asInstanceOf[Int] <= e2.asInstanceOf[Int]
  def gte(e1: Rep, e2: Rep): Rep = e1.asInstanceOf[Int] >= e2.asInstanceOf[Int]
  def and(e1: Rep, e2: Rep): Rep = e1.asInstanceOf[Boolean] && e2.asInstanceOf[Boolean]
  def not(e1: Rep): Rep = !e1.asInstanceOf[Boolean]
  def or(e1: Rep, e2: Rep): Rep = e1.asInstanceOf[Boolean] || e2.asInstanceOf[Boolean]
  def project(e1: Rep, f: String) = f match {
    //case "_1" if e1.isInstanceOf[List[(Int, List[_])]] => e1.asInstanceOf[List[(Int, List[_])]].head._2 //no
    case "_1" => e1.asInstanceOf[Product].productElement(0)
    case "_2" if e1.isInstanceOf[RecordValue] => e1.asInstanceOf[RecordValue].map("v")
    case "_2" => e1.asInstanceOf[Product].productElement(1)
    case f => e1 match {
      case None => None
      case m:Rec => m.map.get("map") match {
        case Some(a) => a.asInstanceOf[Map[String, Any]](f)
        case _ => m.map(f)
      }
      case m:RecordValue => m.map(f)
      case c:CaseClassRecord => 
        val field = c.getClass.getDeclaredFields.find(_.getName == f).get
        field.setAccessible(true)
        field.get(c)
      //case m:HashMap[_,_] => m(f.asInstanceOf[_])
      case l:List[_] => l.map(project(_,f))
      case p:Product => p.productElement(f.toInt)
      case t => sys.error(s"unsupported projection type ${t.getClass} for object:\n$t") 
    }
  }
  def ifthen(cond: Rep, e1: Rep, e2: Option[Rep]): Rep = e2 match {
    case Some(a) => if (cond.asInstanceOf[Boolean]) { e1 } else { a }
    case _ => if (cond.asInstanceOf[Boolean]) { e1 } else { Nil }
  } 
  def merge(e1: Rep, e2: Rep): Rep = e1.asInstanceOf[List[_]] ++ e2.asInstanceOf[List[_]]
  def comprehension(e1: Rep, p: Rep => Rep, e: Rep => Rep): Rep = {
    e1 match {
      case Nil => e(Nil) match { case i:Int => 0; case i:Double => 0.0; case _ => Nil }
      case data @ (head :: tail) => e(head) match {
        case i:Int =>
          data.withFilter(p.asInstanceOf[Rep => Boolean]).map(e).asInstanceOf[List[Int]].sum
        case i:Double =>
          data.withFilter(p.asInstanceOf[Rep => Boolean]).map(e).asInstanceOf[List[Double]].sum
  case _ => 
          data.withFilter(p.asInstanceOf[Rep => Boolean]).
            flatMap(e.asInstanceOf[Rep => scala.collection.GenTraversableOnce[Rep]])
      }
    }
  }
  def dedup(e1: Rep): Rep = {
    val data = e1.asInstanceOf[List[_]]
    data.head match {
      case r:Rec => data.distinct
      case r:RecordValue => data.map(_.asInstanceOf[RecordValue].toRec).distinct
    }
  }
  def named(n: String, e: Rep): Rep = {
    ctx(n) = e
    println(n+" := "+e+"\n")
    e
  }
  def linset(e: List[Rep]): Rep = e
  def bind(e1: Rep, e: Rep => Rep): Rep = e(e1)
  def groupby(e1: Rep, g: List[String], v: List[String]): Rep = e1
  def reduceby(e1: Rep, g: List[String], v: List[String]): Rep = groupby(e1, g, v)

  def lookup(lbl: Rep, dict: Rep): Rep = dict match {
    case (flat, tdict) => flat match {
      case (head:Map[_,_]) :: tail => flat
      case _ => flat.asInstanceOf[List[(_,_)]].withFilter(_._1 == lbl).map(_._2)
    }
    case _ => dict // (flat, ())
  }
  def emptydict: Rep = ()
  def bagdict(lbl: LabelType, flat: Rep, dict: Rep): Rep = (flat.asInstanceOf[List[_]].map(v => (lbl, v)), dict)
  def tupledict(fs: Map[String, Rep]): Rep = fs
  def dictunion(d1: Rep, d2: Rep): Rep = d1 // TODO

  def select(x: Rep, p: Rep => Rep): Rep = { 
    x.asInstanceOf[List[_]].filter(p.asInstanceOf[Rep => Boolean])
  }
  def reduce(e1: Rep, f: List[Rep] => Rep, p: List[Rep] => Rep): Rep = {
    e1.asInstanceOf[List[_]].map(v2 => f(tupleVars(v2))) 
  }
  def unnest(e1: Rep, f: List[Rep] => Rep, p: List[Rep] => Rep, value: List[Rep] => Rep): Rep = {
    e1.asInstanceOf[List[_]].flatMap{
      v =>
        val v1 = tupleVars(v)
        f(v1).asInstanceOf[List[_]].map{ v2 => 
          val nv = v1 :+ v2
          if (p.asInstanceOf[Rep => Boolean](nv)) { value(nv) } else { Nil }
      }
    }
  }
  def join(e1: Rep, e2: Rep, p1: List[Rep] => Rep, p2: Rep => Rep, proj1: List[Rep] => Rep, proj2: Rep => Rep): Rep = {
    outerjoin(e1, e2, p1, p2, proj1, proj2)
  } 
  def nest(e1: Rep, f: List[Rep] => Rep, e: List[Rep] => Rep, p: List[Rep] => Rep, g: List[Rep] => Rep, dk: Boolean): Rep = {
    val grps = e1.asInstanceOf[List[_]].groupBy(v => f(tupleVars(v)))
    val res = e1 match {
      case Nil => e(Nil) match { case i:Int => 0; case _ => Nil }
      case head :: tail => e(head.asInstanceOf[List[_]]) match {
        case i:Int => 
          grps.map(x1 => x1._1.asInstanceOf[List[_]] :+ x1._2.foldLeft(0)((acc, v1) => { 
            // this should be if g(x2) != None 
            if (g(v1.asInstanceOf[List[_]]) != None && p(v1.asInstanceOf[List[_]]).asInstanceOf[Boolean]) { 
              val nacc = e(v1.asInstanceOf[List[_]]) match { case Nil => 0; case c => c.asInstanceOf[Int] }
              acc + nacc 
            } else { acc } 
           })).toList
        case i:Double =>
          grps.map(x1 => x1._1.asInstanceOf[List[_]] :+ x1._2.foldLeft(0.0)((acc, v1) => { 
            // this should be if g(x2) != None  
            if (g(v1.asInstanceOf[List[_]]) != None && p(v1.asInstanceOf[List[_]]).asInstanceOf[Boolean]) { 
              acc + e(v1.asInstanceOf[List[_]]).asInstanceOf[Double] } else { acc } 
           })).toList
  case _ => 
          grps.map(x1 => x1._1.asInstanceOf[List[_]] :+ x1._2.flatMap(v => { 
            val v2 = tupleVars(v)
            if (!g(v2).asInstanceOf[List[_]].contains(None) && p(v2).asInstanceOf[Boolean]) { 
              List(e(v2)) 
            } else { Nil } })).toList
        }
     }
     res
  }
  def outerunnest(e1: Rep, f: List[Rep] => Rep, p: List[Rep] => Rep, value: List[Rep] => Rep): Rep = {
    e1.asInstanceOf[List[_]].flatMap{
      v =>
        val v1 = tupleVars(v)
        f(v1).asInstanceOf[List[_]].map{ v2 => 
          val nv = v1 :+ v2
          if (p(nv).asInstanceOf[Boolean]) { value(nv) } else { v1 :+ None }
      }
    }
  }
  def outerjoin(e1: Rep, e2: Rep, p1: List[Rep] => Rep, p2: Rep => Rep, proj1: List[Rep] => Rep, proj2: Rep => Rep): Rep = {
    val hm = e1.asInstanceOf[List[_]].groupBy(v => p1(tupleVars(v)))
    e2.asInstanceOf[List[_]].flatMap(v2 => hm.get(p2(v2)) match {
      case Some(v1) => v1.map(v => tupleVars(v) :+ v2)
      case _ => Nil
    })
  }

  def flatdict(e1: Rep): Rep = e1
  def groupdict(e1: Rep): Rep = e1
  
  // keys and flattens input tuples
  def tupleVars(k: Any): List[Rep] = k match {
    case c:CaseClassRecord => List(k).asInstanceOf[List[Rep]]
    case c:Rec => List(k).asInstanceOf[List[Rep]]
    case _ => k.asInstanceOf[List[Rep]]
  }
  
  def mapVars(k: Any): Map[Any, Any] = k match {
    case l:List[_] if l.head.isInstanceOf[RecordValue] =>
      l.asInstanceOf[List[RecordValue]].map{ case rv => (rv.map("k"), rv.map("v")) }.toMap
    case _ => k.asInstanceOf[List[(Any, Any)]].toMap
  }

}
