package framework.plans

import framework.common._

/**
  * Normalization for intermediate calculus of the plan language. 
  * TODO: re-evaluate the necessity of this with updated NRC
  */
class BaseNormalizer(val letOpt: Boolean = false) extends BaseCompiler {

  // reduce conditionals
  override def equals(e1: Rep, e2: Rep): Rep = (e1, e2) match {
    case (Constant(x1), Constant(x2)) => constant(x1 == x2)
    case _ => super.equals(e1, e2)
  }

  override def not(e1: Rep): Rep = e1 match {
    case Constant(true) => super.constant(false)
    case Constant(false) => super.constant(true)
    case _ => super.not(e1)
  }

  override def and(e1: Rep, e2: Rep): Rep  = (e1, e2) match {
    case (Constant(true), Constant(true)) => super.constant(true)
    case (Constant(false), _) => super.constant(false)
    case (_, Constant(false)) => super.constant(false)
    case (Constant(true), e3) => e3
    case (e3, Constant(true)) => e3
    case _ => super.and(e1, e2)
  }

  override def or(e1: Rep, e2: Rep): Rep  = (e1, e2) match {
    case (Constant(false), Constant(false)) => super.constant(false)
    case (Constant(true), _) => super.constant(true)
    case (_, Constant(true)) => super.constant(true)
    // test this case
    case (e1, e2) if e1.vstr == e2.vstr => e1
    case _ => super.or(e1, e2)
  }

  override def lookup(lbl: Rep, dict: Rep): Rep = dict match {
    case BagCDict(lbl2, flat2, dict2) if (lbl2 == lbl.tp) => flat2
    case BagCDict(lbl2, flat2, dict2) => super.lookup(lbl,dict)
    case _ => super.lookup(lbl, dict)
  }
  
  /** need to update the types in a sequence **/
  override def linset(es: List[Rep]): Rep = {
    LinearCSet(es.flatMap{
      case LinearCSet(fs) => fs
      case fs => List(fs)
    })
  } 

  // N1, N2
  override def bind(e1: Rep, e: Rep => Rep): Rep = {
    if (letOpt) super.bind(e1, e) 
    else e(e1)
  }

  // N3 (a := e1, b: = e2).a = e1
  override def project(e1: Rep, f: String): Rep = e1 match {
    case t:Tuple => t(f.toInt)
    case t:Record => t(f)
    //case t:Label => t(f)
    case t:TupleCDict => t(f)
    case t:BagCDict => t(f)
    case _ => super.project(e1, f)
  }

  override def ifthen(cond: Rep, e1: Rep, e2: Option[Rep]): Rep = cond match {
    case Constant(true) => e1 match {
      case Sng(t) if t.tp.isInstanceOf[PrimitiveType] => t
      case _ => e1
    }
    case Constant(false) => e2 match {
      case Some(a @ Sng(t)) if t.tp.isInstanceOf[PrimitiveType] => t 
      case Some(a) => a 
      case _ => super.emptysng
    }
    case _ => e1 match {
      case Sng(t) if t.tp == IntType => super.ifthen(cond, t, Some(Constant(0)))
      case Sng(t) if t.tp == DoubleType => super.ifthen(cond, t, Some(Constant(0.0))) 
      case If(cond2, e3, e4) => e2 match {
        case None => ifthen(and(cond, cond2), e3, e4)
        case Some(e5) => ifthen(and(cond, not(cond2)), e4.get, Some(ifthen(and(cond, cond2), e3, e2)))
      }
      case _ => super.ifthen(cond, e1, e2)
    }
  }

  override def sng(x: Rep): Rep = x match {
    case Record(fs) => fs.get("_1") match {
      case Some(CUnit) => fs.get("_2").get 
      case _ => super.sng(x)
    }
    case Sng(t) => sng(t)
    case y if x.tp.isInstanceOf[BagCType] => x
    case _ => super.sng(x)
  }

  def normalizeIf(c: Rep): Rep = c match {
    case Comprehension(e2, v2, p2, If(cond, e4, None)) => 
      Comprehension(e2, v2, and(p2, cond), e4)
    case _ => c
  }
 
  // { e(v) | v <- e1, p(v) }
  // where fegaras and maier does: { e | q, v <- e1, s }
  // this has { { { e | s } | v <- e1 } | q }
  // N10 is automatically handled in this representation
  override def comprehension(e1: Rep, p: Rep => Rep, e: Rep => Rep): Rep = {
    e1 match {
      case If(cond, e3, e4 @ Some(a)) => //N4
        If(cond, comprehension(e3, p, e), Some(comprehension(a, p, e)))
      case EmptySng => EmptySng // N5
      case Sng(t) => ifthen(p(t), sng(e(t))) // N6
      case Merge(e1, e2) => Merge(comprehension(e1, p, e), comprehension(e2, p, e))  //N7
      case Variable(name,_) => ifthen(p(e1), Sng(e(e1))) // input relation
      
      // { e(v) | v <- { e3 | v2 <- e2, p2 }, p(v) }
      // { { e(v) | v <- e3 } | v2 <- e2, p2 }
      case Comprehension(e2, v2, p2, e3:If) => normalizeIf(e1)
        
      // normalize let's per usual
      case Comprehension(e2, v2, p2, e3) if (!letOpt) =>  
          Comprehension(e2, v2, p2, normalizeIf(comprehension(e3, p, e)))

      // how to normalize in a nested??
      case CReduceBy(in, v, ks, vs) if (!letOpt) => 
        val nc = comprehension(in, (i: Rep) => Constant(true), (i: Rep) => i)
        val v2 = Variable.freshFromBag(nc.tp)
        CReduceBy(nc, v2, ks, vs)

      case _ => // standard case (return self)
        val v = Variable.freshFromBag(e1.tp)
        e(v) match {
          case If(cond, e4, None) => Comprehension(e1, v, and(p(v), cond), e4)
          case ev => 
            Comprehension(e1, v, p(v), ev)
        }

      }
    }

}
