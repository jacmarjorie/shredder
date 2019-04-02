package shredding.calc

import org.scalatest.FunSuite
import shredding.core._

class CalcTest extends FunSuite with CalcImplicits{
   
   def print(e: CompCalc) = println(calc.quote(e.asInstanceOf[calc.CompCalc]))

   val nstype = TupleType("c" -> IntType)
   val stype = TupleType("a" -> IntType, "b" -> StringType, "c" -> BagType(nstype))
   val x1 = VarDef("x", stype)
   val y1 = VarDef("y", nstype)
   val relationS = InputR("S", List(
                    Map("a" -> "42", "b" -> "Milos", "c" -> List(Map("c" -> "42"), Map("c" -> "42"), Map("c" -> "30"))),
                    Map("a" -> "69", "b" -> "Michael", "c" -> List(Map("c" -> "30"), Map("c" -> "69"), Map("c" -> "50"))),
                    Map("a" -> "34", "b" -> "Jaclyn", "c" -> List(Map("c" -> "42"), Map("c" -> "34"), Map("c" -> "30"))),
                    Map("a" -> "42", "b" -> "Thomas", "c" -> List(Map("c" -> "42"), Map("c" -> "34"), Map("c" -> "69"))),
                  ), BagType(stype))

  test("Calc.Constant"){
    assert(Constant("one", StringType).x == "one")
    assert(Constant("1", IntType).x == "1")
    assert(Constant("1", IntType).normalize == Constant("1", IntType))
  }

  test("Calc.Var"){
    val x = VarDef("x", IntType)
    assert(Var(x) == PrimitiveVar(x))
    val v1 = VarDef("x", StringType)
    assert(Var(v1) == PrimitiveVar(v1))
    assert(Var(v1).normalize == PrimitiveVar(v1))
    
    val v2 = VarDef("x", TupleType("a" -> IntType, "b" -> StringType))
    assert(Var(v2) == TupleVar(v2))
    assert(Var(v2).normalize == TupleVar(v2))
    
    val v3 = VarDef("x", BagType(TupleType("a" -> IntType, "b" -> StringType)))
    assert(Var(v3) == BagVar(v3))
    assert(Var(v3).normalize == BagVar(v3))
  }

  test("Calc.ProjToPrimitive"){
    val proj1 = Proj(Tup("a" -> Constant(1, IntType)), "a")
    assert(proj1.normalize == Constant(1, IntType))
  }

  test("Calc.Conditional"){
    val c1 = Conditional(OpEq, Constant("one", StringType), Constant("one", StringType))
    assert(c1.tp == BoolType)
    assert(c1.normalize == c1)

    // are we supporting bag equality?
    val c2 = Conditional(OpEq, Sng(Tup("a" -> Constant("one", StringType))), Sng(Tup("b" -> Constant("1", IntType))))
    assert(c2.tp == BoolType)
    assert(c2.normalize == c2)

    val c3 = NotCondition(c1)
    assert(c3.tp == BoolType)
    assert(c3.normalize == c3)
    assertThrows[AssertionError](NotCondition(Constant("one", StringType)))

    val c4 = AndCondition(c1, c2)
    assert(c4.tp == BoolType)
    assert(c4.normalize == c4)
    assertThrows[AssertionError](AndCondition(Constant("one", StringType), c2))
    assertThrows[AssertionError](AndCondition(c2, Constant("one", StringType)))

    val c5 = OrCondition(c1, c2)
    assert(c5.tp == BoolType)
    assert(c5.normalize == c5)
    assertThrows[AssertionError](OrCondition(Constant("one", StringType), c2))
    assertThrows[AssertionError](OrCondition(c2, Constant("one", StringType)))

  }

  test("Calc.ProjToBag"){
    val proj1 = Proj(Tup("w" -> Sng(Tup("w" -> Constant(1, IntType)))), "w")
    assert(proj1.normalize == Sng(Tup("w" -> Constant(1, IntType))))
    
    val v = VarDef("x", TupleType("a" -> StringType))
    val gen = Generator(v, Sng(Tup(Map("a" -> Constant("one", StringType)))))
    val proj2 = Proj(Tup("w" -> Constant(1, IntType), "y" -> BagComp(TupleVar(v), List(gen))), "y")
    assert(proj2.normalize == BagComp(TupleVar(v), List(gen)))
  }
  
  test("Calc.Generator"){
    val v = VarDef("x", TupleType("a" -> StringType))
    val gen = Generator(v, Sng(Tup(Map("a" -> Constant("one", StringType)))))
    assert(gen.tp == BagType(TupleType("a" -> StringType)))
    assert(gen.normalize == BindTuple(v, Tup(Map("a" -> Constant("one", StringType)))))
    assertThrows[AssertionError](
      Generator(VarDef("x", TupleType("a" -> IntType)), Sng(Tup(Map("a" -> Constant("one", StringType))))))

  }
  
  test("Calc.Bind"){
    val vb1 = VarDef("x", TupleType("a"-> StringType))
    val bnd1 = Bind(vb1, Tup(Map("a" -> Constant("one", StringType))))
    assert(bnd1 == BindTuple(vb1, Tup(Map("a" -> Constant("one", StringType)))))
    assert(bnd1.tp == TupleType("a"-> StringType))
    assert(bnd1.normalize == BindTuple(vb1, Tup(Map("a" -> Constant("one", StringType)))))
    assertThrows[AssertionError](
      BindTuple(VarDef("x", IntType), Tup(Map("a" -> Constant("one", StringType))))
    )

    val vb2 = VarDef("x", IntType)
    val bnd2 = Bind(vb2, Constant("1", IntType))
    assert(bnd2 == BindPrimitive(vb2, Constant("1", IntType)))
    assert(bnd2.tp == IntType)
    assert(bnd2.normalize == BindPrimitive(vb2, Constant("1", IntType)))
    assertThrows[AssertionError](
      BindPrimitive(VarDef("x", TupleType("a" -> StringType)), Constant("1", IntType))
    )
  }


  test("Calc.Sng"){

    val zero = Zero()
    assert(zero.normalize == zero)

    val esng = Sng(Tup(Map[String, TupleAttributeCalc]()))
    assert(esng.tp == zero.tp)
    assert(esng.normalize == Zero())

    val sng = Sng(Tup("a" -> Constant("one", StringType)))
    assert(sng.tp == BagType(TupleType("a" -> StringType)))
    assert(sng.normalize == sng)
    
  }
 
   /**
    * Basic generator
    */
  test("Calc.BagComp"){
   
    // basic generator comprehension
    // { x | x <- S } normalized to itself 
    val bc1 = BagComp(Tup("w" -> Proj(TupleVar(x1), "b")), List(Generator(x1, relationS)))
    assert(bc1.tp == BagType(TupleType("w" -> StringType)))
    assert(bc1.normalize == bc1)
  }
  
  test("Calc.IfStmt"){
    val ifs = IfStmt(Conditional(OpEq, Constant("one", StringType), Constant("two", StringType)),
                     Sng(Tup("w" -> Constant("one", StringType))), Option(Sng(Tup("w" -> Constant("two", StringType)))))
    assert(ifs.tp == BagType(TupleType("w" -> StringType)))

    val ifs2 = IfStmt(Conditional(OpEq, Constant("one", StringType), Constant("two", StringType)),
                     Sng(Tup("w" -> Constant("one", StringType))), None)
    assert(ifs.tp == BagType(TupleType("w" -> StringType)))

  }

  /**
    * N4
    * { ( w := x.w ) | x <- if ( "one" = "two" ) then sng((w := "one")) else sng((w := "two")) }
    * { ( w := "one" ) | ( "one" = "two" ) } U { ( w := "two" ) | not( "one" = "two" ) }
    *
    */
  test("Calc.BagComp.normalize.IfGenerator"){
    val ifs = IfStmt(Conditional(OpEq, Constant("one", StringType), Constant("two", StringType)),
                     Sng(Tup("w" -> Constant("one", StringType))), Option(Sng(Tup("w" -> Constant("two", StringType)))))

    val x2 = VarDef("x", TupleType("w" -> StringType))
    val ifs3 = BagComp(Tup("w" -> Proj(TupleVar(x2), "w")), List(Generator(x2, ifs)))
    assert(ifs3.tp == ifs.tp)
    val ifs3n = Merge(BagComp(Tup("w" -> Constant("one", StringType)), List(
                  Conditional(OpEq, Constant("one", StringType), Constant("two", StringType)))),
                    BagComp(Tup("w" -> Constant("two", StringType)), List(
                        NotCondition(Conditional(OpEq, Constant("one", StringType), Constant("two", StringType)))))) 
    assert(ifs3.normalize == ifs3n)
  }

   /**
     * N5
     * { ( w := x.a ) | x <- { } } 
     * { }
     */
  test("Calc.BagComp.normalize.EmptyGenerator"){
    val x = VarDef("x", TupleType("a" -> StringType))
    val y = VarDef("y", stype)
    val z = VarDef("z", TupleType())
    val gen1 = Generator(x, Sng(Tup(Map("a" -> Constant("one", StringType)))))
    val gen2 = Generator(y, relationS)
    val gen3 = Generator(z, Zero())
    val pred1 = Conditional(OpEq, Proj(TupleVar(x), "a"), Constant("one", StringType))

    val cq4 = BagComp(Tup("w" -> Proj(TupleVar(x), "a")), List(gen3))
    assert(cq4.normalize == Zero())

    // N5, zero regardless of qualifiers in the comprehension
    val cq5 = BagComp(Tup("w" -> Proj(TupleVar(x), "a")), List(gen1, gen2, gen3, pred1))
    assert(cq5.normalize == Zero())
  }

 test("Calc.BagComp.normalized.Singleton"){
    val x = VarDef("x", TupleType("a" -> StringType))
    val y = VarDef("y", stype)
    val gen1 = Generator(x, Sng(Tup(Map("a" -> Constant("one", StringType)))))
    val gen2 = Generator(y, relationS)
    val pred1 = Conditional(OpEq, Proj(TupleVar(x), "a"), Constant("one", StringType))
    val bind1 = Bind(x, Tup(Map("a" -> Constant("one", StringType))))
   
    // N6
    // { v0 | x <- { ( a := "one" ) }, v0 := ( w := x.a ) }
    // { v0 | x := ( a:= "one" ), v0 := ( w:= x.a ) }
    // { "one" | }
    val v = VarDef("v", TupleType("w" -> StringType))
    val cq1a = BagComp(TupleVar(v), List(gen1, Bind(v, Sng(Tup("w" -> Proj(TupleVar(x), "a"))))))
    assert(cq1a.normalize == Sng(Tup("w" -> Constant("one", StringType))))

    // { ( w := x.a ) | x <- { ( a := "one" ) } }
    // { ( w := x.a ) | x := ( a: = "one" ) } } 
    val cq1 = BagComp(Tup("w" -> Proj(TupleVar(x), "a")), List(gen1))
    assert(cq1.normalize == Sng(Tup("w" -> Constant("one", StringType))))

    // test this case
    // N6, preserves predicate
    // { ( w := x.a ) | x <- { ( a := "one" ) }, x.a = "one" }
    // { ( w := x.a ) | x := ( a: = "one" ), x.a = "one" }
    // val cq2 = BagComp(Tup("w" -> Proj(TupleVar(x), "a")), List(gen1, pred1))
    // assert(cq2.normalize == BagComp(Tup("w" -> Proj(TupleVar(x), "a")), List(bind1, pred1)))

    // N6, preserves qualifiers before and after
    // { ( w := x.a ) | y <- R, x <- { ( a := "one" ) }, x.a = "one" }
    // { ( w := x.a ) | y <- R, x := ( a: = "one" ), x.a = "one" }
    val cq3 = BagComp(Tup("w" -> Proj(TupleVar(x), "a")), List(gen2, gen1, pred1))
    assert(cq3.normalize == BagComp(Tup("w" -> Constant("one", StringType)), 
          List(gen2, Conditional(OpEq, Constant("one", StringType), Constant("one", StringType)))))
  }
 
 test("Calc.BagComp.normalized.2"){ 
    val x = VarDef("x", TupleType("a" -> StringType))
    val y = VarDef("y", stype)
    val z = VarDef("z", TupleType())
    val gen1 = Generator(x, Sng(Tup(Map("a" -> Constant("one", StringType)))))
    val gen2 = Generator(y, relationS)
    val gen3 = Generator(z, Zero())
    val pred1 = Conditional(OpEq, Proj(TupleVar(x), "a"), Constant("one", StringType))
    val bind1 = Bind(x, Tup(Map("a" -> Constant("one", StringType))))

    val x2 = VarDef("x2",  TupleType("a" -> StringType))
    val x3 = VarDef("x3", TupleType("a" -> StringType))
    val gen4 = Generator(x3, BagComp(Tup("a" -> Proj(TupleVar(x),"a")), List(gen1)))
    // N8
    // { ( w := x0.a ) |  x0 <- { ( a := x0.a ) |  x0 <- {( a := "one" )}  }  }
    // { 
    val cq6 = BagComp(Tup("w" -> Proj(TupleVar(x3), "a")), List(gen4))
    //println(calc.quote(cq6.asInstanceOf[calc.CompCalc]))
    //println(calc.quote(cq6.normalize.asInstanceOf[calc.CompCalc]))

    //assert(cq6.normalize == BagComp(Tup("w" -> Proj(TupleVar(x2), "a")), 
    //                          List(bind1, Bind(x2, Tup("a" -> Proj(TupleVar(x),"a")))))) 

    // N8, a more complex expression and preserves qualifiers
    // { ( w := x13.a ) |  x13 <- { ( a := y11.b ) |  y11 <- R  } ,  x13.a = "one"  }
    // { ( w := x13.a ) |  y11 <- R , x13 := ( a := y11.b ),  x13.a = "one"  }
    val gen5 = Generator(x2, BagComp(Tup("a" -> Proj(TupleVar(y), "b")), List(gen2)))
    val pred2 = Conditional(OpEq, Proj(TupleVar(x2), "a"), Constant("one", StringType))
    val cq7 = BagComp(Tup("w"-> Proj(TupleVar(x2), "a")), List(gen5, pred2))
    print(cq7)
    print(cq7.normalize)
    //assert(cq7.normalize == BagComp(Tup("w"-> Proj(TupleVar(x2), "a")), 
    //                          List(gen2, Bind(x2, Tup("a" -> Proj(TupleVar(y), "b"))), pred2)))

    // N8, an even more complex expression and preserves qualifiers inside and out
    // { ( w := x13.a ) |  x13 <- { ( a := y11.b ) |  y11 <- R  } ,  x13.a = "one"  }
    // { ( w := x13.a ) |  y11 <- R , x13 := ( a := y11.b ),  x13.a = "one"  }
    val pred3 = Conditional(OpGt, Proj(TupleVar(y), "a"), Constant("15", IntType))
    val gen6 = Generator(x2, BagComp(Tup("a" -> Proj(TupleVar(y), "b")), List(gen2, pred3)))
    val cq8 = BagComp(Tup("w"-> Proj(TupleVar(x2), "a")), List(gen6, pred2))
    //assert(cq8.normalize == BagComp(Tup("w"-> Proj(TupleVar(x2), "a")), 
    //                          List(gen2, pred3, Bind(x2, Tup("a" -> Proj(TupleVar(y), "b"))), pred2)))

  }

  test("Calc.Merge"){ 

    val m1 = Merge(Sng(Tup("a" -> Constant("one", StringType))), Sng(Tup("a" -> Constant("two", StringType))))
    assert(m1.tp == BagType(TupleType("a" -> StringType)))
    assert(m1.tp == Sng(Tup("a" -> Constant("one", StringType))).tp) 
    assert(m1.normalize == m1) 

    // N7, generator with a merge
    val x2 = VarDef("x",  TupleType("a" -> StringType))
    val m2 = BagComp(Tup("w" -> Proj(TupleVar(x2), "a")), List(Generator(x2, m1)))
    assert(m2.normalize == Merge(Sng(Tup("w" -> Constant("one", StringType))),
                                  Sng(Tup("w" -> Constant("two", StringType)))))
  }


  test("Calc.Tup"){
    
    val t1 = Tup("a" -> Constant("one", StringType), "b" -> Constant("two", StringType))
    assert(t1.tp == TupleType("a" -> StringType, "b" -> StringType))
    assert(t1.normalize == t1)

    val t2 = Tup("a" -> Constant("one", StringType), "b" -> Sng(Tup("b" -> Constant("two", StringType))))
    assert(t2.tp == TupleType("a" -> StringType, "b" -> BagType(TupleType("b" -> StringType))))
    assert(t2.normalize == t2)
    
  }

}