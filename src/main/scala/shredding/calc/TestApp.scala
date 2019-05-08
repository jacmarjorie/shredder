package shredding.calc

import shredding.Utils.Symbol
import shredding.core._
import shredding.runtime.{Context, Evaluator, ScalaShredding}
import shredding.nrc._

object TestApp extends App with 
  ShredPipelineRunner with CalcTranslator with Evaluator with ScalaShredding {
  
  override def main(args: Array[String]){
    //runM1()
    //runM2()
    //runM3()
    //runM4()
    run1()
    //run2()
    //run3()
    //run4()
  } 
 
  def runM1(){
    val btp = TupleType("t" -> IntType)
    val xtp = TupleType("a" -> BagType(btp), "b" -> IntType)
    val xdef = VarDef("x", xtp)
    val xvalue = Map("a" -> List(Map("t" -> 1), Map("t" -> 2)), "b" -> 3)
    
    val q = TupleVarRef(xdef)("a")
    val sq = ShredPipeline.runShred(q)

    val ctx = new Context()
    ctx.add(xdef, xvalue)
    
    println(eval(q, ctx))

    val shredX = shred(xvalue, xtp)
    println("xF := ")
    println(shredX.flat)
    println("xD := ")
    println(shredX.dict)
    val xd = VarDef(dictName(xdef.name), shredX.dict.tp)
    val xf = VarDef(flatName(xdef.name), shredX.flatTp) 
    val labelTp = LabelType(flatName(xdef.name) -> shredX.flatTp)//, dictName(xdef.name) -> shredX.dict.tp)
    val initCtx = VarDef(initCtxName, BagType(TupleType("lbl" -> LabelType(Map(xf.name -> shredX.flatTp)))))
    ctx.add(xf, shredX.flat)
    ctx.add(xd, shredX.dict)
    ctx.add(initCtx, List(Map("lbl" -> ROutLabel(Map(xf -> shredX.flat)))))
    println(sq)
    //println(eval(sq, ctx).asInstanceOf[List[Any]].mkString(""))

  }

  def runM2(){
    val btp = TupleType("t" -> IntType)
    val rtp = TupleType("a" -> BagType(btp), "b" -> IntType)
    val relationR = BagVarRef(VarDef("R", BagType(rtp)))
    val xdef = VarDef("x", rtp)
    val q = ForeachUnion(xdef, relationR, 
              BagProject(TupleVarRef(xdef), ("a")))
    ShredPipeline.runShred(q)
  }

  def runM3(){
    val btp = TupleType("t" -> IntType)
    val xtp = TupleType("a" -> BagType(btp), "b" -> IntType)
    val xdef = VarDef("x", xtp)
    val q = Singleton(Tuple("a'" -> TupleVarRef(xdef)("a")))
    ShredPipeline.runShred(q)
  }

  def runM4(){
    val ctp = TupleType("t" -> IntType)
    val xtp = TupleType("c" -> IntType)
    val ytp = TupleType("a" -> BagType(ctp), "c" -> IntType)
    val relationR = BagVarRef(VarDef("R", BagType(ytp)))
    val xdef = VarDef("x", xtp)
    val ydef = VarDef("y", ytp)
    val f = ForeachUnion(ydef, relationR, 
              IfThenElse(Cond(OpEq, TupleVarRef(ydef)("c"), TupleVarRef(xdef)("c")),
                BagProject(TupleVarRef(ydef), "a")))
    val q = Singleton(Tuple("a'" -> TupleVarRef(xdef)("c"), "b'" -> f))
    ShredPipeline.runShred(q)
  }

  def run1() {
    
    val itemTp = TupleType("a" -> IntType, "b" -> StringType)
    val relationR = BagVarRef(VarDef("R", BagType(itemTp)))
    val relationRValue = List(
        Map("a" -> 42, "b" -> "Milos"),
        Map("a" -> 69, "b" -> "Michael"),
        Map("a" -> 34, "b" -> "Jaclyn"),
        Map("a" -> 42, "b" -> "Thomas")
    )

    val ctx = new Context()
    ctx.add(relationR.varDef, relationRValue) 

    val xdef = VarDef(Symbol.fresh("x"), itemTp)
    val q = ForeachUnion(xdef, relationR, 
              IfThenElse(Cond(OpGt, TupleVarRef(xdef)("a"), Const(40, IntType)), 
                Singleton(Tuple("w" -> TupleVarRef(xdef)("b")))))
    val ucq = Pipeline.run(q)
  }

  def run1shred() {
    
    val itemTp = TupleType("a" -> IntType, "b" -> StringType)
    val relationR = BagVarRef(VarDef("R", BagType(itemTp)))
    val relationRValue = List(
        Map("a" -> 42, "b" -> "Milos"),
        Map("a" -> 69, "b" -> "Michael"),
        Map("a" -> 34, "b" -> "Jaclyn"),
        Map("a" -> 42, "b" -> "Thomas")
    )

    val ctx = new Context()
    ctx.add(relationR.varDef, relationRValue) 

    val xdef = VarDef(Symbol.fresh("x"), itemTp)
    val q = ForeachUnion(xdef, relationR, Singleton(Tuple("w" -> TupleVarRef(xdef)("b"))))
    val ucq1 = ShredPipeline.run(q)
    val ucq2 = ShredPipeline.runOptimized(q)
  }

  def run2(){
    val itemTp = TupleType("a" -> IntType, "b" -> StringType)
    val relationR = BagVarRef(VarDef("R", BagType(itemTp)))
    val relationRValue = List(
        Map("a" -> 42, "b" -> "Milos"),
        Map("a" -> 69, "b" -> "Michael"),
        Map("a" -> 34, "b" -> "Jaclyn"),
        Map("a" -> 42, "b" -> "Thomas")
    )

    val ctx = new Context()
    ctx.add(relationR.varDef, relationRValue) 
    val x0def = VarDef(Symbol.fresh("x"), itemTp)
    val x1def = VarDef(Symbol.fresh("x"), itemTp)
    val rq1 = ForeachUnion(x0def, relationR,
                ForeachUnion(x1def, relationR,
                  Singleton(Tuple("w1" -> Singleton(TupleVarRef(x0def)), "w2" -> Singleton(TupleVarRef(x1def))))))

    val x2def = VarDef(Symbol.fresh("x"), itemTp)
    val itemTp2 = TupleType("w1" -> BagType(itemTp), "w2" -> BagType(itemTp))
    val x3def = VarDef(Symbol.fresh("x"), itemTp2)
    val rq2 = ForeachUnion(x3def, rq1, ForeachUnion(x2def, relationR,
                Singleton(Tuple("w1" -> Singleton(TupleVarRef(x3def)), "w2" -> Singleton(TupleVarRef(x2def))))))
    val x4def = VarDef(Symbol.fresh("x"), TupleType("w1" -> BagType(itemTp2), "w2" -> BagType(itemTp)))
    val rq3 = ForeachUnion(x4def, rq2, ForeachUnion(x2def, relationR,
                Singleton(Tuple("w1" -> Singleton(TupleVarRef(x4def)), "w2" -> Singleton(TupleVarRef(x2def))))))
    //val ucq1 = ShredPipeline.run(rq3)
    val ucq2 = ShredPipeline.runOptimized(rq3)

  }

  def run3(){
    val dtype = TupleType("dno" -> IntType, "dname" -> StringType)
    val etype = TupleType("dno" -> IntType, "ename" -> StringType)
    val employees = BagVarRef(VarDef("Employees", BagType(etype)))
    val departments = BagVarRef(VarDef("Departments", BagType(dtype)))
    val e = VarDef(Symbol.fresh("e"), etype)
    val d = VarDef(Symbol.fresh("d"), dtype)
    val employeesValue = List(Map("dno" -> 1, "ename" -> "one"),Map("dno" -> 2, "ename"-> "two"),
                            Map("dno" -> 3, "ename" -> "three"),Map("dno" -> 4, "ename" -> "four"))
    val departmentsValue = List(Map("dno" -> 1, "dname" -> "five"),Map("dno" -> 2, "dname" -> "six"),
                              Map("dno" -> 3, "dname" -> "seven"),Map("dno" -> 4, "dname" -> "eight"))
    val ctx = new Context()
    ctx.add(employees.varDef, employeesValue)
    ctx.add(departments.varDef, departmentsValue)
    val q4 = ForeachUnion(d, departments,
              ForeachUnion(e, employees,
                IfThenElse(Cond(OpEq, TupleVarRef(d)("dno"), TupleVarRef(e)("dno")),
                  Singleton(Tuple("D" -> TupleVarRef(d)("dno"), "E" -> Singleton(TupleVarRef(e)))))))
    //val ucq1 = ShredPipeline.run(q4)
    val ucq2 = ShredPipeline.runOptimized(q4)
  }

  def run4(){
      val nested2ItemTp = TupleType(Map("n" -> IntType))

      val nestedItemTp = TupleType(Map(
        "m" -> StringType,
        "n" -> IntType,
        "k" -> BagType(nested2ItemTp)
      ))
      val itemTp = TupleType(Map(
        "h" -> IntType,
        "j" -> BagType(nestedItemTp)
      ))
      val relationR = BagVarRef(VarDef("R", BagType(itemTp)))
      val relationRValues = List(Map(
          "h" -> 42,
          "j" -> List(
            Map(
              "m" -> "Milos",
              "n" -> 123,
              "k" -> List(
                Map("n" -> 123),
                Map("n" -> 456),
                Map("n" -> 789),
                Map("n" -> 123)
              )
            ),
            Map(
              "m" -> "Michael",
              "n" -> 7,
              "k" -> List(
                Map("n" -> 2),
                Map("n" -> 9),
               Map("n" -> 1)
              )
            ),
            Map(
              "m" -> "Jaclyn",
              "n" -> 12,
              "k" -> List(
                Map("n" -> 14),
                Map("n" -> 12)
              )
            )
          )
        ),
        Map(
          "h" -> 69,
          "j" -> List(
            Map(
              "m" -> "Thomas",
              "n" -> 987,
              "k" -> List(
                Map("n" -> 987),
                Map("n" -> 654),
                Map("n" -> 987),
                Map("n" -> 654),
                Map("n" -> 987),
                Map("n" -> 987)
              )
            )
          )
        )
      )
      val ctx = new Context()
      ctx.add(relationR.varDef, relationRValues)

      val xdef = VarDef("x", itemTp)
      val xref = TupleVarRef(xdef)
      val wdef = VarDef("w", nestedItemTp)
      val wref = TupleVarRef(wdef)
      val ndef = VarDef("y", TupleType("n" -> IntType))

      val q4 = ForeachUnion(xdef, relationR,
        Singleton(Tuple(
          "o5" -> xref("h"),
          "o6" ->
            ForeachUnion(wdef, BagProject(xref, "j"),
              Singleton(Tuple(
                "o7" -> wref("m"),
                "o8" -> Total(BagProject(wref, "k"))
              ))
            )
        )))

      val ucq = ShredPipeline.runOptimized(q4)
     }

}