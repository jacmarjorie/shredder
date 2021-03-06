package framework.nrc

import framework.common.VarDef
import framework.runtime.{Evaluator, RuntimeContext, ScalaPrinter, ScalaShredding}
import framework.examples.tpch
import framework.examples.tpch.TPCHSchema
import framework.examples.genomic

object TestMaterialization extends App
  with MaterializeNRC
  with Shredding
  with ScalaShredding
  with ScalaPrinter
  with Materialization
  with Printer
  with Evaluator
  with Optimizer {

  def run(program: Program): Unit = {
    println("Program: \n" + quote(program) + "\n")

    val shredded = shred(program)
    println("Shredded program: \n" + quote(shredded) + "\n")

    val optShredded = optimize(shredded)
    println("Shredded program optimized: \n" + quote(optShredded) + "\n")

    val materializedProgram = materialize(optShredded, eliminateDomains = true)
    println("Materialized program: \n" + quote(materializedProgram.program) + "\n")

    val unshredded = unshred(optShredded, materializedProgram.ctx)
    println("Unshredded program: \n" + quote(unshredded) + "\n")

    val lDict = List[Map[String, Any]](
      Map("l_orderkey" -> 1, "l_partkey" -> 42, "l_suppkey" -> 789, "l_quantity" -> 7.0)
    )
    val pDict = List[Map[String, Any]](
      Map("p_partkey" -> 42, "p_name" -> "Kettle", "p_retailprice" -> 12.45)
    )
    val cDict = List[Map[String, Any]](
      Map("c_custkey" -> 10, "c_name" -> "Alice")
    )
    val oDict = List[Map[String, Any]](
      Map("o_orderkey" -> 1, "o_custkey" -> 10, "o_orderdate" -> 20200317)
    )
    val sDict = List[Map[String, Any]](
      Map("s_suppkey" -> 789, "s_name" -> "Supplier#1")
    )

    val ctx = new RuntimeContext
    ctx.add(VarDef(inputBagName("L__D"), TPCHSchema.lineittype), lDict)
    ctx.add(VarDef(inputBagName("P__D"), TPCHSchema.parttype), pDict)
    ctx.add(VarDef(inputBagName("C__D"), TPCHSchema.customertype), cDict)
    ctx.add(VarDef(inputBagName("O__D"), TPCHSchema.orderstype), oDict)
    ctx.add(VarDef(inputBagName("S__D"), TPCHSchema.suppliertype), sDict)

    println("Program eval: ")
    eval(materializedProgram.program, ctx)
    materializedProgram.program.statements.foreach { s =>
      println("  " + s.name + " = " + ctx(VarDef(s.name, s.rhs.tp)))
    }

    println("Unshredded program eval: ")
    eval(unshredded, ctx)
    program.statements.foreach { s =>
      println("  " + s.name + " = " + ctx(VarDef(s.name, s.rhs.tp)))
    }
  }

  def runSequential(): Unit = {
    val q1 = tpch.Test2.program.asInstanceOf[Program]

    println("Program: \n" + quote(q1) + "\n")

    val (shredded, shreddedCtx) = shredCtx(q1)
    println("Shredded program: \n" + quote(shredded) + "\n")

    val optShredded = optimize(shredded)
    println("Shredded program optimized: \n" + quote(optShredded) + "\n")

    val materializedProgram = materialize(optShredded, eliminateDomains = true)
    println("Materialized program: \n" + quote(materializedProgram.program) + "\n")

    val unshredded = unshred(optShredded, materializedProgram.ctx)
    println("Unshredded program: \n" + quote(unshredded) + "\n")

    val lDict = List[Map[String, Any]](
      Map("l_orderkey" -> 1, "l_partkey" -> 42, "l_suppkey" -> 789, "l_quantity" -> 7.0)
    )
    val pDict = List[Map[String, Any]](
      Map("p_partkey" -> 42, "p_name" -> "Kettle", "p_retailprice" -> 12.45)
    )
    val cDict = List[Map[String, Any]](
      Map("c_custkey" -> 10, "c_name" -> "Alice")
    )
    val oDict = List[Map[String, Any]](
      Map("o_orderkey" -> 1, "o_custkey" -> 10, "o_orderdate" -> 20200317)
    )
    val sDict = List[Map[String, Any]](
      Map("s_suppkey" -> 789, "s_name" -> "Supplier#1")
    )

    val ctx = new RuntimeContext
    ctx.add(VarDef(inputBagName("L__D"), TPCHSchema.lineittype), lDict)
    ctx.add(VarDef(inputBagName("P__D"), TPCHSchema.parttype), pDict)
    ctx.add(VarDef(inputBagName("C__D"), TPCHSchema.customertype), cDict)
    ctx.add(VarDef(inputBagName("O__D"), TPCHSchema.orderstype), oDict)
    ctx.add(VarDef(inputBagName("S__D"), TPCHSchema.suppliertype), sDict)

    println("Program eval: ")
    eval(materializedProgram.program, ctx)
    materializedProgram.program.statements.foreach { s =>
      println("  " + s.name + " = " + ctx(VarDef(s.name, s.rhs.tp)))
    }

    println("Unshredded program eval: ")
    eval(unshredded, ctx)
    q1.statements.foreach { s =>
      println("  " + s.name + " = " + ctx(VarDef(s.name, s.rhs.tp)))
    }

    val q4 = tpch.Query4.program.asInstanceOf[Program]

    println("Program: \n" + quote(q4) + "\n")

    val (shredded4, _) = shredCtx(q4, shreddedCtx)
    println("Shredded program: \n" + quote(shredded4) + "\n")

    val optShredded4 = optimize(shredded4)
    println("Shredded program optimized: \n" + quote(optShredded4) + "\n")

    val materializedProgram4 = materialize(optShredded4, materializedProgram.ctx, eliminateDomains = true)
    println("Materialized program: \n" + quote(materializedProgram4.program) + "\n")

    val unshredded4 = unshred(optShredded4, materializedProgram4.ctx)
    println("Unshredded program: \n" + quote(unshredded4) + "\n")

    println("Program eval: ")
    eval(materializedProgram4.program, ctx)
    materializedProgram4.program.statements.foreach { s =>
      println("  " + s.name + " = " + ctx(VarDef(s.name, s.rhs.tp)))
    }

    println("Unshredded program eval: ")
    eval(unshredded4, ctx)
    q4.statements.foreach { s =>
      println("  " + s.name + " = " + ctx(VarDef(s.name, s.rhs.tp)))
    }

  }

  def runSequential2(): Unit = {
    val q1 = tpch.Test2Full.program.asInstanceOf[Program]

    println("Program: \n" + quote(q1) + "\n")

    val (shredded, shreddedCtx) = shredCtx(q1)
    println("Shredded program: \n" + quote(shredded) + "\n")

    val optShredded = optimize(shredded)
    println("Shredded program optimized: \n" + quote(optShredded) + "\n")

    val materializedProgram = materialize(optShredded, eliminateDomains = true)
    println("Materialized program: \n" + quote(materializedProgram.program) + "\n")

    val unshredded = unshred(optShredded, materializedProgram.ctx)
    println("Unshredded program: \n" + quote(unshredded) + "\n")

    val lDict = List[Map[String, Any]](
      Map("l_orderkey" -> 1, "l_partkey" -> 42, "l_suppkey" -> 789, "l_quantity" -> 7.0)
    )
    val pDict = List[Map[String, Any]](
      Map("p_partkey" -> 42, "p_name" -> "Kettle", "p_retailprice" -> 12.45)
    )
    val cDict = List[Map[String, Any]](
      Map("c_custkey" -> 10, "c_name" -> "Alice")
    )
    val oDict = List[Map[String, Any]](
      Map("o_orderkey" -> 1, "o_custkey" -> 10, "o_orderdate" -> 20200317)
    )
    val sDict = List[Map[String, Any]](
      Map("s_suppkey" -> 789, "s_name" -> "Supplier#1")
    )

//    val ctx = new RuntimeContext
//    ctx.add(VarDef(inputBagName("L__D"), TPCHSchema.lineittype), lDict)
//    ctx.add(VarDef(inputBagName("P__D"), TPCHSchema.parttype), pDict)
//    ctx.add(VarDef(inputBagName("C__D"), TPCHSchema.customertype), cDict)
//    ctx.add(VarDef(inputBagName("O__D"), TPCHSchema.orderstype), oDict)
//    ctx.add(VarDef(inputBagName("S__D"), TPCHSchema.suppliertype), sDict)
//
//    println("Program eval: ")
//    eval(materializedProgram.program, ctx)
//    materializedProgram.program.statements.foreach { s =>
//      println("  " + s.name + " = " + ctx(VarDef(s.name, s.rhs.tp)))
//    }
//
//    println("Unshredded program eval: ")
//    eval(unshredded, ctx)
//    q1.statements.foreach { s =>
//      println("  " + s.name + " = " + ctx(VarDef(s.name, s.rhs.tp)))
//    }

    val q4 = Program(Assignment(tpch.Test2NN.name, tpch.Test2NN.query.asInstanceOf[Expr]))

    println("Program: \n" + quote(q4) + "\n")

    val (shredded4, _) = shredCtx(q4, shreddedCtx)
    println("Shredded program: \n" + quote(shredded4) + "\n")

    val optShredded4 = optimize(shredded4)
    println("Shredded program optimized: \n" + quote(optShredded4) + "\n")

    val materializedProgram4 = materialize(optShredded4, materializedProgram.ctx, eliminateDomains = true)
    println("Materialized program: \n" + quote(materializedProgram4.program) + "\n")

    val unshredded4 = unshred(optShredded4, materializedProgram4.ctx)
    println("Unshredded program: \n" + quote(unshredded4) + "\n")

//    println("Program eval: ")
//    eval(materializedProgram4.program, ctx)
//    materializedProgram4.program.statements.foreach { s =>
//      println("  " + s.name + " = " + ctx(VarDef(s.name, s.rhs.tp)))
//    }
//
//    println("Unshredded program eval: ")
//    eval(unshredded4, ctx)
//    q4.statements.foreach { s =>
//      println("  " + s.name + " = " + ctx(VarDef(s.name, s.rhs.tp)))
//    }

  }

  def domainTest(): Unit = {
    val q1 = tpch.Query5.program.asInstanceOf[Program]

    println("Program: \n" + quote(q1) + "\n")

    val (shredded, shreddedCtx) = shredCtx(q1)
    println("Shredded program: \n" + quote(shredded) + "\n")

    val optShredded = optimize(shredded)
    println("Shredded program optimized: \n" + quote(optShredded) + "\n")

    val materializedProgram = materialize(optShredded, eliminateDomains = true)
    println("Materialized program: \n" + quote(materializedProgram.program) + "\n")

    val unshredded = unshred(optShredded, materializedProgram.ctx)
    println("Unshredded program: \n" + quote(unshredded) + "\n")

    val q4 = tpch.Query6Full.program.asInstanceOf[Program]
    // Program(Assignment(tpch.Query6Full.name, tpch.Query6Full.program.asInstanceOf[Expr]))

    println("Program: \n" + quote(q4) + "\n")

    val (shredded4, _) = shredCtx(q4, shreddedCtx)
    println("Shredded program: \n" + quote(shredded4) + "\n")

    val optShredded4 = optimize(shredded4)
    println("Shredded program optimized: \n" + quote(optShredded4) + "\n")

    val materializedProgram4 = materialize(optShredded4, materializedProgram.ctx, eliminateDomains = true)
    println("Materialized program: \n" + quote(materializedProgram4.program) + "\n")

    val unshredded4 = unshred(optShredded4, materializedProgram4.ctx)
    println("Unshredded program: \n" + quote(unshredded4) + "\n")
  }

  // These are multi-attribute label cases, but all attributes 
  // satisfy a domain-optimization requirement.
  def dualConditionLabels(): Unit = {
    // first case satisfies If Hoisting and Dict Iteration
    val q1 = genomic.HybridBySample.program.asInstanceOf[Program]

    println("Program: \n" + quote(q1) + "\n")

    val (shredded, _) = shredCtx(q1)
    println("Shredded program: \n" + quote(shredded) + "\n")

    val optShredded = optimize(shredded)
    println("Shredded program optimized: \n" + quote(optShredded) + "\n")

    val materializedProgram = materialize(optShredded, eliminateDomains = true)
    println("Materialized program (if hoists + dict iteration): \n" + quote(materializedProgram.program) + "\n")

    val unshredded = unshred(optShredded, materializedProgram.ctx)
    println("Unshredded program: \n" + quote(unshredded) + "\n")
  }

  def dualConditionLabels2(): Unit = {
    // second case has two Dict Iteration labels (from two different inputs)
    val q2 = genomic.EffectBySample.program.asInstanceOf[Program]

    println("Program: \n" + quote(q2) + "\n")

    val (shredded2, _) = shredCtx(q2)
    println("Shredded program: \n" + quote(shredded2) + "\n")

    val optShredded2 = optimize(shredded2)
    println("Shredded program optimized: \n" + quote(optShredded2) + "\n")

    val materializedProgram2 = materialize(optShredded2, eliminateDomains = true)
    println("Materialized program (dict iterations): \n" + quote(materializedProgram2.program) + "\n")

    val unshredded2 = unshred(optShredded2, materializedProgram2.ctx)
    println("Unshredded program: \n" + quote(unshredded2) + "\n")

  }

  // this version of the query fails an assertion
  def matFailedAssertion(): Unit = {

    // third case throws a materialization error
    val q1 = genomic.EffectBySample2.program.asInstanceOf[Program]
    val (shredded, shreddedCtx) = shredCtx(q1)
    println("Shredded program: \n" + quote(shredded) + "\n")

    val optShredded = optimize(shredded)
    println("Shredded program optimized: \n" + quote(optShredded) + "\n")

    val materializedProgram = materialize(optShredded, eliminateDomains = true)
    println("Materialized program: \n" + quote(materializedProgram.program) + "\n")

    val unshredded = unshred(optShredded, materializedProgram.ctx)
    println("Unshredded program: \n" + quote(unshredded) + "\n")

  }

  // not sure where this error is coming from
  def matTupleDictUnsupported1(): Unit = {
    val q1 = genomic.HybridBySample2.program.asInstanceOf[Program]

    println("Program: \n" + quote(q1) + "\n")

    val (shredded, _) = shredCtx(q1)
    println("Shredded program: \n" + quote(shredded) + "\n")

    val optShredded = optimize(shredded)
    println("Shredded program optimized: \n" + quote(optShredded) + "\n")

    val materializedProgram = materialize(optShredded, eliminateDomains = true)
    println("Materialized program (if hoists + dict iteration): \n" + quote(materializedProgram.program) + "\n")

    val unshredded = unshred(optShredded, materializedProgram.ctx)
    println("Unshredded program: \n" + quote(unshredded) + "\n")
  }

  def matTupleDictUnsupported2(): Unit = {
    val q1 = genomic.HybridBySample2.program.asInstanceOf[Program]

    println("Program: \n" + quote(q1) + "\n")

    val (shredded, _) = shredCtx(q1)
    println("Shredded program: \n" + quote(shredded) + "\n")

    val optShredded = optimize(shredded)
    println("Shredded program optimized: \n" + quote(optShredded) + "\n")

    val materializedProgram = materialize(optShredded, eliminateDomains = true)
    println("Materialized program (if hoists + dict iteration): \n" + quote(materializedProgram.program) + "\n")

    val unshredded = unshred(optShredded, materializedProgram.ctx)
    println("Unshredded program: \n" + quote(unshredded) + "\n")
  }

  def matTupleDictUnsupported3(): Unit = {
    val q1 = genomic.OccurGroupByCase0.program.asInstanceOf[Program]

    println("Program: \n" + quote(q1) + "\n")

    val (shredded, _) = shredCtx(q1)
    println("Shredded program: \n" + quote(shredded) + "\n")

    val optShredded = optimize(shredded)
    println("Shredded program optimized: \n" + quote(optShredded) + "\n")

    val materializedProgram = materialize(optShredded, eliminateDomains = true)
    println("Materialized program (if hoists + dict iteration): \n" + quote(materializedProgram.program) + "\n")

    val unshredded = unshred(optShredded, materializedProgram.ctx)
    println("Unshredded program: \n" + quote(unshredded) + "\n")
  }

  def matTupleDictUnsupported4(): Unit = {
    val q1 = genomic.OccurGroupByCase.program.asInstanceOf[Program]

    println("Program: \n" + quote(q1) + "\n")

    val (shredded, _) = shredCtx(q1)
    println("Shredded program: \n" + quote(shredded) + "\n")

    val optShredded = optimize(shredded)
    println("Shredded program optimized: \n" + quote(optShredded) + "\n")

    val materializedProgram = materialize(optShredded, eliminateDomains = true)
    println("Materialized program (if hoists + dict iteration): \n" + quote(materializedProgram.program) + "\n")

    val unshredded = unshred(optShredded, materializedProgram.ctx)
    println("Unshredded program: \n" + quote(unshredded) + "\n")
  }

  // an example that iterates through but doesn't fail
  def matTupleDictUnsupported5(): Unit = {
    val q1 = genomic.OccurGroupByCase1.program.asInstanceOf[Program]

    println("Program: \n" + quote(q1) + "\n")

    val (shredded, _) = shredCtx(q1)
    println("Shredded program: \n" + quote(shredded) + "\n")

    val optShredded = optimize(shredded)
    println("Shredded program optimized: \n" + quote(optShredded) + "\n")

    val materializedProgram = materialize(optShredded, eliminateDomains = true)
    println("Materialized program (if hoists + dict iteration): \n" + quote(materializedProgram.program) + "\n")

    val unshredded = unshred(optShredded, materializedProgram.ctx)
    println("Unshredded program: \n" + quote(unshredded) + "\n")
  }

//  runSequential()
//   runSequential2()
//   domainTest()

  // dualConditionLabels()
  // dualConditionLabels2()
  // matFailedAssertion()
  
  //matTupleDictUnsupported1()
  //matTupleDictUnsupported2()
  //matTupleDictUnsupported3()
  matTupleDictUnsupported4()
  // similar query that projects less attributes, and passes
  //matTupleDictUnsupported5()


//  run(tpch.Query1.program.asInstanceOf[Program])
//  run(tpch.Query2.program.asInstanceOf[Program])
//  run(tpch.Query3.program.asInstanceOf[Program])
//  run(tpch.Query4.program.asInstanceOf[Program])
//  run(tpch.Query5.program.asInstanceOf[Program])
//  run(tpch.Query6.program.asInstanceOf[Program])
//  run(tpch.Query7.program.asInstanceOf[Program])
}
