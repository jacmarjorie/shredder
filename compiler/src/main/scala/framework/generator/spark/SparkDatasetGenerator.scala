package framework.generator.spark

import framework.common._
// import framework.plans.{Multiply => CMultiply}
import framework.plans._
import framework.utils.Utils.ind

/**
  * Spark/Scala generator for Datasets. 
  * @param cache boolean flag for caching intermediate outputs
  * @param evaluate boolean flag for evaluating intermediate outputs
  * @param skew boolean flag for skew-aware application (requires only minor adjustments)
  * @param isDict boolean flag to represent coming from shredded pipeline
  * @param unshred boolean flag to represent unframework 
  * @param evalFinal boolean flag to run evaluate on the final output (standard pipeline)
  * @param inputs map of input types that should not be reproduced (important for programs)
  */
class SparkDatasetGenerator(cache: Boolean, evaluate: Boolean, skew: Boolean = false, optLevel: Int = 2,
  unshred: Boolean = false, evalFinal: Boolean = true, inputs: Map[Type, String] = Map(), dedup: Boolean = true) extends SparkTypeHandler with SparkUtils {

  implicit def expToString(e: CExpr): String = generate(e)

  var types: Map[Type, String] = inputs
  var encoders: Map[String, Type] = Map()
  override val BAGTYPE: String = "Seq"
  val ext = new Extensions{}

  /** Generates the code for the set of case class records associated to the 
    * records in the generated program.
    *
    * @param names list of names to omit from header creation
    * @return string representing all the records required to run the corresponding application
    */
  def generateHeader(names: List[String] = List()): String = {
	val h1 = typelst.map(x => generateTypeDef(x)).mkString("\n")
    val h2 = inputs.withFilter(x => !names.contains(x._2)).map( x => generateTypeDef(x._1)).toList
    if (h2.nonEmpty) { s"$h1\n${h2.mkString("\n")}" } else { h1 }
  }

  def generateHeaderList(names: List[String] = List()): Seq[String] = {
  val h1 = typelst.map(x => generateTypeDef(x)).toSeq
    val h2 = inputs.withFilter(x => !names.contains(x._2)).map( x => generateTypeDef(x._1)).toSeq
    if (h2.nonEmpty) { h1 ++ h2 } else { h1 }
  }

  /** Generates the code for the set of implicit encoders associated to the 
    * records in the generated program.
    *
    * @return string representing all encoders required by the application
    */
  def generateEncoders(): String = encoders.map{
    case r => 
      s"""|implicit val encoder${r._1}: Encoder[${r._1}] = Encoders.product[${r._1}]
          |${generateUdf(r._2)}
          |""".stripMargin
  }.mkString("\n")

  def generateUdf(tp: Type): String = {
    val args = tp.attrs.map(t => s"${t._1}: ${generateType(t._2)}").mkString("(", ",", ")")
    val inpts = tp.attrs.map(t => t._1).mkString(",")
    s"val udf${generateType(tp)} = udf { $args => ${generateType(tp)}($inpts) }"
  }

  /**
    * Drop a bag attribute and create an index where necessary.
    * 
    * @param tp type of record to drop attribute from
    * @param v variable for substitution within record attribute expressions
    * @param field bag projection attribute
    * @param index boolean flag to introduce index or not
    * @return new record with projected bag type
    */
  private def drop(tp: Type, v: Variable, field: String, index: Boolean = true): CExpr = tp match {
    case RecordCType(fs) => 
      val imap = if (index) Map("index" -> Index) else Map()
      Record(imap ++ (fs - field).map{ case (
        attr, atp) => attr -> Project(v, attr)})
    case _ => sys.error(s"unsupported type ${tp}")
  }

  /** Generates a single variable or a tuple of variables
    * never more than two, ie. either a or (a,b)
    *
    * @param list of variables from left subplan
    * @return string tupled vars
    */
  private def vars(vs: List[Variable]): String = {
    if (vs.size == 1) generate(vs.head)
    else vs.map(generate(_)).mkString("(", ",", ")")
  }

  private def matchOption(v: Variable, attr: String): String = {
    v.tp.attrs(attr) match {
      case OptionType(otp) => 
        s"${v.name} => ${v.name}.$attr match { case Some($attr) => $attr; case _ => ${zero(otp)} }"
      case _ => s"${v.name} => ${v.name}.$attr"
    }
  }

  private def matchOption(v: Variable, attr: String, rattr: Double): String = {
    v.tp.attrs(attr) match {
      case OptionType(otp) => 
        s"${v.name} => ${v.name}.$attr match { case Some($attr) => $rattr; case _ => 0.0 }"
      case _ => s"${v.name} => ${v.name}.$attr"
    }
  }

  /** This is called from functions, not Dataset operations.
    * When a value is an option, then it will call get on it 
    */
  private def accessOption(e: CExpr, nv: Variable): String = e match {
    case Project(v, field) => 
      nv.tp.attrs.getOrElse(field, v.tp.attrs(field)) match {
        case OptionType(otp) => 
          val zt = otp match { case StringType => "\""+null+"\""; case _ => zero(otp)}
          s"""${nv.name}.$field match { case Some(x) => x; case _ => $zt }"""
          //s"${nv.name}.$field.get"
        case _ => s"${nv.name}.$field"
      }
    case Record(fs) => 
      val unouter = e.tp.unouter
      handleType(unouter)
      val rcnts = fs.map(f => accessOption(f._2, nv)).mkString(", ")
      s"${generateType(unouter)}($rcnts)"
    case If(cond, e1, Some(e2)) => 
      s"if (${accessOption(cond, nv)}) ${accessOption(e1, nv)} else ${accessOption(e2, nv)} "
    case Equals(e1, e2) => s"${accessOption(e1, nv)} == ${accessOption(e2, nv)}"
    case Lt(e1, e2) => s"${accessOption(e1, nv)} < ${accessOption(e2, nv)}"
    case Gt(e1, e2) => s"${accessOption(e1, nv)} > ${accessOption(e2, nv)}"
    case Lte(e1, e2) => s"${accessOption(e1, nv)} <= ${accessOption(e2, nv)}"
    case Gte(e1, e2) => s"${accessOption(e1, nv)} >= ${accessOption(e2, nv)}"
    case And(e1, e2) => s"${accessOption(e1, nv)} && ${accessOption(e2, nv)}"
    case Or(e1, e2) => s"${accessOption(e1, nv)} || ${accessOption(e2, nv)}"
    case Not(e1) => s"!(${accessOption(e1, nv)})"
    case MathOp(op, e1, e2) => 
      s"(${accessOption(e1, nv)}) $op (${accessOption(e2, nv)})"     
    case _ => generate(e)
  }

  /** This is specific to Dataset, when there is a projection 
    * then it is treated as a column reference
    */
  private def generateReference(e: CExpr, literal: Boolean = false): String = e match {
    case Project(v, f) => v.tp match {
      case LabelType(fs) if fs.size == 1 => s"""col("_LABEL")"""
      case LabelType(_) => s"""col("_LABEL").getField("$f")"""
      case _ => s"""col("$f")"""
    }
    case Label(fs) if fs.size == 1 => 
      generateReference(fs.head._2)
    case Label(fs) => 
      encoders = encoders + (generateType(e.tp) -> e.tp)
      handleType(e.tp)
      val rcnts = e.tp.attrs.map(f => generateReference(fs(f._1))).mkString(", ")
      s"udf${generateType(e.tp)}($rcnts)"
    case Constant(c) if literal => s"lit(${generate(e)})"
    case If(cond, e1, Some(e2)) => 
      s"when(${generate(cond)}, ${generateReference(e1)}).otherwise(${generateReference(e2)})"
    case If(cond, e1, None) => 
      s"when(${generate(cond)}, ${generateReference(e1)})"
    case _ => generate(e)
  }

  private def defaultCastNull(expr: CExpr, col: String, literal: Boolean = false): List[String] = {
    val baseCol = List(s"""|  .withColumn("$col", ${generateReference(expr, literal)})""")
    if (expr.tp.isNumeric) 
      baseCol :+ s""".withColumn("$col", when(col("$col").isNull, ${zero(expr.tp)}).otherwise(col("$col")))"""
    else baseCol
  }

  private def defaultCastNull(col: String, oldCol: String, tp: Type): List[String] = {
    val baseCol = List(s"""| .withColumnRenamed("$oldCol", "$col")""")
    if (tp.isNumeric) 
      baseCol :+ s""".withColumn("$col", when(col("$col").isNull, ${zero(tp)}).otherwise(col("$col")))"""
    else baseCol
  }

  private def buildLocalAgg(path: String, nrec: Record, v2: String, vset: Set[String], filter: CExpr = Constant(true)): String = {

    def adjustReduceType(t: Type): Type = t match {
      case RecordCType(ms) => RecordCType(ms.map(m => 
        if (vset(m._1)) m._2 match { 
          case OptionType(_) => m._1 -> OptionType(DoubleType); case _ => m._1 -> DoubleType }
        else m))
      case _ => t
    }

    val keyRec = Record(nrec.fields.filter(f => !vset(f._1)))
    val krec = generate(keyRec)

    val ktp = generateType(keyRec.tp)

    // only case for one item in values
    val (vtp, vdefault, value) = ("Double", "0.0", vset.head)
    
    val nv = Variable(v2, nrec.tp)
    val nv2 = Variable(v2, nrec.tp.unouter)
    val adjustType = adjustReduceType(nrec.tp)
    handleType(adjustType)
    val resRec = getRecord(nv, vset, generateType(adjustType))

    filter match {
      case Constant(true) => 
        s"""|       $path.foldLeft(HashMap.empty[$ktp, $vtp].withDefaultValue($vdefault))(
            |         (acc, $v2) => {acc($krec) += $v2.$value.asInstanceOf[$vtp]; acc} 
            |       ).map($v2 => $resRec)
            |""".stripMargin
      case _ => 
        s"""|       $path.foldLeft(HashMap.empty[$ktp, $vtp].withDefaultValue($vdefault))(
            |         (acc, $v2) => {
            |           if (${accessOption(filter, nv2)}) { acc($krec) += $v2.$value.asInstanceOf[$vtp]; acc }
            |           else acc
            |       }).map($v2 => $resRec)
            |""".stripMargin
    }

  }

  private def generateUnnest(e: UnnestOp, pushAgg: Option[CExpr] = None): String = {

    // generate references 
    val gin = generate(e.in)
    val gv = generate(e.v)
    val gv2 = generate(e.v2)

    // generate output record components
    val topAttrs = e.topAttrs
    val nrec = Record(topAttrs ++ e.nextAttrs)
    val gnrec = generate(nrec)
    val nrecName = generateType(nrec.tp)

    val localMap = (pushAgg, e.filter) match {
      case (Some(CReduceBy(_,_,_,values)), _) =>
        buildLocalAgg(e.path, nrec, gv2, values.toSet, e.filter)
      case (_, Constant(true)) => s"${e.path}.map( $gv2 => $gnrec )"
      case _ => s"${e.path}.flatMap( $gv2 => if (${accessOption(e.filter, e.v2)}) Seq($gnrec) else Seq() )"
    }

    if (e.outer){

      // generate output record components for empty inner bags
      val nrec0 = topAttrs ++ e.nextAttrs.map(f => f._1 -> COption(Null))
      val gnrec0 = s"$nrecName(${nrec0.map(f => generate(f._2)).mkString(", ")})"
      
      // catch case where nested path attribute is of option type
      val innerMatch = e.v.tp.attrs(e.path) match {
        case OptionType(_) => 
          s"""|   $gv.${e.path} match {
              |     case Some(${e.path}) if ${e.path}.nonEmpty => 
              |       $localMap
              |     case _ => Seq($gnrec0)
              |   }
              |""".stripMargin
        case _ => 
          s"""|   if ($gv.${e.path}.isEmpty) Seq($gnrec0)
              |   else $gv.$localMap
              |""".stripMargin
      }

      // return flat map for outer unnest
      s"""|$gin.flatMap{
          | case $gv => 
          |   $innerMatch
          |}.as[$nrecName]
          |""".stripMargin    

    }else 

      // return flat map for non-outer unnest
      s"""|$gin.flatMap{ case $gv => 
          |   $gv.$localMap
          |}.as[$nrecName]
          |""".stripMargin

  }

  def generate(e: CExpr): String = e match {
    /** ZEROS **/
    case Null => "null"
    case CUnit => "()"
    case EmptySng => "Seq()"
    case EmptyCDict => s"()"
    case Index => "index"
    case COption(e1) => e1 match {
      case Null => "None"
      case _ => s"Some(${generate(e1)})"
    }
    
    /** BASIC CONSTRUCTS **/
    case Variable(name, _) => name
    case InputRef(name, tp) => name
    case Constant(s:String) => "\"" + s + "\""
    case Constant(x) => x.toString
    case CUdf(n, e1, tp) => s"$n(${generateReference(e1)})"
	  case Sng(e) => s"Seq(${generate(e)})"
    case CGet(e1) => s"${generate(e1)}.head"
    case CDeDup(e1) if dedup => s"${generate(e1)}.distinct"
    case CDeDup(e1) => s"${generate(e1)}/** distinct removed for stats gathering **/"
    case Label(fs) if fs.size == 1 => generate(fs.head._2)
    case Record(fs) if fs.contains("element") && fs.size == 1 => fs("element")
    case Record(fs) => {
      handleType(e.tp)
      val rcnts = e.tp.attrs.map(f => generate(fs(f._1))).mkString(", ")
      s"${generateType(e.tp)}($rcnts)"
    }
    case Tuple(fs) => s"(${fs.map(f => generate(f)).mkString(",")})"

    // case Project(e1, "_LABEL") => s"${generate(e1)}._LABEL"
    case Project(e1, "element") => s"${generate(e1)}"
    case Project(e2 @ Record(fs), field) => 
      s"${generate(e2)}.${kvName(field)(fs.size)}"
    case Project(e2, field) => s"${generate(e2)}.$field"
    /** MATH OPS **/

    case MathOp(op, e1, e2) => op match {
      case OpDivide if !e2.isInstanceOf[Constant] => 
        val ge2 = generateReference(e2)
        val ze2 = zero(e2.tp)
        s"when($ge2 === $ze2, $ze2).otherwise(${generateReference(e1)} $op $ge2)"
      case _ => s"(${generateReference(e1)} $op ${generateReference(e2)})"
    }

    /** BOOL OPS **/
    case Equals(e1, e2) =>
      val eq = (e1, e2) match {
        // case (Project(p1, _), _) if p1.tp.isInstanceOf[LabelType] => "=="
        // case (_, Project(p2, _)) if p2.tp.isInstanceOf[LabelType] => "=="
        case _ => "==="
      }
      s"${generateReference(e1)} $eq ${generateReference(e2)}"
    case Lt(e1, e2) => s"${generateReference(e1)} < ${generateReference(e2)}"
    case Gt(e1, e2) => s"${generateReference(e1)} > ${generateReference(e2)}"
    case Lte(e1, e2) => s"${generateReference(e1)} <= ${generateReference(e2)}"
    case Gte(e1, e2) => s"${generateReference(e1)} >= ${generateReference(e2)}"
    case And(e1, e2) => s"${generateReference(e1)} && ${generateReference(e2)}"
    case Or(e1, e2) => s"${generateReference(e1)} || ${generateReference(e2)}"
    case Not(Equals(e1, e2)) => s"${generateReference(e1)} =!= ${generateReference(e2)}"
    case Not(e1) => s"!(${generateReference(e1)})"

    case If(cond, s1, Some(s2)) => 
      s"when(${generate(cond)}, ${generate(s1)}).otherwise(${generate(s2)})"
    case If(cond, s1, None) => 
      s"when(${generate(cond)}, ${generate(s1)})"

    case FlatDict(e1) => s"${generate(e1)}"
    case GroupDict(e1) => generate(e1) 
    
    case Projection(in, v, Constant(true), Nil) => generate(in)

    case ep @ Projection(in, v, pat:Record, fields) =>
      handleType(pat.tp)

      val nfields = ext.collect(pat)
      val select = if (in.tp.attrs.keySet == nfields) ""
        else s".select(${nfields.toList.mkString("\"", "\", \"", "\"")})"

      val projectNulls = fields.contains("remove_nulls")
      // input table attributes
      val projectCols = fields.toSet - "remove_nulls"
      // output attributes
      val newCols = pat.fields.keySet

      // make new columns
      val newColumns = pat.fields.flatMap{
        // avoid column renaming
        case (col, Project(_, oldCol)) if col != oldCol => Nil
        case (col, expr) if !projectCols(col) => defaultCastNull(expr, col, true)
        case (ncol, col @ Label(fs)) => 
          fs.head match {
            case (_, Project(_, "_1")) if fs.size == 1 => Nil
            // check this case
            case (_, Project(_, pcol)) if (ncol == pcol && fs.size == 1) => Nil
            case (_, expr) => defaultCastNull(col, ncol, false)
          }
 		    case _ => Nil
      }

      // override existing columns
      val renamedColumns = pat.fields.flatMap{
        // create a new column from an old column
        case (col, p @ Project(_, oldCol)) if col != oldCol => 
          // creates an additional column
          if (newCols(oldCol)) List(s"""| .withColumn("$col", $$"$oldCol")""")
          // overrides a column
          else defaultCastNull(col, oldCol, p.tp)
		      case _ => Nil
      } 

      // ensure that new columns are made before renaming occurs
      val allColumns = (newColumns ++ renamedColumns)
      val columns = allColumns.mkString("\n").stripMargin

      val ncast = if (in.tp.attrs.keySet == nfields && allColumns.isEmpty) ""
        else s".as[${generateType(pat.tp)}]"

      // project nulls needs more testing
      s"""|${generate(in)}${if (projectNulls) ".na.drop()" else ""}$select
          $columns
          | $ncast
          |""".stripMargin

    // lookup iteration translates to inner join 
    case ej @ OuterJoin(left, v1, right, v2, Equals(Project(_, p1), Project(_, p2 @ "_1")), filt) if right.tp.isDict =>
      // adjust lookup column of dictionary
      val rcol = s"${p1}${p2}"
      val rtp = rename(right.tp.attrs, p2, rcol)
	    handleType(rtp)
      val grtp = generateType(rtp)

      // adjust label lookup column
      val lcol = s"${p1}_LABEL"
      val (lcolumn, ltp) = v1.tp.attrs get "_LABEL" match {
        case Some(LabelType(ms)) if ms.size > 1 =>
          (s""".withColumn("$lcol", col("_LABEL").getField("$p1"))""", 
            RecordCType(left.tp.attrs))
        case _ => 
          (s""".withColumnRenamed("${p1}", "$lcol")""",
            rename(left.tp.attrs, p1, lcol))
      }
      handleType(ltp)
      val gltp = generateType(ltp)

      val nrecTp = RecordCType(ltp.merge(rtp).attrs -- Set(rcol,lcol))
      handleType(nrecTp)
      val classTags = if (!skew) ""
        else s"[$grtp, ${generateType(right.tp.attrs(p2))}]"

      s"""|${generate(left)}$lcolumn
          |   .as[$gltp].equiJoin$classTags(
          |   ${generate(right)}.withColumnRenamed("${p2}", "$rcol").as[$grtp], 
          |   Seq("$lcol"), Seq("$rcol"), "inner").drop("$lcol", "$rcol")
          |   .as[${generateType(nrecTp)}]
          |""".stripMargin

    // JOIN operator
    case ej:JoinOp =>
      handleType(ej.tp.tp)
      val nrec = generateType(ej.tp.tp)
      val gright = generate(ej.right)
      val rtp = ej.right.tp.attrs

      if (ej.isEquiJoin){

          val (p1, p2) = (ej.p1s.mkString("\",\""), ej.p2s.mkString("\",\""))

          val classTags = if (!skew) ""
            else {
              handleType(RecordCType(rtp))
              s"[${generateType(RecordCType(rtp))}, ${generateType(rtp(p2))}]"
            }

          s"""|${generate(ej.left)}.equiJoin$classTags($gright, 
              | Seq("${p1}"), Seq("${p2}"), "${ej.jtype}").as[$nrec]
              |""".stripMargin

        }else {
          ej.cond match {
            case Constant(true) => 
              s"""|${generate(ej.left)}.crossJoin($gright)
                  |  .as[$nrec]
                  |""".stripMargin 
            case _ => 
              s"""|${generate(ej.left)}.join($gright, ${generateReference(ej.cond)}, "${ej.jtype}")
                  |  .as[$nrec]
                  |""".stripMargin   
          }      
        }

    // push local aggregation inside of unnest
    case Bind(vu, un:UnnestOp, Bind(vc, cr @ CReduceBy(_, v1, keys, values), e2)) =>
      s"val ${generate(vc)} = ${generateUnnest(un, Some(cr))}\n${generate(e2)}"

    case eu:UnnestOp => generateUnnest(eu)

    // Nest - Join => CoGroup
    case Bind(vj, join:JoinOp, Bind(nv, nd @ Nest(in, v, key, value @ Record(fs), filter, nulls, tag), e2)) 
      if (unshred) && (ext.collect(value) subsetOf join.v2.tp.attrs.keySet) && join.isEquiJoin =>
      
      val (p1, p2) = (join.p1, join.p2)
      val gv = generate(join.v)
      val gv2 = generate(join.v2)
      val gv3 = generate(v)

      val gright = join.v2.tp.attrs(p2) match {
        case LabelType(fs) if fs.size > 1 => 
          val rkeys = fs.map(f => s"$gv2.${p2}.${f._1}").mkString("(",",",")")
          s"${generate(join.right)}.unionGroupByKey($gv2 => $rkeys)"
        case _ => s"${generate(join.right)}.unionGroupByKey($gv2 => $gv2.${p2})"
      }

      val lkeys = join.v.tp.attrs(p1) match {
        case LabelType(fs) if fs.size > 1 => fs.map(f => s"$gv.${p1}.${f._1}").mkString("(",",",")")
        case _ => s"$gv.${p1}"
      }

      handleType(nd.tp.tp)
      val nrec = generateType(nd.tp.tp)
      val frec = getRecord(nrec, gv3, nd.tp.attrs, tag, "grp")

      val gvalue = accessOption(value, join.v2)
      val leftMap = join.jtype match {
        case "left_outer" => s"ve1.map($gv3 => $frec)"
        case "inner" => "ve1.flatMap($gv3 => if (grp.nonEmpty) $frec else Seq())"
        case _ => sys.error("unsupported join type")
      }

      s"""|val ${generate(nv)} = ${generate(join.left)}.cogroup($gright, $gv => $lkeys)(
          |   (_, ve1, ve2) => {
          |     val grp = ve2.map($gv2 => $gvalue).toSeq
          |     $leftMap
          |   }).as[$nrec]
          |${generate(e2)}
          |""".stripMargin

    // local aggregation pushed to input relations
    case CReduceBy(vin, vout, keys, values) => 

      val nrec = Record(e.tp.attrs.map(f => f._1 -> Project(vout, f._1)))
      val gnrec = generate(nrec)
      val v2 = vout.name
      s"""|${generate(vin)}.mapPartitions(it => 
          |   ${buildLocalAgg("it", nrec, vout.name, values.toSet)}.iterator
          | )
          |""".stripMargin

    // primitive monoid
    case en @ Nest(in, v, key, Constant(c), Record(fs), nulls, tag) =>
      handleType(en.tp.tp)
      val intp = generateType(in.tp.asInstanceOf[BagCType].tp)
      val gtp = generateType(en.tp.tp)

      val gv = generate(v)
      val rkey = Record(key.map(k => k -> Project(v, k)).toMap)
      val rvalues = fs.keySet.map(vs => 
        s"typed.sum[$intp](${matchOption(v, vs, c.asInstanceOf[Double])})\n").toList.mkString("agg(", ",", ")")

      val v2 = generate(Variable.fresh(StringType))
      val nrec = en.tp.tp.attrs.map(k => 
        if (k._1 == tag) s"$v2._2" else s"$v2._1.${k._1}").mkString(s"$gtp(", ", ", ")")

      s"""|${generate(in)}.groupByKey($gv => ${generate(rkey)})
          | .$rvalues.mapPartitions{ it => it.map{ $v2 =>
          |   $nrec
          |}}.as[$gtp]
          |""".stripMargin

    case en @ Nest(in, v, key, value, filter, nulls, tag) =>

      val gv = generate(v)

      // make key expression
      val rkey = Record(key.map(k => k -> Project(v, k)).toMap)
      val kv = Variable("key", rkey.tp)

      // reference value in group
      val grpv = Variable("grp", BagCType(value.tp.unouter))
      val frec = Record(en.tp.tp.attrs.map(k => if (k._1 == tag) k._1 -> grpv 
        else k._1 -> Project(kv, k._1)).toMap)

      // handle nulls
      val nullSet = nulls.toSet
      val rnulls = v.tp.attrs.filter(n => nullSet(n._1) 
        && n._2.isInstanceOf[OptionType]).take(22).keySet
      val uscores = List.fill(rnulls.size)("_")
      val nmatch = rnulls.map(n => s"$gv.$n").mkString("(",",",")")
      val caseMatches = Range(0, rnulls.size).map(i => 
        uscores.patch(i, Seq(s"None"), 1).mkString("case (",",",") => Seq()")).mkString("\n")
     
      s"""|${generate(in)}.groupByKey($gv => ${generate(rkey)}).mapGroups{
          | case (key, value) => 
          |   val grp = value.flatMap($gv => 
          |    $nmatch match {
          |      $caseMatches
          |      case _ => Seq(${accessOption(value, v)})
          |   }).toSeq
          |   ${generate(frec)}
          | }.as[${generateType(frec.tp)}]
          |""".stripMargin

    case er @ Reduce(in, v, key, value) =>
      handleType(er.tp.tp)
      val intp = generateType(in.tp.asInstanceOf[BagCType].tp)
      val gtp = generateType(er.tp.tp)

      val gv = generate(v)
      val rkey = Record(key.map(k => k -> Project(v, k)).toMap)
      val rvalues = value.map(vs => 
        s"typed.sum[$intp](${matchOption(v, vs)})\n").mkString("agg(", ",", ")")

      val nrec = er.tp.tp.attrs.map(k => 
        if (value.contains(k._1)) k._2 match {
          case _:OptionType => s"Some(${k._1})"
          case _ => k._1 
        }else s"key.${k._1}").mkString(s"$gtp(", ", ", ")")

      s"""|${generate(in)}.groupByKey($gv => ${generate(rkey)})
          | .$rvalues.mapPartitions{ it => it.map{ case (key, ${value.mkString(", ")}) =>
          |   $nrec
          |}}.as[$gtp]
          |""".stripMargin

    case ei @ AddIndex(e1, name) => 
      handleType(ei.tp.tp)
      val nrec = generateType(ei.tp.tp)
      s"""|${generate(e1)}.withColumn("$name", monotonically_increasing_id())
          | .as[$nrec]
          |""".stripMargin

    // filter
    case Select(x, v, filt) => 
      handleType(v.tp)
      val filter = filt match {
        case Constant(true) => ""
        case _ => s".filter(${generateReference(filt)})"
      }
      s"""|${generate(x)}$filter
          |""".stripMargin

    case Bind(v, CNamed(n, e1), LinearCSet(fs)) =>
      val gtp = if (skew) "[Int]" else ""
      val repart = if (n.contains("MDict")){
        if (e1.tp.attrs.contains("_LABEL")) s""".repartition$gtp($$"_LABEL")"""
        else s""".repartition$gtp($$"_1")""" 
      }else ""
      val gv = generate(v)
      s"""|val $gv = ${generate(e1)}
          |val $n = $gv$repart
          |//$n.count
          |""".stripMargin
          // |${if (!cache) comment(n) else n}.cache
          // |${if (!cache && !evalFinal) comment(n) else n}.count

    case Bind(v, CNamed(n, e1), e2) =>
      val gtp = if (skew) "[Int]" else ""
      val repart = if (n.contains("MDict")){
        if (e1.tp.attrs.contains("_LABEL")) s""".repartition$gtp($$"_LABEL")"""
        else s""".repartition$gtp($$"_1")""" 
      }else ""

      val gv = generate(v)
      val ge2 = if (e2.isInstanceOf[Variable]) "" else generate(e2)
       s"""|val $gv = ${generate(e1)}
           |val $n = $gv$repart  
           |${if (cache) n else comment(n)}.cache
           |${if (cache) n else comment(n)}.count
           |$ge2
           |""".stripMargin   
      // s"""|val $gv = ${generate(e1)}
      //     |val $n = $gv$repart
      //     |//$n.print
      //     |${if (!cache || evalFinal) comment(n) else n}.cache
      //     |${if (!evaluate) comment(n) else n}.count
      //     |$ge2
      //     |""".stripMargin

    case LinearCSet(fs) => ""
    case Bind(v, e1, e2) => 
      s"val ${generate(v)} = ${generate(e1)} \n${generate(e2)}"
    case _ => s"/** TODO: $e **/"
  }

}
