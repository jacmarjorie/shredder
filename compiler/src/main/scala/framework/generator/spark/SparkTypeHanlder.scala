package framework.generator.spark

import framework.common._
import framework.plans._
import framework.utils.Utils.ind

trait SparkTypeHandler {

  var types: Map[Type, String]
  var typelst: Seq[Type] = Seq()
  val dotEquality: Boolean = false
  val BAGTYPE: String = "Vector"
  val LABELTYPE: String = "String"



  /** Translate a k,v record into a tuple type
    * @deprecated this should no longer be required for datasets
    * @param x string attribute name
    */
  def kvName(x: String)(implicit s: Int = -1): String = x match {
    case "_KEY" if s == 2 => "_1"
    case "_VALUE" if s == 2 => "_2"
    case _ => x
  }

  /** This ensures that records with the same attributes and corresponding 
    * types, but in different order produce difference case classes.
    * @todo switch to ordered names, or normalize these earlier in the pipeline
    * @param map corresponding to the input contents of the record type.
    * @return the type of the map to generate
    */
  def checkType(mapType: Map[String, Type]): Type = {
    val check = types.filter{
      case (RecordCType(ms), name) => ms.keySet == mapType.keySet
      case _ => false
    }
    if (check.size == 1) check.head._1 else RecordCType(mapType)
  }

  /** Generator for named tuples, used when creating the 
    * header.
    * 
    * @param tp type of record
    * @return string representing code to create a class class
    */
  def generateTypeDef(tp: Type): String = tp match {
    case LabelType(fs) => generateTypeDef(RecordCType(fs))
    case RecordCType(fs) =>
      val name = types(tp)
      val fsize = fs.size
      s"case class $name(${fs.map(x => s"${kvName(x._1)(fsize)}: ${generateType(x._2)}").mkString(", ")})"
    case _ => sys.error("unsupported type "+tp)
  }

  /** Generator for all types, used within the main generator.
    *
    * @param tp type of expression from within generator
    * @return string representing the native Scala type
    */
  def generateType(tp: Type): String = tp match {
    case RecordCType(fs) if fs.isEmpty => "Unit"
    case LabelType(fs) if fs.isEmpty => LABELTYPE
    case RecordCType(fs) if fs.contains("element") && fs.size == 1 => generateType(fs("element"))
    //case RecordCType(fs) if fs.keySet == Set("_1", "_2") => fs.generateType(fs._2).mkString("(",",",")")
    // case RecordCType(ms) if ms.values.toList.contains(EmptyCType) => ""
    case RecordCType(_) if types.contains(tp) => types(tp)
    case LabelType(fs) if fs.size == 1 => generateType(fs.head._2)
    case LabelType(fs) => generateType(RecordCType(fs))
    case IntType => "Int"
    case StringType => "String"
    case BoolType => "Boolean"
    case DoubleType => "Double"
    case LongType => "Long"
    case TTupleType(fs) => s"(${fs.map(generateType(_)).mkString(",")})"
    case BagCType(tp) => 
		s"$BAGTYPE[${generateType(tp)}]" //combineByKey, etc. may need this to be iterable
    case MatDictCType(lbl, dict) => generateType(BagCType(RecordCType(tp.attrs)))//s"${generateType(dict)}"
    case BagDictCType(flat @ BagCType(TTupleType(fs)), dict) =>
      dict match {
        case TupleDictCType(ds) if !ds.filter(_._2 != EmptyDictCType).isEmpty =>
          s"($BAGTYPE[(${generateType(fs.head)}, ${generateType(fs.last)})], ${generateType(dict)})"
        case _ => s"($BAGTYPE[(${generateType(fs.head)}, ${generateType(fs.last)})], Unit)"
      }
    case TupleDictCType(fs) if !fs.filter(_._2 != EmptyDictCType).isEmpty =>
      generateType(RecordCType(fs.filter(_._2 != EmptyDictCType)))
    case OptionType(e1) => s"Option[${generateType(e1)}]"
    case EmptyCType => "()"
    case _ => sys.error("not supported type " + tp)
  }

   /** Updates the type map with records and record attribute types 
    * from the generated plan.
    *
    * @param tp type from the input program (called for records)
    * @param givenName optional name used for naming the generated record
    */ 
  def handleType(tp: Type, givenName: Option[String] = None): Unit = {
    if(!types.contains(tp)) {
      tp match {
        case RecordCType(fs) =>
          fs.foreach(f => handleType(f._2))
          val name = givenName.getOrElse("Record"+java.util.UUID.randomUUID().toString.replace("-", ""))//"Record" + Variable.newId)
          types = types + (tp -> name)
          typelst = typelst :+ tp
        case BagCType(tp) => handleType(tp, givenName)
        case LabelType(fs) if fs.size == 1 => handleType(fs.head._2)
        case LabelType(fs) if !fs.isEmpty => handleType(RecordCType(fs))
        case MatDictCType(lbl, dict) => handleType(BagCType(RecordCType(tp.attrs)))//handleType(dict)
        case BagDictCType(flat @ BagCType(TTupleType(fs)), dict) =>
          // val nid = Variable.newId
          handleType(fs.last, Some(givenName.getOrElse("")))
        case TupleDictCType(fs) =>
          val ffs = fs.filter(_._2 != EmptyDictCType)
          if (!ffs.isEmpty) { handleType(RecordCType(ffs), givenName) } else { () }
        case _ => ()
      }
    }
  }
  
}
