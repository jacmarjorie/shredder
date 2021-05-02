package sparkutils.skew

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.collection.mutable.HashMap
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.expressions.scalalang._
import sparkutils.Config
import sparkutils.Util

/** Implicits used for the generated Spark code from the compiler framework.
  * This file contains the following implicits:
  * i) skew-unaware high-level operations defined on a single Dataset/Dataframe
  * ii) skew-aware high-level operations defined on a skew-triple with non-null heavy key set
  * these functions work with a skew-triple (light, heavy, heavyKeys)
  * iii) skew-aware high-level operations defined on a skew-triple with null heavy key set
  * these functions work on a tuple (Dataset, Dataset), for example
  *
  */
object SkewDataset{

  /** Implicits for skew-unaware high-level operations defined on a single Dataset **/
  implicit class DatasetOps[T: Encoder: ClassTag](left: Dataset[T]) extends Serializable {

    // TODO replace with JSON
    def print: Unit = {
      println(left.take(100).toList.map(f => 
        Util.getCCParams(f.asInstanceOf[AnyRef])).mkString("{\n", ",\n", "}\n"))
    }
    

    /** Create an empty Dataset with the same type 
      * Repartition to 1, this will be a check for empty dataframes that avoids 
      * the checkpoint behavior of isEmpty
      */
    def empty: Dataset[T] = left.sparkSession.emptyDataset[T].repartition(1)

    /** Create an empty Dataset with the an alternative type 
      * Repartition to 1, this will be a check for empty dataframes that avoids 
      * the checkpoint behavior of isEmpty
      */
    def empty[U: Encoder : TypeTag]: Dataset[U] = left.sparkSession.emptyDataset[U].repartition(1)

    /** Create an empty Dataframe
      * Repartition to 1, this will be a check for empty dataframes that avoids 
      * the checkpoint behavior of isEmpty
      */
    def emptyDF: DataFrame = left.sparkSession.emptyDataFrame.repartition(1)

    /** Create the light component of this Dataset 
      * filters based on heavy keys; null values are considered light
      * @param col Column to filter on
      * @param hkeys Broadcast set of heavy keys used to filter
      * @return Light component of this Dataset
      */
    def lfilter[K](col: Column, hkeys: Broadcast[Set[K]]): Dataset[T] = {
      left.filter(!col.isInCollection(hkeys.value) || col.isNull)
    }

    /** Create the heavy component of this Dataset 
      * filters based on heavy keys
      * @param col Column to filter on
      * @param hkeys Broadcast set of heavy keys used to filter
      * @return Heavy component of this Dataset
      */
    def hfilter[K](col: Column, hkeys: Broadcast[Set[K]]): Dataset[T] = {
      left.filter((col.isInCollection(hkeys.value)))
    }

    /** Equi-join this Dataset with the right, this is experimental for the skew implicits 
      * mainly provides a join that is a tupled version of the left and right result 
      * rather than a merged case class. 
      * @param right Dataset to join with 
      * @param usingColumns a sequence of columns ordered left to right defining join columns, note this 
      * is different than the Spark native implementation of joinWith
      * @param joinType string join type, default "inner"
      * @return the result of the join
      */
    def equiJoinWith[S: Encoder : ClassTag](right: Dataset[S], leftCols: Seq[String], rightCols: Seq[String], joinType: String): Dataset[(T,S)] = {
      var cond: Column = col(leftCols(0)) === col(rightCols(0))
      if (leftCols.size > 1) cond = (cond) && (col(leftCols(1)) === col(rightCols(1)))

      left.joinWith(right, cond, joinType)
    }

    /** Equi-join this Dataset with the right, this returns a Dataframe
      * @param right Dataset to join with 
      * @param usingColumns a sequence of columns ordered left to right defining join columns, note this 
      * is different than the Spark native implementation of joinWith
      * @param joinType string join type, default "inner"
      * @return the result of the join
      */
    def equiJoin[S: Encoder : ClassTag](right: Dataset[S], leftCols: Seq[String], rightCols: Seq[String], joinType: String): DataFrame = {

      var cond: Column = col(leftCols(0)) === col(rightCols(0))
      if (leftCols.size > 1) cond = (cond) && (col(leftCols(1)) === col(rightCols(1)))

      left.join(right, cond, joinType)

    }
    
    /** Cogroup this dataset with a Dataset that has already been formatted by groupByKey
      * @param right Dataset to cogroup with
      * @param lkey key function for the left
      * @param f map function representing what is returned, usually a left outer cogroup 
      * @return the result of the cogroup based on f
      */
    def cogroup[S: Encoder : ClassTag, R : Encoder: ClassTag, K : Encoder: ClassTag](right: KeyValueGroupedDataset[K,S], lkey: (T) => K)
      (f: (K, Iterator[T], Iterator[S]) => TraversableOnce[R]): Dataset[R] = {
      left.groupByKey(lkey).cogroup(right)(f)
    }
 
    /** Cogroup this dataset with another Dataset, not often used with the code generator due to the type parameters required
      * @param right Dataset to cogroup with
      * @param lkey key function for the left
      * @param rkey key function for the right
      * @param f map function representing what is returned, usually a left outer cogroup 
      * @return the result of the cogroup based on f
      */    
    def cogroup[S: Encoder : ClassTag, R : Encoder: ClassTag, K](right: Dataset[S], lkey: (T) => K, rkey: (S) => K, key: Option[String] = None)
      (f: (K, Iterator[T], Iterator[S]) => TraversableOnce[R])(implicit arg0: Encoder[K]): Dataset[R] =
        left.groupByKey(lkey).cogroup(right.groupByKey(rkey))(f)

    /** A wrapper to groupByKey that is leverage by the skew-aware operations
      * @param f key function
      * @return key value grouped dataset based on f
      */
    def unionGroupByKey[K : Encoder](f: (T) => K): KeyValueGroupedDataset[K, T] = left.groupByKey(f)

    /** sumBy+, reduceByKey for a Double attribute defined identified 
      * @key keying function
      * @value function to extract value from row
      * @return a key value tuple with the second the summed attributes for each key in the first
      */
    def reduceByKey[K: Encoder](key: (T) => K, value: (T) => Double): Dataset[(K, Double)] = {
      left.groupByKey(key).agg(typed.sum[T](value))
    }

  }

  /** Implicits for skew-unaware high-level operations defined on a single Dataframe **/
  implicit class DataframeOps(left: DataFrame) extends Serializable {

    /** Print support for a Dataframe **/
    def print: Unit = {
      println(left.take(10).toList.map(f => 
        Util.getCCParams(f.asInstanceOf[AnyRef])).mkString("{\n", ",\n", "}\n"))
    }

    /** Create an empty Dataset with the an alternative type 
      * Repartition to 1, this will be a check for empty dataframes that avoids 
      * the checkpoint behavior of isEmpty
      */
    def empty[U: Encoder : TypeTag]: Dataset[U] = left.sparkSession.emptyDataset[U].repartition(1)

    /** Create an empty Dataframe
      * Repartition to 1, this will be a check for empty dataframes that avoids 
      * the checkpoint behavior of isEmpty
      */
    def emptyDF: DataFrame = left.sparkSession.emptyDataFrame.repartition(1)

  }

  /** Implicits for skew-aware high-level operations defined on a single Dataframe, with known key sets **/
  implicit class SkewDataframeKeyOps[K: ClassTag](dfs: (DataFrame, DataFrame, Option[String], Broadcast[Set[K]])) extends Serializable {

    val light = dfs._1
    val heavy = dfs._2
    val key = dfs._3
    val heavyKeys = dfs._4

    def print: Unit = (light, heavy).print

    def count: Long = (light, heavy).count

    def union: DataFrame = (light, heavy).union

    def select(col: String, cols: String*): (DataFrame, DataFrame, Option[String], Broadcast[Set[K]])= {
      (light.select(col, cols:_*), heavy.select(col, cols:_*), key, heavyKeys)
    }

    /** Cast a skew-triple of Dataframe to a skew-triple of Dataset,
      * does not alter the key
      */
    def as[U: Encoder : TypeTag]: (Dataset[U], Dataset[U], Option[String], Broadcast[Set[K]]) = {
      if (heavy.rdd.getNumPartitions == 1) {
        (light.as[U], light.empty[U], key, heavyKeys)
      }
      else (light.as[U], heavy.as[U], key, heavyKeys)
    }

    /** Create a new column in both the light and heavy component of a skew-triple 
      * does not alter the key
      */
    def withColumn(colName: String, col: Column): (DataFrame, DataFrame, Option[String], Broadcast[Set[K]]) = {
      (light.withColumn(colName, col), heavy.withColumn(colName, col), key, heavyKeys)
    }

    /** Rename a column in both the light and heavy component of a skew-triple 
      * does not alter the key
      */
    def withColumnRenamed(existingName: String, newName: String): (DataFrame, DataFrame, Option[String], Broadcast[Set[K]]) = {
      (light.withColumnRenamed(existingName, newName), 
        heavy.withColumnRenamed(existingName, newName), key, heavyKeys)
    }

    def drop(colName: String): (DataFrame, DataFrame) = {
      (light.drop(colName), heavy.drop(colName))
    }

    def drop(colNames: String*): (DataFrame, DataFrame) = {
      (light.drop(colNames:_*), heavy.drop(colNames:_*))
    }

    def filter(condition: Column): (DataFrame, DataFrame, Option[String], Broadcast[Set[K]]) = {
      (light.filter(condition), heavy.filter(condition), key, heavyKeys)
    }

    def crossJoin(right: (DataFrame, DataFrame)): (DataFrame, DataFrame) = {
      val result = dfs.union.crossJoin(right.union)
      (result, result.emptyDF)
    }

  }

  implicit class SkewDatasetKeyOps[T: Encoder : ClassTag, K: Encoder: ClassTag](dfs: (Dataset[T], Dataset[T], Option[String], Broadcast[Set[K]])) extends Serializable {
    val light = dfs._1
    val heavy = dfs._2
    val key = dfs._3 
    val heavyKeys = dfs._4
    val partitions = light.rdd.getNumPartitions

    def print: Unit = (light, heavy).print

    def count: Long = {
	  //println(s"heavy key size: ${heavyKeys.value.size}")
	  (light, heavy).count
	}

    def cache: Unit = (light, heavy).cache

    /** Repartition a skew-triple of Dataset type, if the key is not changed then no 
      * repartitioning will happen 
      * TODO: the first case should be tested
      */
    def repartition[S](partitionExpr: Column): (Dataset[T], Dataset[T], Option[String], Broadcast[Set[K]]) = {
      key match {
        case Some(k) if col(k) == partitionExpr => 
          //(light.repartition(Seq(partitionExpr):_*), heavy, key, heavyKeys)
          (light, heavy, key, heavyKeys)
		    case _ => 
          (light.repartition(Seq(partitionExpr):_*), heavy.repartition(Seq(partitionExpr):_*), None, 
            light.sparkSession.sparkContext.broadcast(Set.empty[K]))
      }

    }

    /** Unions the light and heavy components **/
    def union: Dataset[T] = (light, heavy).union

    def select(col: String, cols: String*): (DataFrame, DataFrame) = {
      (light.select(col, cols:_*), heavy.select(col, cols:_*))
    }

    def drop(colName: String): (DataFrame, DataFrame) = {
      (light.drop(colName), heavy.drop(colName))
    }

    def drop(colNames: String*): (DataFrame, DataFrame) = {
      (light.drop(colNames:_*), heavy.drop(colNames:_*))
    }

    def filter(condition: Column): (Dataset[T], Dataset[T], Option[String], Broadcast[Set[K]]) = {
      (light.filter(condition), heavy.filter(condition), key, heavyKeys)
    }

    /** Cast a skew-triple of Dataset to a new skew-triple of Dataset **/
    def as[U: Encoder : TypeTag]: (Dataset[U], Dataset[U], Option[String], Broadcast[Set[K]]) = {
      if (heavy.rdd.getNumPartitions == 1){
        (light.as[U], light.empty[U], key, heavyKeys)
      }else (light.as[U], heavy.as[U], key, heavyKeys)
    }

    /** Create a new column in both the light and heavy component of a skew-triple 
      * does not alter the key
      */
    def withColumn(colName: String, col: Column): (DataFrame, DataFrame, Option[String], Broadcast[Set[K]]) = {
      (light.withColumn(colName, col), heavy.withColumn(colName, col), key, heavyKeys)
    }

    /** Rename a column in both the light and heavy component of a skew-triple 
      * does not alter the key, but updates the key if it has been renamed
      */
    def withColumnRenamed(existingName: String, newName: String): (DataFrame, DataFrame, Option[String], Broadcast[Set[K]]) = {
      key match{
        case Some(k) if k == existingName =>
        (light.withColumnRenamed(existingName, newName), 
          heavy.withColumnRenamed(existingName, newName), Some(newName), heavyKeys)
        case _ => 
          (light.withColumnRenamed(existingName, newName), 
            heavy.withColumnRenamed(existingName, newName), key, heavyKeys)
      }

    }

    /** map partitions dropping the known heavy keys in the process
      */
    def mapPartitions[U: Encoder : ClassTag](func: (Iterator[T]) ⇒ Iterator[U]): (Dataset[U], Dataset[U]) = {
      (light, heavy).mapPartitions(func)
    }

    /** flatmap the light and heavy components
      * persists heavy keys
      */
    def flatMap[U: Encoder : ClassTag](func: (T) ⇒ TraversableOnce[U]): (Dataset[U], Dataset[U], Option[String], Broadcast[Set[K]]) = {
      (light.flatMap(func), heavy.flatMap(func), key, heavyKeys)
    }

    /** Equi-join this Dataset skew-triple with the right Dataset skew-triple, this is experimental for the skew implicits 
      * mainly provides a join that is a tupled version of the left and right result 
      * rather than a merged case class. 
      * When the join key is the heavy key, then use that information to filter the right relation -
      * performing the standard plan for the light and a broadcast join for the heavy
      * This drops the known heavy key information, though that might not be required
      * @param right Dataset skew-triple to join with 
      * @param leftCols a sequence of columns ordered left to right defining join columns, note this 
      * is different than the Spark native implementation of joinWith
      * @param rightCols a sequence of columns ordered left to right defining join columns
      * @param joinType string join type, default "inner"
      * @return the result of the join
      */
    def equiJoinWith[S: Encoder : ClassTag](right: (Dataset[S], Dataset[S]), leftCols: Seq[String], rightCols: Seq[String], joinType: String = "inner")
    (implicit arg0: Encoder[(T,S)]): (Dataset[(T, S)], Dataset[(T,S)]) = {

      val usingColumns = leftCols ++ rightCols

      val hkeys = key match {
        case Some(k) if usingColumns.contains(k) => heavyKeys
        case _ => light.sparkSession.sparkContext.broadcast(Set.empty[K])
      }

      // MAJOR TODO HERE FIX THIS FOR SKEW!!!
      if (hkeys.value.nonEmpty && !key.isEmpty){
        val rkey = usingColumns(1)

        val runion = right.union
        val rlight = runion.lfilter(col(rkey), hkeys)
        val lresult = light.joinWith(rlight, col(key.get) === col(rkey), joinType)

        val rheavy = runion.hfilter(col(rkey), hkeys)
        val hresult = heavy.joinWith(rheavy.hint("broadcast"), col(key.get) === col(rkey), joinType)

        (lresult, hresult)
      }else{
        (light, heavy).equiJoinWith[S, K](right, leftCols, rightCols, joinType)
      }

    }

    /** Equi-join this Dataset skew-triple with the right skew-triple that does not have known keys,
      * this returns a Dataframe, and persists the heavy keys. 
      * Does the standard plan for the light component, and broadcasts the heavy component
      * @param right Dataset skew-triple to join with 
      * @param usingColumns a sequence of columns ordered left to right defining join columns, note this 
      * is different than the Spark native implementation of joinWith
      * @param joinType string join type, default "inner"
      * @return the result of the join
      */
    def equiJoin[S: Encoder : ClassTag, J: ClassTag](right: (Dataset[S], Dataset[S]), leftCols: Seq[String], rightCols: Seq[String], joinType: String): (DataFrame, DataFrame, Option[String], Broadcast[Set[K]]) = {
	    
      val usingColumns = leftCols ++ rightCols

      val hkeys = key match {
        case Some(k) if usingColumns.contains(k) => heavyKeys
        case _ => light.sparkSession.sparkContext.broadcast(Set.empty[K])
      }
      if (hkeys.value.nonEmpty && !key.isEmpty){
        val rkey = usingColumns(1)
        val runion = right.union
        val rlight = runion.lfilter(col(rkey), hkeys)
        val lresult = light.join(rlight, col(key.get) === col(rkey), joinType)

        val rheavy = runion.hfilter(col(rkey), hkeys)
        val hresult = heavy.join(rheavy.hint("broadcast"), col(key.get) === col(rkey), joinType)

        (lresult, hresult, key, hkeys)
      }else{
        (light, heavy).equiJoin[S, K](right, leftCols, rightCols, joinType)
      }

    }

    def equiJoin[S: Encoder : ClassTag, J: ClassTag](right: (Dataset[S], Dataset[S], Option[String], Broadcast[Set[K]]), leftCols: Seq[String], rightCols: Seq[String], joinType: String): (DataFrame, DataFrame, Option[String], Broadcast[Set[K]]) = {
      equiJoin[S,J]((right.light, right.heavy), leftCols, rightCols, joinType)
    }

    /** Default join for complex join conditions **/
    def join[S: Encoder : ClassTag](right: (Dataset[S], Dataset[S]), cond: Column, joinType: String = "inner"): (DataFrame, DataFrame) = {
      val result = dfs.union.join(right.union, cond, joinType)
      (result, result.emptyDF)
    }

    /** Grouping operations will drop heavy keys and call the skew-triple version with no known keys **/

    def reduceByKey[K: Encoder](key: (T) => K, value: (T) => Double)(implicit arg0: Encoder[(K, Double)]): (Dataset[(K, Double)], Dataset[(K, Double)]) = {
      (light, heavy).reduceByKey(key, value)
    }

    def unionGroupByKey(f: (T) => K)(implicit arg0: Encoder[(K, T)]): KeyValueGroupedDataset[K, T] = {
      (light, heavy).unionGroupByKey(f)
    }

    def unionGroupByKey[S: Encoder : ClassTag](f: (T) => S)(implicit arg0: Encoder[(S, T)]): KeyValueGroupedDataset[S, T] = {
      (light, heavy).unionGroupByKey(f)
    }

    def groupByKey[K: Encoder](f: (T) => K): (KeyValueGroupedDataset[K, T], KeyValueGroupedDataset[K, T]) = {
      (light, heavy).groupByKey(f)
    }

    def cogroup[S: Encoder : ClassTag, R : Encoder: ClassTag, K : Encoder: ClassTag](right:  KeyValueGroupedDataset[K,S], lkey: (T) => K)
      (f: (K, Iterator[T], Iterator[S]) => TraversableOnce[R]): (Dataset[R], Dataset[R]) = {
      (light, heavy).cogroup(right, lkey)(f)
    }

    def cogroup[S: Encoder : ClassTag, R : Encoder: ClassTag](right: (Dataset[S], Dataset[S]), lkey: (T) => K, rkey: (S) => K)
      (f: (K, Iterator[T], Iterator[S]) => TraversableOnce[R]): (Dataset[R], Dataset[R]) = {
        (light, heavy).cogroup(right, lkey, rkey)(f)
    }

    def cogroup[S: Encoder : ClassTag, R : Encoder: ClassTag](right: (Dataset[S], Dataset[S], Option[String], Broadcast[Set[K]]), lkey: (T) => K, rkey: (S) => K)
      (f: (K, Iterator[T], Iterator[S]) => TraversableOnce[R]): (Dataset[R], Dataset[R]) = {
        (light, heavy).cogroup((right._1, right._2), lkey, rkey)(f)
    }

    def crossJoin[S: Encoder : ClassTag](right: (Dataset[S], Dataset[S])): (DataFrame, DataFrame) = {
      (light, heavy).crossJoin(right)
    }

  }

  implicit class SkewDataframeOps(dfs: (DataFrame, DataFrame)) extends Serializable {

    val light = dfs._1
    val heavy = dfs._2

    /** counts the light and heavy component, returns as sum **/
    def count: Long = if (heavy.rdd.getNumPartitions == 1) {
	  	val l = light.count
		println(s"light: $l, heavy: 0")
		l
      } else {
        val l = light.count
        val h = heavy.count
		println(s"light: $l, heavy: $h")
		l + h
      }

    /** prints the light and heavy components **/
    def print: Unit = {
      println("light")
      println(light.take(10).toList.map(f => 
        Util.getCCParams(f.asInstanceOf[AnyRef])).mkString("{\n", ",\n", "}\n"))
      println("heavy")
      println(heavy.take(10).toList.map(f => 
        Util.getCCParams(f.asInstanceOf[AnyRef])).mkString("{\n", ",\n", "}\n"))
    }

    /** union the light and heavy component, 
      * if heavy is empty then just return light 
      */
    def union: DataFrame = if (heavy.rdd.getNumPartitions == 1) light 
      else light.union(heavy)

    def select(col: String, cols: String*): (DataFrame, DataFrame) = {
      (light.select(col, cols:_*), heavy.select(col, cols:_*))
    }

    def filter(condition: Column): (DataFrame, DataFrame) = {
      (light.filter(condition), heavy.filter(condition))
    }

    def drop(colName: String): (DataFrame, DataFrame) = {
      (light.drop(colName), heavy.drop(colName))
    }

    def drop(colNames: String*): (DataFrame, DataFrame) = {
      (light.drop(colNames:_*), heavy.drop(colNames:_*))
    }

    def as[U: Encoder : TypeTag]: (Dataset[U], Dataset[U]) = {
      if (heavy.rdd.getNumPartitions == 1){
        (light.as[U], light.empty[U])
      }else (light.as[U], heavy.as[U])
    }

    def withColumn(colName: String, col: Column): (DataFrame, DataFrame) = {
      (light.withColumn(colName, col), heavy.withColumn(colName, col))
    }

    def withColumnRenamed(existingName: String, newName: String): (DataFrame, DataFrame) = {
      (light.withColumnRenamed(existingName, newName), 
        heavy.withColumnRenamed(existingName, newName))
    }

    def crossJoin(right: (DataFrame, DataFrame)): (DataFrame, DataFrame) = {
      val result = dfs.union.crossJoin(right.union)
      (result, result.emptyDF)
    }

  }

  implicit class SkewDatasetOps[T: Encoder : ClassTag](dfs: (Dataset[T], Dataset[T])) extends Serializable {
    
    val light = dfs._1
    val heavy = dfs._2
    val partitions = light.rdd.getNumPartitions
    val random = scala.util.Random
	  val thresh = Config.threshold
    val sampled = Config.sample
    val strategy = Config.heavyKeyStrategy

    /** counts the light and heavy component, returns as sum **/
    def count: Long = if (heavy.rdd.getNumPartitions == 1) {
	  	val l = light.count 
		  println(s"light: $l, heavy: 0")
		  l
      } else {
        val lc = light.count
        val hc = heavy.count
        println(s"light: $lc, heavy: $hc")
		lc + hc
      }

    /** prints the light and heavy components **/
    def print: Unit = {
      println("light")
      println(light.take(10).toList.map(f => 
        Util.getCCParams(f.asInstanceOf[AnyRef])).mkString("{\n", ",\n", "}\n"))
      println("heavy")
      println(heavy.take(10).toList.map(f => 
        Util.getCCParams(f.asInstanceOf[AnyRef])).mkString("{\n", ",\n", "}\n"))
    }

    def cache: Unit = {
      light.cache
      heavy.cache
    }

    /** Repartitions the light component, and keeps the heavy keys in their current location.
      * This first calculates the heavy keys of the unioned light and heavy components. 
      * If the heavy keys are empty, the skew-triple have an empty heavy dataset and a null key set
      * @param partitionExpr column to repartition on 
      * @return skew-triple with light component repartitioned and a known set of heavy keys
      */
    def repartition[K: ClassTag](partitionExpr: Column): (Dataset[T], Dataset[T], Option[String], Broadcast[Set[K]]) = {
      val key = partitionExpr.toString
      val (dfull, hkeys) = heavyKeys[K](key)
      if (hkeys.nonEmpty){
        val hk = dfull.sparkSession.sparkContext.broadcast(hkeys)
        (dfull.lfilter[K](col(key), hk).repartition(Seq(partitionExpr):_*), dfull.hfilter[K](col(key), hk), Some(key), hk)
      }else (light.repartition(Seq(partitionExpr):_*), heavy.repartition(Seq(partitionExpr):_*),
        None, light.sparkSession.sparkContext.broadcast(Set.empty[K]))
    }

    /** union the light and heavy component, 
      * if heavy is empty then just return light 
      */
    def union: Dataset[T] = if (heavy.rdd.getNumPartitions == 1) light 
      else light.union(heavy)

    def select(col: String, cols: String*): (DataFrame, DataFrame) = {
      (light.select(col, cols:_*), heavy.select(col, cols:_*))
    }

    def filter(condition: Column): (Dataset[T], Dataset[T]) = {
      (light.filter(condition), heavy.filter(condition))
    }

    def drop(colName: String): (DataFrame, DataFrame) = {
      (light.drop(colName), heavy.drop(colName))
    }

    def drop(colNames: String*): (DataFrame, DataFrame) = {
      (light.drop(colNames:_*), heavy.drop(colNames:_*))
    }

    def as[U: Encoder : TypeTag]: (Dataset[U], Dataset[U]) = if (heavy.rdd.getNumPartitions == 1) (light.as[U], light.empty[U])
      else (light.as[U], heavy.as[U])

    def withColumn(colName: String, col: Column): (DataFrame, DataFrame) = {
      (light.withColumn(colName, col), heavy.withColumn(colName, col))
    }

    def withColumnRenamed(existingName: String, newName: String): (DataFrame, DataFrame) = {
      (light.withColumnRenamed(existingName, newName), 
        heavy.withColumnRenamed(existingName, newName))
    }

    /** Heavy key calculation using a string for the column name
      * samples 10% while counting the partition - which is later used
      * to adjust the threshold to the size of the partition 
      * a key is heavy when 2.5% of the values on a partition are associated 
      * to that key.
      * @param key string to determine heavy key set
      * @return the unioned dataset and a set of heavy keys 
      */
    def heavyKeys[K: ClassTag](key: String): (Dataset[T], Set[K]) = {
      val dfull = dfs.union
      val keys = strategy match {
        case "full" => fullHeavyKeys[K](dfull, key)
		case "partial" => partialHeavyKeys[K](dfull, key)
		case "sample" => sampleHeavyKeys[K](dfull, key)
        case "slice" => sliceHeavyKeys[K](dfull, key)
        case _ => sys.error(s"unsupported heavy key strategy: $strategy.")
      }
      //println(s"heavy keys for $key: ${keys.size}")
	  (dfull, keys.asInstanceOf[Set[K]])
    }

	/** filter by a threshold and then use the number 
		of partitions as the final filter?
	**/
	def fullHeavyKeys[K: ClassTag](dfull: Dataset[T], key: String): Set[K] = {
      dfull.select(key).rdd.mapPartitions(it => {
        var cnt = 0
        val acc = HashMap.empty[Row, Int].withDefaultValue(0)
        it.foreach{ c => 
          cnt +=1
          c match { case null => Unit
            case _ => acc(c) += 1 }}		
        acc.filter(_._2 > (cnt*thresh)).iterator
      }).reduceByKey(_+_).filter(_._2 > partitions).map(r => r._1.getAs[K](0)).collect.toSet
    }

	def partialHeavyKeys[K: ClassTag](dfull: Dataset[T], key: String): Set[K] = {
      dfull.select(key).rdd.mapPartitions(it => {
        var cnt = 0
        val acc = HashMap.empty[Row, Int].withDefaultValue(0)
        it.foreach{ c => 
          cnt +=1
          c match { case null => Unit
            case _ => acc(c) += 1 }}
        acc.filter(_._2 > (cnt*thresh)).map(r => r._1.getAs[K](0)).iterator
      }).collect.toSet
    }

    def sampleHeavyKeys[K: ClassTag](dfull: Dataset[T], key: String): Set[K] = {
      dfull.select(key).rdd.mapPartitions(it => {
        var cnt = 0
        val acc = HashMap.empty[Row, Int].withDefaultValue(0)
        it.foreach{ c => 
          cnt +=1
          c match { case null => Unit
            case _ => if (random.nextDouble <= .1) acc(c) += 1 }}
        acc.filter(_._2 > (cnt*.1)*thresh).map(r => r._1.getAs[K](0)).iterator
      }).collect.toSet
    }

    def sliceHeavyKeys[K: ClassTag](dfull: Dataset[T], key: String): Set[K] = {
      dfull.select(key).rdd.mapPartitions(it => {
        var cnt = 0
        val acc = HashMap.empty[Row, Int].withDefaultValue(0)
        while (cnt < sampled && it.hasNext) { cnt += 1; it.next match { case null => Unit; case c => acc(c) += 1 }}
        if (cnt < sampled) Iterator()
        else {
		  val accsize = acc.size
		  val accsum = acc.values.sum
          val avg = accsum / accsize
		  acc.filter(_._2 > avg*thresh).map(r => r._1.getAs[K](0)).iterator
        }
      }).collect.toSet
    }

    def mapPartitions[U: Encoder : ClassTag](func: (Iterator[T]) ⇒ Iterator[U]): (Dataset[U], Dataset[U]) = {
      (light.mapPartitions(func), heavy.mapPartitions(func))
    }

    def flatMap[U: Encoder : ClassTag](func: (T) ⇒ TraversableOnce[U]): (Dataset[U], Dataset[U]) = {
      (light.flatMap(func), heavy.flatMap(func))
    }

    /** Determines if the join key is heavy, if it is then call the equiJoinWith in the skew-triple implicits with
      * known heavy keys. If the heavy key set is empty, then perform the Dataset equiJoinWith and return an empty
      * heavy component
      * @param right skew-triple Dataset with no known heavy keys
      * @param usingColumns an ordered Seq of string column names
      * @param joinType identifies the type of join
      * @return skew-triple result from the skew-aware equiJoinWith
      */
    def equiJoinWith[S: Encoder : ClassTag, K: Encoder : ClassTag](right: (Dataset[S], Dataset[S]), leftCols: Seq[String], rightCols: Seq[String], joinType: String = "inner")
      (implicit arg0: Encoder[(T,S)]): (Dataset[(T, S)], Dataset[(T,S)]) = {
      val usingColumns = leftCols ++ rightCols
      val nkey = usingColumns(0)
      val (dfull, hk) = heavyKeys[K](nkey)
      if (hk.nonEmpty){
        val hkeys = dfull.sparkSession.sparkContext.broadcast(hk)
        (dfull.lfilter[K](col(nkey), hkeys), dfull.hfilter[K](col(nkey), hkeys), Some(nkey), hkeys).equiJoinWith(right, leftCols, rightCols, joinType)
      }else{
        val result = dfull.equiJoinWith(right.union, leftCols, rightCols, joinType)
        (result, result.sparkSession.emptyDataset[(T,S)].repartition(1))
      }
    }

    /** Determines if the join key is heavy, if it is then call the equiJoin in the skew-triple implicits with
      * known heavy keys. If the heavy key set is empty, then perform the Dataset equiJoin and return an empty
      * heavy component. This persists the heavy keys.
      * @param right skew-triple Dataset with no known heavy keys
      * @param usingColumns an ordered Seq of string column names
      * @param joinType identifies the type of join
      * @return skew-triple result from the skew-aware equiJoinWith
      */
    def equiJoin[S: Encoder : ClassTag, K: ClassTag](right: (Dataset[S], Dataset[S]), leftCols: Seq[String], rightCols: Seq[String], joinType: String)(implicit arg0: Encoder[K]): 
    (DataFrame, DataFrame, Option[String], Broadcast[Set[K]]) = {
      val usingColumns = leftCols ++ rightCols
      val nkey = usingColumns(0)
      val (dfull, hk) = heavyKeys[K](nkey)
      if (hk.nonEmpty){
        val hkeys = dfull.sparkSession.sparkContext.broadcast(hk)
        (dfull.lfilter[K](col(nkey), hkeys), dfull.hfilter[K](col(nkey), hkeys), Some(nkey), hkeys).equiJoin[S,K](right, leftCols, rightCols, joinType)
      }else{
        (dfull.equiJoin(right.union, leftCols, rightCols, joinType), light.emptyDF, Some(nkey), light.sparkSession.sparkContext.broadcast(Set.empty[K]))
      }
    }
    
    def equiJoin[S: Encoder : ClassTag, K: ClassTag](right: (Dataset[S], Dataset[S], Option[String], Broadcast[Set[K]]), leftCols: Seq[String], rightCols: Seq[String], joinType: String)(implicit arg0: Encoder[K]): 
      (DataFrame, DataFrame, Option[String], Broadcast[Set[K]]) = {
        equiJoin[S,K]((right.light, right.heavy), leftCols, rightCols, joinType)
    }
 
    /** Default join for complex join conditions **/
    def join[S: Encoder : ClassTag](right: (Dataset[S], Dataset[S]), cond: Column, joinType: String): (DataFrame, DataFrame) = {
      val result = dfs.union.join(right.union, cond, joinType)
      (result, result.emptyDF)
    }

    /** Grouping operations - union the light and heavy components and perform the standard operations. 
      * The heavy component is empty and the heavy key set is null.
      */

    def reduceByKey[K: Encoder](key: (T) => K, value: (T) => Double)(implicit arg0: Encoder[(K, Double)]): (Dataset[(K, Double)], Dataset[(K, Double)]) = {
      val dfull = dfs.union
      val result = dfull.reduceByKey(key, value)
      (result, result.empty)
    }

    def unionGroupByKey[K: Encoder : ClassTag](f: (T) => K)(implicit arg0: Encoder[(K, T)]): KeyValueGroupedDataset[K, T] = {
      dfs.union.groupByKey(f)
    }

    def groupByKey[K: Encoder](f: (T) => K): (KeyValueGroupedDataset[K, T], KeyValueGroupedDataset[K, T]) = {
      val dfull = dfs.union
      (dfull.groupByKey(f), dfull.empty.groupByKey(f))
    }

    def cogroup[S: Encoder : ClassTag, R : Encoder: ClassTag, K : Encoder: ClassTag](right: KeyValueGroupedDataset[K,S], lkey: (T) => K)
      (f: (K, Iterator[T], Iterator[S]) => TraversableOnce[R]): (Dataset[R], Dataset[R]) = {
      val result = dfs.union.cogroup(right, lkey)(f)
      (result, result.empty)
    }

    def cogroup[S: Encoder : ClassTag, R : Encoder: ClassTag, K : Encoder: ClassTag](right: (Dataset[S], Dataset[S]), lkey: (T) => K, rkey: (S) => K)
      (f: (K, Iterator[T], Iterator[S]) => TraversableOnce[R]): (Dataset[R], Dataset[R]) = {
        val result = dfs.union.cogroup(right.union, lkey, rkey)(f)
        (result, result.empty)
    }

    def cogroup[S: Encoder : ClassTag, R : Encoder: ClassTag, K : Encoder: ClassTag](right: (Dataset[S], Dataset[S], Option[String], Broadcast[Set[K]]), lkey: (T) => K, rkey: (S) => K)
      (f: (K, Iterator[T], Iterator[S]) => TraversableOnce[R]): (Dataset[R], Dataset[R]) = {
        dfs.cogroup((right._1, right._2), lkey, rkey)(f)
    }

    def crossJoin[S: Encoder: ClassTag](right: (Dataset[S], Dataset[S])): (DataFrame, DataFrame) = {
      val result = dfs.union.crossJoin(right.union)
      (result, result.emptyDF)
    }

  }

  /** skew-aware implicits for KeyValueGroupedDataset with null heavy keys 
    * since grouping operations do not handle skew there should never be heavy key sets 
    * associated to a KeyValueGroupedDataset
    */
  implicit class SkewKeyValueDatasetOps2[K: Encoder : ClassTag, V: Encoder : ClassTag](dfs: (KeyValueGroupedDataset[K, V], KeyValueGroupedDataset[K, V])) extends Serializable {

    val light = dfs._1
    val heavy = dfs._2

    def agg[U1 : Encoder : ClassTag](col1: TypedColumn[V, U1]): (Dataset[(K, U1)], Dataset[(K, U1)]) = {
      (light.agg(col1), heavy.agg(col1))
    }

    def agg[U1 : Encoder : ClassTag, U2 : Encoder : ClassTag](col1: TypedColumn[V, U1], col2: TypedColumn[V, U2]): (Dataset[(K, U1, U2)], Dataset[(K, U1, U2)]) = {
      (light.agg(col1, col2), heavy.agg(col1, col2))
    }

    def mapGroups[S: Encoder: ClassTag](func: (K, Iterator[V]) => S): (Dataset[S], Dataset[S]) = {
      (light.mapGroups(func), heavy.mapGroups(func))
    }

  }
  

}
