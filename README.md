## NRC Compilation Framework 

The framework is organized into two subfolders:
* `compiler` contains all the components for running the standard and shredded pipeline, both skew-unaware and skew-aware. This goes from NRC to generated code.
* `executor` contains plan operator implementations specific to the targets of the code generator. Currently, only Spark is supported.

### Example

For now, NRC queries are described directly in Scala. There are several examples of how to do this 
in `compilers/src/main/scala/framework/examples/`. Here we describe how to generate and 
execute code for an example from the TPC-H benchmark.

To run an example, we will generate code into the `executor` package and then compile it 
into an application jar. To start run the following in your terminal:

```
mkdir -p executor/spark/src/main/scala/sparkutils/generated/
cd compilers
sbt run
```

Select application 2. This will generate two files:
* standard pipeline: `../executor/spark/src/main/scala/sparkutils/generated/Test2Spark.scala`
* shredded pipeline:`../executor/spark/src/main/scala/sparkutils/generated/ShredTest2UnshredSpark.scala`

Note that these files are generated inside the executor package. This is because it will need to 
access the same `spark.sql.implicits` as the functions inside this package. 

Navigate to `executor/spark`. Update `data.flat` with your configuration details, and compile the package:

```
cd executors/spark
sbt package
```

You can now run the application with spark-submit. For example, to run the 
application defined in `Test2Spark.scala` as a local spark job do:

```
spark-submit --class sparkutils.generated.Test2Spark \
  --master "local[*]" target/scala-2.12/sparkutils_2.12-0.1.jar
```

If you would like your output to print to console, you can edit the generated 
application jar to do so. Just uncomment the line `Test2.print`, then redirect to 
stdout: 

```
spark-submit --class sparkutils.generated.Test2Spark \
  --master "local[*]" target/scala-2.12/sparkutils_2.12-0.1.jar > out
```
