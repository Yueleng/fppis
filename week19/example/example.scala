case class Demographic(id: Int,
                       age: Int,
                       codingBootcamp: Boolean,
                       country: String,
                       gender: String,
                       isEthnicMinority: Boolean,
                       servedInMilitary: Boolean)
val demographics = sc.textfile(...)... // Pard RDD, (id, demographic)

case class Finances(id: Int,
                    hasDebt: Boolean,
                    hasFinancialDependents: Boolean,
                    hasStudentLoans: Boolean,
                    income: Int)

val finances = sc.textfile(...)... // Pair RDD, (id, finances)

// Let's count:
// Swiss students 
// Who have debt & financial dependents

// Possibility 1:
demographics.join(finances) // (id: Int, (Demographic, Finances))
            .filter { p => 
                p._2._1.country == "Switzerland" && 
                p._2._2.hasFinancialDependents && 
                p._2._2.hasDebt
            }.count

// Steps:
// 1. Inner join first
// 2. Filter to select people in Switzerland
// 3. Filter to select people with debt & financial dependents


// Possibility 2
val filtered = finances.filter( p => p._2.hasFinancialDependents && p._2.hasDebt)

demographics.filter(p => p._2.country == "Switzerland")
            .join(filtered)
            .count

// Steps:
// 1. Filter down the dataset first (look at only people with debt & financial dependents)
// 2. Filter to select people in Switzerland (look at only people in Switzerland)
// 3. Inner join on smaller, filtered down dataset


// Possibility 3
val cartesian = demographics.cartesian(finances)

cartesian.filter {
    case (p1, p2) => p1._1 == p2._1
}
.filter {
    case (p1, p2) => (p1._2.country == "Switzerland") && 
                     (p2._2.hasFinancialDependents) &&
                     (p2._2.hasDebt)
}.count

// Steps
// 1. Cartesian product on both datasets
// 2. Filter to select resulting of cartesian with same IDs
// 3. Filter to select people in Switzerland who have debt and financial dependents


// Spark SQL
// 1. possible to seamlessly intermix SQL queries with Scala
// 2. get all of the optimizations we're used to in the database community on Spark jobs

// Main goals:
// 1. Support relational processing both within Spark programs and on external data sources with a friendly API.
// 2. High performance, achieved by using techniques from research in databases.
// 3. Easily support new data sources such as semi-structred data and external databses.

// ----------------------------------------------------

// Everything starts with the SparkSession
import org.apache.spark.sql.SparkSession
val spark = SparkSession
            .builder()
            .appName("My App")
            // .config("spark.some.config.option", "some-value")
            .getOrCreate()

// DataFrame can be created in two ways:
// 1. From an existing RDD.
// 2. Reading in a specific data source from file (such JSON)

val tupleRDD = ... // Assume RDD[(Int, String, String, String)]
val tupleDF = tupleRDD.toDF("id", "name", "city", "country") // column names
// Note: If you use toDF without arguments, Spark will assign numbers as attributes (column names)
//       to your DataFrame


// If you already have an RDD containing some kind of case class instance,
// then Spark can infer the attributes from the case class's fields

case class Person(id: Int, name: String, city: String)
val peopleRDD = ... // Assume RDD[Person]
val peopleDF = peopleRDD.toDF

// ----------------------------------------------------

// Create DataFrame from existing RDD, schema explicitly specified
// The schema is encoded in a string
case class Person(name: String, age: Int)
val peopleRdd = sc.textFile(...) // Assume RDD[Person]

// The schema is encoded in a string
val schemaString = "name age"

// Generate the schema based on the string of schema
val fields = schemaString.split(" ")
                         .map(fieldName => StructField(fieldName, StringType, nullable = true))
val schema = StructType(fields)

// Convert records of the RDD (people) to Rows
val rowRDD = peopleRDD.map(_.split(","))
                      .map(attributes => Row(attributes(0), attributes(1).trim))

// Apply the schema to the RDD
val peopleDF = spark.createDataFrame(rowRDD, schema)

// ----------------------------------------------------

// Create DataFrame by reading in a data source from file.
// 'spark' is the SparkSession object we created a few slides back
val df = spark.read.json("example/src/main/resources/people.json")


// ----------------------------------------------------
// SQL Literals

// Register the DataFrame as a SQL temporary view
peopleDF.createOrReplaceTempView("people")

// This essentially gives a name to our DataFrame in SQL
// so we can refer to it in an SQL FROM statement

// SQL literals can be passed to Spark's sql method
val adultsDF = spark.sql("SELECT * FROM people WHERE age > 17")

// ----------------------------------------------------
// A more interesting SQL Query
case class Employee(id: Int, fname: String, lname: String, age: Int, city: String)
// DateFrame with schema defined in Employee case class
val employeeDF = sc.parallelize(...).toDF

val sydneyEmployeesDF 
  = spark.sql("""SELECT id, lname FROM employees WHERE city = "Sydney" ORDER BY id""")


// ----------------------------------------------------
// DataFrames have their own APIs as well

// Scala type
case class Person(name: String, age: Int)

// SQL type
StructType(List(StructField("name", StringType, true)), StructField("name", StringType, true)))

// In order to access any of these data types, either basic or complex, you must first import 
// Spark SQL types!
import org.apache.spark.sql.types._

// .show()
case class Employee(id: Int, fname: String, lname: String, age: Int, city: String)
val employeeDF = sc.parallelize(...).toDF
employeeDF.show() // show the SQL table style data
employeeDF.printSchema() // prints the schema of your DataFrame in a tree format

// Some common transformations include:
def select(col: String, cols: String*): DataFrame
// selects a set of named columns and returns a new DataFrame with these
// columns as a result

def agg(exp: Column, exprs: Column*): DataFrame
// performs aggregations on a series of columns and returns a new DataFrame 
// with the calculated output.

def groupBy(col1: String, cols: String*): DataFrame // simplified
// groups the DataFrame using the specified columns. Intended to be used before an aggregation.

def join(right: DataFrame): DataFrame // simplified
// inner join with another DataFrame


// Using $-notation
df.filter($"age" > 18)

// Referring to the Dataframe 
df.filter(df("age") > 18)

// SQL query string
df.filter("age > 18")

// Transformations: Example
sydneyEmployeesDF = employeeDF.select("id", "lname")
                              .where("city == 'Sydney'")
                              .orderBy("id")


// Filtering in Spark SQL
val over30 = employeeDF.filter("age > 30").show()
val over30 = employeeDF.where("age > 30").show()

// more complext filter
employeeDF.filter(($"age" > 25) && ($"city" === "Sydney")).show()

// Group and aggregate
df.groupBy($"attribute1")
  .agg(sum($"attribute2"))

df.groupBy($"attribute1")
  .count($"attribute2")


// Example
case class Listing(street: String, zip: Int, price: Int)
val listingsDF = ... // DataFrame of Listings


case class Post(authorID: Int, subforum: String, likes: Int, date: String)

val postsDF = ... // DataFrame of Posts

import org.apache.spark.sql.functions._

val rankedDF = 
  postsDF.groupBy($"authorID", $"subforum") // : RelationalGroupedDataset
         .agg(count($"authorID")) // new DF with columns authorID, subforum, count(authorID)
         .orderBy($"subforum", $"count(authorID)".desc)

// After calling groupBy, methods on RelationalGroupedDataset: 
//   To see a list of all operations
// http : //spark.apache.org/docs/latest/api/scala/index.html#org.apache.spark.sql.RelationalGroupedDataset

// Methods within agg: 
// Examples includes：min, max, sum, mean, stddev, count, avg, first, last.
// http : //spark.apache.org/docs/latest/api/scala/index.html#org.apache.spark.sgl.functions$


// ----------------------------------------------------

.drop() // : drops rows that contain null or NaN values in any column and returns a new DataFrame
.drop("all") //  drops rows that contains null or Nan values in all columns and returns a new DataFrame
.drop(Array("id", "name")) // drops rows that contain null or NaN values in the specified columns and returns a new DataFrame

fill(0) // replaces all occurrences of null or NaN in numeric columns with specified value and returns a new DataFrame
fill(Map("minBalance" -> 0)) // replaces all occurrences of null or NaN in specified column with specified value and returns a new DataFrame
replace(Array("id"), Map(1234 -> 8923))  // replace specified value(1234) in specified column (id) with specified replacement value (8923) and returns a new DataFrame.


// Join

df1.join(df2, $"df1.id" === $"df2.id")

df1.join(df2, $"df1.id" === $"df2.id", "right_outer") 


// CFF data
case class Abo(id: Int, v: (String, String))
case class Loc(id: Int, v: String)

val as = List(Abo(101, ("Ruetli", "AG")), Abo(102, ("Brelaz", "DemiTarif")),Abo(103, ("Gress", "DemiTarifVisa")), Abo(104, ("Schatten", "DemiTarif")))

val abosDF = sc.parallelize(as).toDF
val ls = List(Loc(101, "Bern"), Loc(101, "Thun"), Loc(102, "Lausanne"), Loc(102, "Geneve"),Loc(102, "Nyon"), Loc(103, "Zurich"), Loc(103, "St-Gallen"), Loc(103, "Chur"))

val locationsDF = sc.parallelize(ls).toDF

val trackedCustomersDF = abosDF.join(locationsDF,abosDF("id") === locationsDF("id"))

val abosWithOptionalLocationsDf = abosDF.join(locationsDF,abosDF("id") === locationsDF("id"), "left_outer")


// revist the Demographics and Finance DataType

demographicsDF.join(financesDf, demographicsDF("ID") === financesDF("ID"), "inner")
              .filter($"HasDebt" && $"HasFinancialDependents")
              .filter($"CountryLive" === "Switzerland")
              .count

// Catalyst complies Spark SQL programs down to an RDD and do optimization by 
// Laziness and structure 


listingsDF.filter($"state" === "CA")
// Limitations of DF: runtime exceptions: cannot result "state"
// Limitations of DF: Limited Data Types


// ----------------------------------------------------
// DataSet
case class Listing(street: String, zip: Int, price: Int)
val listingsDF = ... // DataFrame of Listings

import org.apache.spark.sql.functions._
val averagePricesDF = listingsDF.groupBy($"zip")
                                .avg("price")

val averagePrices = averagePricesDF.collect()
// collect() bring back to master node.
// averagePrices: Array[org.apache.spark.sql.Row]

// cast
val averagePricesAgain = averagePrices.map {
    row => (row(0).asInstanceOf[String], row(1).asInstanceOf[Int])
}
// java.lang.ClassCastException

averagePrices.head.schema.printTreeString()
// root
//  |-- zip: integer (nullable = true)
//  |-- avg(price): double (nullable = true)

val averagePricesAgain = averagePrices.map {
    row => (row(0).asInstanceOf[Int], row(1).asInstanceOf[Double])
}

// DataSets: have both Spark SQL optimizations and typesafety.
type DataFrame = DataSet[Row]

// listingsDs: Dataset[Listing]
listingDS.groupByKey(l => l.zip)  // looks like groupByKey on RDDs!
         .agg(avg($"price").as[Double]) // looks like out DataFra on RDDs!

// Datasets are a something in the middle between DataFrames and RDDs

// DataFrame to DataSet
import spark.implicits._
myDF.toDS

// from Json
val myDS = spark.read.json("people.json").as[Person]

// from RDD
import spark.implicits._
myRDD.toDS

// from common Scala types
List("yay", "ohnoes", "hooray!").toDS // requires import spark.implicits._

// going from a DataFrame to a Dataset via typed transformations
val keyValuesDF = List((3,"Me"),(1,"Thi"),(2,"Se"),(3,"ssa"),(3,"-)"),(2,"cre"),(2,"t")).toDF
val res = keyValuesDF.map(row => row(0).asInstanceOf[Int] + 1)

// agg
someDs.agg(avg($"column").as[Double])

// ReduceByKey?

// mapGroupds
val keyValues = List((3,"Me"),(1,"Thi"),(2,"Se"),(3,"ssa"),(3,"-)"),(2,"cre"),(2,"t"))
val keyValuesDS = keyValues.toDS

keyValuesDS.groupByKey(p => p._1)
           .mapGroups((k, vs) => (k, vs.foldLeft("")((acc, p) => acc + p._2))).show() // ._2??
           .sort($"_1").show()


// mapValues
val keyValuesDS = keyValues.toDS
keyValuesDS.groupByKey(p => p._1)
           .mapValues(p => p._2)
           .reduceGroups((acc, str) => acc + str)


// Aggregator[-IN, BUF, OUT]
import org.apache.spark.sql.expressions.Aggregator

val strConcat = new Aggregator[(Int, String), String, String] {
  def zero: String = ""
  def reduce(b:String, a:(Int, String)): String = b + a._2
  def merge(b1: String, b2: String): String = b1 + b2
  def finish(r: String): String = r

  // two extra encoder
  override def bufferEncoder: Encoder[String] = Encoders.STRING
  override def outputEncoder: Encoder[String] = Encoders.STRING

}.toColumn

keyValuesDS.groupByKey(pair => pair._1)
           .agg(strConcat.as[String]).show


// Common DataSet Actions
// collect(): Array[T]
// Returns an array that contains all of Rows in this Dataset

// count(): Long
// Returns the number of rows in the Dataset.

// first(): T/head(): T
// Returns the first row in this Dataset.

// foreach(f: T => Unit): Unit
// Applies a function f to all rows.

// reduce(f: (T, T) => T): T
// Reduces the elements of this Dataset using the specified binary function.

// show(): Unit
// Displays the top 20 rows of Dataset in a tabular form.

// take(n: Int): Array[T]
// Returns the first n rows in the Dataset.

