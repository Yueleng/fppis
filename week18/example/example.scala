val pairs = sc.parallelize(List((1, "one"), (2, "two"), (3, "three")))
pairs.groupBy()
// res2: org.apache.spark.rdd.RDD[(Int, Iterable[String])]
//  = ShuffledRdd[16] at groupByKey at <console>: 37

// Shuffles can be an enormous hit because it means that Spark must send
// data from one node to another. WHY? Latency!

// ------------------------------------------------------

case class CFFPurchase(customerId: Int, destination: String, price: Double)
val purchasesRdd: RDD[CFFPurchase] = sc.textFile(...)

// Goal: Calculate how many trips, and how much money was spent by each 
//       individual customer over the course of the month

val purchasePerMonth = 
    purchasesRdd.map(p => (p.customerId, p.price)) // Pair RDD
                .groupByKey() // groupByKey returns RDD[K, Iterable[V]]
                .map(p => (p._1, (p._2.size, p._2.sum))
                .collect() // trigger calculation

// Node: GroupByKey results in one key-value pair per key. And this single 
//       key-value pair cannot span across multiple worker nodes.
// We don't want to be sending all of our data over the network if it's not
// absolutely required. Too much network communication kills performances.

// ------------------------------------------------------

// Signature:
def reduceByKey(func: (V, V) => V): RDD[(K, V)]
val purchasePerMonth = 
    purchasesRdd.map(p => (p.customerId, (1, p.price))) // Pair RDD
                .reduceByKey(...) // ?

// Notice that the function passed to MAP has changed.
// What function do we pass to reduceByKey?


val purchasePerMonth = 
    purchasesRdd.map(p => (p.customerId, (1, p.price))) // Pair RDD
                .reduceByKey((v1, v2) => (v1._1 + v2._1, v1._2 + v2._2))
                .collect()

// By reducing the dataset first, the amount of data sent over the network
// during the shuffle is greatly reduced.


// --------------------------------------------------------

// Partitioning
//   1. Call partitionBy on an RDD, providing an explicit Partitioner.
//   2. Using transformations that return RDDs with specific partitioners.


val pairs = purchasesRdd.map(p => (p.customerId, p.price))

val tunedPartitioner = new RangePartitioner(8, pairs)
val partitioned = pairs.partitionBy(tunedPartitioner).persist()

// Creating a RangePartitioner requires:
//   1. Specifying the desired number of partitions.
//   2. Providing a Pair RDD with ordered keys. This RDD is sampled to create a suitable
//      set of sroted ranges.

// Important: the result of partitionBy should be persisted. 
// Otherwise, the partitioning is repeatedly applied (involving shuffling!) each
// time the partitioned RDD is used.



// --------------------------------------------------------

// Optimization using range partitioning

val purchasesPerCust = partitioned.map(p => (p._1, (1, p._2)))

val purchasesPerMonth = purchasesPerCust
        .reduceByKey((v1, v2) => (v1._1 + v2._1, v1._2 + v2._2))
        .collect() // faster than reduceByKey without partition


// partitionBy example

val sc = new SparkContext(...)
val userData = sc.sequenceFile[UserID, UserInfo]("hdfs://...").persist()

def processNewLogs(logFileName: String) {
    val events = sc.sequenceFile[UserID, LinkInfo](logFileName)
    val joined = userData.join(events) // RDD of (UserID, (UserInfo, LinkInfo))
    val offTopicVisits = joined.filter {
        case (userId, (userInfo, linkInfo)) => // Expand the tuple
            !userInfo.topics.contains(linkInfo.topic)
    }.count()
    println("Number of visits to non-subscribed topics: " + offTopicVisits)
}

// Is this OK? Very inefficient

val userData = sc.sequenceFile[UserId, UserInfo]("hdfs://...")
                 .partitionBy(new HashPartitioner(100)) // Create 100 partitions
                 .persist()

// partitionBy first will decrease the time a lot. Because we only a few partion will be 
// joined with events

// --------------------------------------------------------

// Q: How do I know a shuffle will occur?
// Rule of thumb:  a shuffle can occur when the resulting RDD depends on other elements from 
// the same RDD or another RDD
// Sometimes one can be clever and avoid much or all network communication while still using an 
// operation like join via smart partitioning

// How do I know a shuffle will occur?
// 1. return type:
org.apache.spark.rdd.RDD[(String, Int)] = ShuffledRDD[366]

// 2. Using function  toDebugString to see it's execution plan
partitioned.reduceByKey((v1, v2) => (v1._1 + v2._1, v1._2 + v2._2))
           .toDebugString

res9: String = MapPartitionsRDD[622] at reduceByKey at <console>: 49 []
   |  ShuffledRDD[615] at partitionBy at <console>: 48 []
   |    CachedPartitions: 8; MemorySize: 1754.8 MB; DiskSize: 0.0 B

// Examples that operations might cause a shuffle and to still avoid much or all network shuffling.
// 1. reduceByKey running on a pre-partitioned RDD
// 2. join called on two RDDs that are pre-partitioned


// --------------------------------------------------------
// Narrow Dependencies: No shuffle necessary. Optimizations like pipelining possible
// map, filter, union, join(with co-partitioned inputs)
// flatMap, mapPartitions, mapPartitionsWithIndex


// Wide Dependencies: Slow! Requires all or some data to be shuffled over the network
// groupByKey, join(with inputs not co-partitioned)
// leftOuterJoin, rightOuterJoin, reduceByKey

val wordRdd = sc.parallelize(largeList)
val pairs = wordsRdd.map(c => (c, 1))
                    .groupByKey()
                    .dependencies
// pairs: Seq[org.apache.spark.Dependency[_]] = 
// List(org.apache.spark.ShuffleDependency@4294a23d)



// indentations in the output separate groups of narrow transformations that maybe
// pipelined together with wide transformations that requires shuffles. These groupings are
// called stages.

val wordsRdd = sc.parallelize(largeList)
val paris = wordsRdd.map(c => (c, 1))
                    .groupByKey()
                    .toDebugString

// pairs: String = 
// (8) ShuffledRDD[219] at groupByKey at <console>: 38 []
// +-(8) MapPartitionsRDD[218] at map <console>: 37 []
//    |  ParallelCollectionRDD[217] at parallelize at <console>: 36 []


// --------------------------------------------------------
// Fault Tolerance
// Recomputing missing partitions fast for narrow dependencies. But slow for wide dependencies
