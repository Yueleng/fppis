// Reduction Operations

// example: foldLeft 
case class Taco(kind: String, price: Double)

val tacoOrder = List(
    Taco("Carnitas", 2.25),
    Taco("Corn", 1.75),
    Taco("Barbacoa", 2.50),
    Taco("Chicken", 2.00)
)

val cost = tacoOrder.foldLeft(0.0)((sum, taco) => sum + taco.price)

// foldLeft is not parallelizable
def foldLeft[B](z: B)(f: (B, A) => B): B

// example: foldLeft
val xs = List(1, 2, 3, 4)
val res = xs.foldLeft("")((str: String, i: Int) => str + i)

// fold is parallelizable
def fold(z: A)(f: (A, A) => A): A

// aggregate is parallelizable and more general
def aggregate[B](z: => B)(seqop: (B, A) => B, combop: (B, B) => B): B

// wiki
case class WikipediaPage(
    title: String, 
    redirectTitle: String,
    timestamp: String,
    lastContributorUsername: String,
    text: String
)

// RDD[(K, V)]: pair RDD



def join[W](other: RDD[(K, W)]): RDD[K, (V, W)]

// transfer from RDD to pair RDD
val rdd: RDD[WikipediaPage] = ...
pairRdd = rdd.map(page => (page.title, page.text))


// groupBy for scala collections
def groupBy[K](f: A => K): Map[K, Traversable[A]]

// pair RDD counterpart for groupBy
def groupByKey(): RDD[(K, Iterable[V])]

// example: groupByKey
case class Event(organizer: String, name: String, budget: Int)

val eventsRdd = sc.parallelize(...)
               .map(event => (event.organizer, event.budget))

val groupedRdd = eventsRdd.groupByKey()

groupedRdd.collect().foreach(println)
// (Prime Sound, CompactBuffer(42000))
// (Sportorg, CompactBuffer(23000, 12000, 1400))

// reduceByKey
def reduceByKey(func: (V, V) => V): RDD[(K, V)]

val budgetsRdd = eventsRdd.reduceByKey(_+_)

reducedRdd.collect().foreach(println)
// (Prime Sound, 42000)
// (Sportorg, 36400)
// (Innotech, 320000)
// (Association Balelec, 50000)


// mapValues
def mapValues[U](f: V => U): RDD[(K, U)] = 
    rdd.map {case (x, y): (x, func(y))}


// usage of mapValues
val intermediate = 
  eventsRdd.mapValues(b => (b, 1))
           .reduceByKey((v1, v2) => (v1._1 + v2._2, v1._2 + v2._2))

val avgBudgets = intermediate.mapValues {
    case (budget, numberOfEvents) => budget / numberOfEvents
}

avgBudgets.collect().foreach(println)

// Keys
def keys: RDD[K]

// usage of keys
case class Visitor(ip: String, timestamp: String, duration: String)
val visits: RDD[Visitor] = sc.textfile(...)
val numUniqueVisits = visits.keys.distinct().count()


// JOIN
val as = List(
    (101, ("Ruetli", "AG")), 
    (102, ("Brelaz", "DemiTarif")),
    (103, ("Gress", "DemiTarifVisa")), 
    (104, ("Schatten", "DemiTarif"))
)
val abos = sc.parallelize(as)

val ls = List(
    (101, "Bern"), 
    (101, "Thun"), 
    (102, "Lausanne"), 
    (102, "Geneve"), 
    (102, "Nyon"), 
    (103, "Zurich"), 
    (103, "St-Gallen"), 
    (103, "Chur")
)

val locations = sc.parallelize(ls)

// join
def join[W](other: RDD[(K, W)]): RDD[(K, (V, W))]

// inner join
val trackedCustomers = abos.join(locations)
trackedCustomers.collect().foreach(println)
// (101, ((Ruetli, AG), Bern))
// (101, ((Ruetli, AG), Thun))
// (102, ((Brelaz, DemiTarif), Nyon))
// (102, ((Brelaz, DemiTarif), Lausanne))
// (102, ((Brelaz, DemiTarif), Geneve))
// (103, ((Gress, DemiTarifVisa), St-Gallen))
// (103, ((Gress, DemiTarifVisa), Chur))
// (103, ((Gress, DemiTarifVisa), Zurich))

// leftjoin
def leftOuterJoin[W](other: RDD[(K, W)]): RDD[(K, (V, Option[W]))]
def rightOuterJoin[W](other: RDD[(K, W)]): RDD[(K, (Option[V], W))]

// abosWithOptionalLocations: RDD[(Int, ((String, Abonnement), Option[String]))]
val abosWithOptionalLocations = abos.leftOuterJoin(locations)
// (101,((Ruetli,AG),Some(Thun)))
// (101,((Ruetli,AG),Some(Bern)))
// (102,((Brelaz,DemiTarif),Some(Geneve)))
// (102,((Brelaz,DemiTarif),Some(Nyon)))
// (102,((Brelaz,DemiTarif),Some(Lausanne)))
// (103,((Gress,DemiTarifVisa),Some(Zurich)))
// (103,((Gress,DemiTarifVisa),Some(St-Gallen)))
// (103,((Gress,DemiTarifVisa),Some(Chur)))
// (104,((Schatten,DemiTarif),None)) 

val customersWithLocationDataAndOptionalAbos = abos.rightOuterJoin(locations)

