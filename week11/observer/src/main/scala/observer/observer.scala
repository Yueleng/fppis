package observer

trait Publisher {
  private var subscribers: Set[Subscriber] = Set()

  def addSubscribe(subscriber: Subscriber): Unit =
    subscribers += subscriber

  def delSubscribe(subscriber: Subscriber): Unit =
    subscribers -= subscriber

  def publish(): Unit =
    subscribers.foreach(_.handler(this))
}

trait Subscriber {
  def handler(pub: Publisher)
}

class BankAccount extends Publisher {
  private var balance = 0

  def currentBalance: Int = balance

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      balance = balance + amount
      publish()
    }

  def withdraw(amount: Int): Unit =
    if (amount > 0 && balance >= amount) {
      balance = balance - amount
      publish()
    } else throw new Error("insufficient funds")
}

class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed.foreach(_.addSubscribe(this))

  // private def total: Int = compute()
  private var total = 0

  private def compute(): Int = {
    // println("calculating sum")
    total = observed.map(_.currentBalance).sum
    total
  }

  def handler(pub: Publisher): Unit = compute()

  def totalBalance = total
}

object Main {

  def main(args: Array[String]): Unit = {
    println("Welcome to Demo!")

    val a = new BankAccount
    val b = new BankAccount
    val c = new Consolidator(List(a, b))

    println(c.totalBalance)
    a deposit 20
    println(a.currentBalance)
    println(c.totalBalance)
  }

}