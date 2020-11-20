package simulator

abstract class Circuit extends Gates {
  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
    val d, e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
    val s, c1, c2 = new Wire
    halfAdder(b, cin, s, c1)
    halfAdder(a, s, sum, c2)
    orGate(c1, c2, cout)
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    println("Welcome to the Simulator")
    object sim extends Circuit with Parameters {
      // ???
      override def InverterDalay: Int = 2
      override def AndGateDelay: Int = 3
      override def OrGateDelay: Int = 5
    }
    import sim._
    val in1, in2, sum, carry = new Wire

    halfAdder(in1, in2, sum, carry)
    probe("sum", sum)
    probe("carry", carry)
    in1 setSignal true
    run()

    in2 setSignal true
    run()

    in1 setSignal false
    run()
  }
}


