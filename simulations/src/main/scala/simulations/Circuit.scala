package simulations

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig)}
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val c, d, e = new Wire
    inverter(a1, c)
    inverter(a2, d)
    andGate(c, d, e)
    inverter(e, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    c match {
      case Nil => andGate(in, in, out(0))
      /*
      case h :: Nil => {
        val notC = new Wire
        val d0 = out(0)
        val d1 = out(1)
        inverter(h, notC)
        andGate(in, notC, d0)
        andGate(in, h, d1)
      }
      */
      case h :: t => {
        val notC, a0, a1 = new Wire
        inverter(h, notC)
        andGate(in, notC, a0)
        andGate(in, h, a1)
        val (out0, out1) = out.splitAt(out.length / 2)
        demux(a0, t, out0)
        demux(a1, t, out1)
      }
    }
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def orGateExample {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run

    in1.setSignal(false)
    run
  }

  def orGate2Example {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run

    in1.setSignal(false)
    run
  }

  def demuxZeroExample {
    val in, out = new Wire
    demux(in, List(), List(out))

    in.setSignal(false)
    run

    in.setSignal(true)
    run
  }

  def demuxOneExample {
    val in, c, out1, out2 = new Wire
    demux(in, List(c), List(out1, out2))

    in.setSignal(false)
    c.setSignal(false)
    run

    in.setSignal(false)
    c.setSignal(true)
    run

    in.setSignal(true)
    c.setSignal(false)
    run

    in.setSignal(true)
    c.setSignal(true)
    run
  }
}

object CircuitMain extends App {
  Circuit.andGateExample
  Circuit.orGateExample
  Circuit.orGate2Example
  Circuit.demuxZeroExample
  Circuit.demuxOneExample
}
