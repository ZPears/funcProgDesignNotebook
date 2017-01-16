// Digital Circuit has:
// WIRES - transport signals to components

// COMPONENTS - which are:
// INVERTER, inverts input
// AND GATE, output is conjunction of inputs
// OR GATE, output is disjunction of inputs

// Components have REACTION TIME (delay)

case class Wire(var last: Component, var next: Option[Component] = None, var charge: Boolean) {
  def connect(component: Component): Unit = {
    require(!next.isDefined, "Wire is already connected!")
    this.next = Some(component)
  }
  override def toString: String = s"WIRE: from ${last.toString} to ${next match {case Some(x) => x.toString case None => "NOT CONNECTED"}}. CHARGED: $charge"
}

trait Component {
  var input: Seq[Wire]
  var output: Seq[Wire] = List()
  def action: Boolean
  var state: Boolean = false
  def updateOutputs: Unit = {
    output.foreach(w => w.charge == state)
    output.foreach(w => w.next match {
      case Some(comp) => if (comp.output.nonEmpty) comp.updateOutputs
      case None =>
    })
  }
  def addInput(other: Component): Unit = {
    val newWire = new Wire(other, Some(this), other.action)
    this.input = this.input :+ newWire
    other.output = other.output :+ newWire
    this.action
    updateOutputs
  }
  def addOutput(other: Component): Unit = {
    val newWire = new Wire(this, Some(other), this.action)
    this.output = this.output :+ newWire
    other.input = other.input :+ newWire
    this.action
    updateOutputs
  }
  def addEmptyOutput: Unit = { output = output :+ new Wire(this, None, this.state) }
  override def toString: String = this.getClass.getSimpleName
}

case class PowerSource(var input: Seq[Wire] = List()) extends Component {
  def action = {
    this.state = true
    updateOutputs
    true
  }
}

case class Inverter(var input: Seq[Wire]) extends Component {
  require(input.length == 1, "Only 1 input allowed for inverter")
  input.foreach(w => w.last.output :+ w)
  input.foreach(w => w.next = Some(this))
  action
  def action = {
    this.state = !input.head.charge
    updateOutputs
    this.state
  }
}
case class AndGate(var input: Seq[Wire]) extends Component {
  input.foreach(w => w.last.addOutput(this))
  input.foreach(w => w.last.output :+ w)
  input.foreach(w => w.next = Some(this))
  def action = {
    this.state = input.forall(_.charge)
    updateOutputs
    this.state
  }
}

case class OrGate(var input: Seq[Wire]) extends Component {
  input.foreach(w => w.last.output :+ w)
  input.foreach(w => w.next = Some(this))
  def action = {
    this.state = input.exists(_.charge)
    updateOutputs
    this.state
  }
}
case class Guage(var input: Seq[Wire]) extends Component {
  input.foreach(w => w.last.output :+ w)
  input.foreach(w => w.next = Some(this))
  def totalCurrent = input.count(_.charge)
  def action = {
    this.state = input.exists(_.charge)
    updateOutputs
    this.state
  }
}

val psu = PowerSource()
psu.addEmptyOutput
psu.output
val inverter = Inverter(Seq(psu.output.head))
inverter.addEmptyOutput
val inverter2 = Inverter(Seq(inverter.output.head))
inverter2.addEmptyOutput
inverter2.addEmptyOutput
val g = Guage(Seq(inverter2.output.head))
g.addEmptyOutput

g.totalCurrent

g.addInput(psu)

g.totalCurrent

val gate = AndGate(Seq(inverter2.output(1), g.output.head))
gate.input.foreach(println)
gate.state