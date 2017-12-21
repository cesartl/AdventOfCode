package y2017

import scala.annotation.tailrec

object Day13Bis extends App{

  case class Layer(firewall: Int, depth: Int, down: Boolean) {
    def move() = {
      val newFirewall: Int = if (down) firewall + 1 else firewall - 1
      if (newFirewall == 0) {
        Layer(newFirewall, depth, true)
      } else if (newFirewall == depth - 1) {
        Layer(newFirewall, depth, false)
      } else {
        Layer(newFirewall, depth, down)
      }
    }
  }

  case class State(position: Int, layers: Map[Int, Layer]) {
    def moveScaner(): State = State(position, layers.mapValues(_.move()))

    def moveState(): State = State(position + 1, layers)

    def caught() = layers.get(position).exists { case Layer(firewall, _, _) => firewall == 0 }

    def length(): Int = layers.keys.max

    def currentLayer: Layer = layers.get(position).orNull

    def delay(n: Int) : State = {
      if(n == 0) this
      else this.moveScaner().delay(n - 1)
    }
  }


  def parseOne(s: String) = {
    val a = s.split(":").map(_.trim)
    (a(0).toInt, Layer(0, a(1).toInt, true))
  }

  def parse(s: String) = State(-1, s.split("\n").map(parseOne).foldLeft(Map[Int, Layer]())((m, p) => {
    m.updated(p._1, p._2)
  }))
  //

  def advance(state: State, caught: Map[Int, Layer]): (State, Map[Int, Layer]) = {
    //  println(state)
    val x = state.moveState()
    if (x.caught()) advance(x.moveScaner(), caught.updated(x.position, x.currentLayer))
    else if (x.position >= x.length()) (x, caught)
    else advance(x.moveScaner(), caught)
  }

//  val foo = advance(state, Map[Int, Layer]())


  def score(caught: Map[Int, Layer]) = {
    caught.foldLeft(0)((s, v) => s + v._1 * v._2.depth)
  }


//  score(foo._2)

//  advance(parse(cesar).delay(71), Map[Int, Layer]())._2

  //score(advance(parse(cesar).delay(71), Map[Int, Layer]())._2)


  @tailrec
  def doNotGetCaught(state: State, delay: Int) : Int = {
    println(delay)
    val delayed = state.moveScaner()
    if(advance(delayed, Map[Int, Layer]())._2.isEmpty) delay
    else doNotGetCaught(delayed, delay +1)
  }

  override def main(args: Array[String]): Unit = {
    val cesar = "0: 4\n1: 2\n2: 3\n4: 5\n6: 8\n8: 6\n10: 4\n12: 6\n14: 6\n16: 8\n18: 8\n20: 6\n22: 8\n24: 8\n26: 8\n28: 12\n30: 12\n32: 9\n34: 14\n36: 12\n38: 12\n40: 12\n42: 12\n44: 10\n46: 12\n48: 12\n50: 10\n52: 14\n56: 12\n58: 14\n62: 14\n64: 14\n66: 12\n68: 14\n70: 14\n72: 17\n74: 14\n76: 14\n80: 20\n82: 14\n90: 24\n92: 14\n98: 14"
    val result = doNotGetCaught(parse(cesar), 2697)
    println(result)
  }
}
