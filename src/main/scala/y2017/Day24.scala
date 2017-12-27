package y2017

object Day24 extends App{

  case class Component(start: Int, end: Int) {
    def flip() = Component(end, start)

    def strength = start + end
  }

  type Bridge = Seq[Component]

  def buildAll(available: Set[Component], current: Bridge): Seq[Bridge] = {
    val end = current.head.end
    available.filter(c => c.start == end || c.end == end).toSeq.flatMap { c => {
      if (c.start == end) {
        val newBridge: Bridge = c +: current
        val foo: Seq[Bridge] = buildAll(available - c, newBridge) :+ newBridge
        foo
      } else {
        val newBridge: Bridge = c.flip() +: current
        buildAll(available - c, newBridge) :+ newBridge
      }
    }
    }
  }

  def parse(s: String): Component = {
    val strings = s.split("/")
    Component(strings(0).toInt, strings(1).toInt)
  }

  def strength(bridge: Bridge): Int = bridge.foldRight(0) {
    (c, s) => s + c.strength
  }

  override def main(args: Array[String]): Unit = {
    val top = Component(0, 0)

    val cesar = "50/41\n19/43\n17/50\n32/32\n22/44\n9/39\n49/49\n50/39\n49/10\n37/28\n33/44\n14/14\n14/40\n8/40\n10/25\n38/26\n23/6\n4/16\n49/25\n6/39\n0/50\n19/36\n37/37\n42/26\n17/0\n24/4\n0/36\n6/9\n41/3\n13/3\n49/21\n19/34\n16/46\n22/33\n11/6\n22/26\n16/40\n27/21\n31/46\n13/2\n24/7\n37/45\n49/2\n32/11\n3/10\n32/49\n36/21\n47/47\n43/43\n27/19\n14/22\n13/43\n29/0\n33/36\n2/6".split("\n").toSet.map(parse)

    val r2 = buildAll(cesar, List(top))

    val maxLength = r2.map(_.length).max

    println(maxLength)

    println(r2.filter(b => b.length == maxLength).map(strength).max)


  }
}
