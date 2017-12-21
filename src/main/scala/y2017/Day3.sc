
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class Position(x: Int, y: Int) {
  def right: Position = Position(x + 1, y)

  def up: Position = Position(x, y + 1)

  def left: Position = Position(x - 1, y)

  def down: Position = Position(x, y - 1)
}

def addOrUpdate[K, V](m: mutable.Map[K, V], k: K, kv: (K, V))(f: V => V) = {
  m.get(k) match {
    case Some(e) => m.update(k, f(e))
    case None => m += kv
  }
}

def put(map: mutable.Map[Int, mutable.Map[Int, Int]], position: Position, value: Int) = {
  val f = (m: mutable.Map[Int, Int]) => m.updated(position.y, value)
  val tuple: (Int, mutable.Map[Int, Int]) = position.x -> mutable.Map(position.y -> value)
  addOrUpdate(map, position.x, tuple)(f)
}

def value(map: mutable.Map[Int, mutable.Map[Int, Int]], position: Position) = map.get(position.x).flatMap(m => m.get(position.y)).getOrElse(0)

def neighborsValue(map: mutable.Map[Int, mutable.Map[Int, Int]], position: Position) = {
  value(map, position.up) +
    value(map, position.down) +
    value(map, position.left) +
    value(map, position.right) +
    value(map, position.up.right) +
    value(map, position.up.left) +
    value(map, position.down.right) +
    value(map, position.down.left)
}

def doStep(table: mutable.Map[Int, mutable.Map[Int, Int]], depth: Int, state: (Position, Int, ArrayBuffer[Int]), max: Int, f: Position => Position) = {
  var positionBuffer = state._1
  var valueBuffer = state._2
  var i = 0
  while (i < depth && valueBuffer < max) {
    i += 1
    positionBuffer = f(positionBuffer)
    valueBuffer = neighborsValue(table, positionBuffer)
    state._3 += valueBuffer
    put(table, positionBuffer, valueBuffer)
  }
  (positionBuffer, valueBuffer, state._3)
}


def buildStructure(max: Int) = {
  val table: mutable.Map[Int, mutable.Map[Int, Int]] = mutable.Map()
  var depth = 1
  table.put(0, mutable.Map(0 -> 1))
  var current: (Position, Int, ArrayBuffer[Int]) = (Position(0, 0), 1, ArrayBuffer[Int]())
  //  var position = Position(0, 0)
  //  var value = 1;

  while (current._2 < max) {
    current = doStep(table, depth, current, max, p => p.right)
    current = doStep(table, depth, current, max, p => p.up)
    depth += 1
    current = doStep(table, depth, current, max, p => p.left)
    current = doStep(table, depth, current, max, p => p.down)
    depth += 1
  }
  current._3
}

buildStructure(277678+1)


