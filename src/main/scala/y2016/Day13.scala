package y2016

import y2016.Dijkstra.{Distance, NodeGenerator, StepConstraint}

object Day13 extends App {

  type Position = (Int, Int)

  def surrounding(position: Position): Seq[Position] = List(
    (position._1 - 1, position._2),
    (position._1 + 1, position._2),
    (position._1, position._2 + 1),
    (position._1, position._2 - 1)
  ).filter { case (x, y) => x >= 0 && y >= 0 }

  def hamming(i: Int): Int = {
    i.toBinaryString.count { case '1' => true case _ => false }
  }

  def formula(position: Position, magicNumber: Int): Int = position match {
    case (x, y) => {
      val tmp = x * x + 3 * x + 2 * x * y + y + y * y + magicNumber
      hamming(tmp)
    }
  }

  case class Office(magicNumber: Int) {
    var neighbours: Map[Position, Seq[Position]] = Map()

    def neighboursOf(position: Position): Seq[Position] = {
      if (neighbours.get(position).isEmpty) {
        //        println("computing neighbours of " + position)
        val nodes = surrounding(position).filter(p => formula(p, magicNumber) % 2 == 0)
        neighbours = neighbours updated(position, nodes)
      }
      //      println("Which are " + neighbours(position))
      neighbours(position)
    }
  }

  def testHamming(n: Int) = {
    println(n.toBinaryString)
    println(hamming(n))
    println()
  }

  def solve1(start: Position, end: Position, office: Office): Int = {
    val nodeGen: NodeGenerator[Position] = p => office.neighboursOf(p)
    val distance: Distance[Position] = (_, _) => 1
    val result = Dijkstra.traverse(start, end, nodeGen, distance, List())

    val path = Dijkstra.findPath(end, result)

    val pathMap = path.foldLeft(Map[Int, Map[Int, Boolean]]()) {
      case (map, (x, y)) => {
        map updated(x, map.getOrElse(x, Map()).updated(y, true))
      }
    }

    val maxX: Int = path.map { case (x, _) => x }.max
    val maxY = path.map { case (_, y) => y }.max
    (0 to maxY + 2).foreach(y => {
      (0 to maxX + 2).foreach(x => {
          if((x, y) == start || (x, y) == end){
            print('*')
          } else if(pathMap.get(x).flatMap(x => x.get(y)).getOrElse(false)){
            print('0')
          }else if(formula((x, y), office.magicNumber) % 2 == 0){
            print('.')
          }else{
            print('#')
          }
      })
      println()
    })
    println()
    path.size
  }

  def solve2(start: Position, end: Position, maxSteps: Long, office: Office): Int = {
    val nodeGen: NodeGenerator[Position] = p => office.neighboursOf(p)
    val distance: Distance[Position] = (_, _) => 1
    val result = Dijkstra.traverse(start, end, nodeGen, distance, List(StepConstraint(maxSteps)))
    result.steps.toSeq.count { case (p: Position, steps: Long) => steps <= maxSteps }
  }


  override def main(args: Array[String]): Unit = {
//        println(solve1((1, 1), (7, 4), Office(10)))
    println(solve1((1, 1), (31, 39), Office(1364)))
//    println(solve2((1, 1), (31, 39), 50, Office(1364)))
  }
}
