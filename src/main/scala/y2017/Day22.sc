import scala.annotation.tailrec

type Position = (Int, Int)

sealed trait Direction {
  def turnRight(): Direction

  def turnLeft(): Direction

  def move(p: Position): Position
}

case class Up() extends Direction {
  override def turnRight() = Right()

  override def turnLeft() = Left()

  override def move(p: (Int, Int)) = (p._1 - 1, p._2)
}

case class Right() extends Direction {
  override def turnRight() = Down()

  override def turnLeft() = Up()

  override def move(p: (Int, Int)) = (p._1, p._2 + 1)
}

case class Down() extends Direction {
  override def turnRight() = Left()

  override def turnLeft() = Right()

  override def move(p: (Int, Int)) = (p._1 + 1, p._2)
}

case class Left() extends Direction {
  override def turnRight() = Up()

  override def turnLeft() = Down()

  override def move(p: (Int, Int)) = (p._1, p._2 - 1)
}

sealed trait Infection {

}

case class Infected() extends Infection

case class Clean() extends Infection

case class Weakened() extends Infection

case class Flagged() extends Infection

type Grid = Map[Int, Map[Int, Infection]]

//def toStr(grid: Grid) = "\n" + grid.toSeq.sortBy(_._1).map {
//  _._2.toSeq.sortBy(_._1).map{
//    case Infected() => '#'
//      cae _ =>
//  }.map("\t" + _).reduceLeft(_ + _) + "\n"
//}.reduceLeft(_ + _)

def toStr(state: State) = {
  val grid = state.grid
  val keys = grid.values.flatMap(_.keys)
  val min = math.min(keys.min, grid.keys.min)
  val max = math.max(keys.max, grid.keys.max)
  for (i <- min to max) {
    for (j <- min to max) {
      if (state.p == (i, j)) {
        print('[')
      } else {
        print(' ')
      }
      print(grid.get(i).flatMap(_.get(j)).map {
        case Infected() => '#'
        case Weakened() => 'W'
        case Flagged() => 'F'
        case _ => '.'
      }.getOrElse('.'))
      if (state.p == (i, j)) {
        print(']')
      } else {
        print(' ')
      }
    }
    println()
  }
  println()
}

def addOrUpdate[K, V](m: Map[K, V], k: K, kv: (K, V))(f: V => V): Map[K, V] = {
  m.get(k) match {
    case Some(e) => m + (k -> f(e))
    case None => m + kv
  }
}

val test = "..#\n#..\n..."

def parseGrid(s: String): Grid = s.split("\n").zipWithIndex.foldLeft(Map[Int, Map[Int, Infection]]()) {
  (map, row) => {
    map + (row._2 -> row._1.toCharArray.zipWithIndex.map { case (c, idx) => {
      c match {
        case '#' => (idx, Infected())
        case '.' => (idx, Clean())
      }
    }
    }.toMap)
  }
}

//toStr(parseGrid(test))

case class State(p: Position, direction: Direction, grid: Grid, infections: Int) {
  def next(): State = {
    val atPosition = grid.get(p._1).flatMap(row => row.get(p._2))
    val work = atPosition match {
      case Some(Weakened()) => (direction, Infected(), 1)
      case Some(Infected()) => (direction.turnRight(), Flagged(), 0)
      case Some(Flagged()) => (direction.turnRight().turnRight(), Clean(), 0)
      case _ => (direction.turnLeft(), Weakened(), 0)
    }
    State(work._1.move(p), work._1, addOrUpdate(grid, p._1, p._1 -> Map[Int, Infection](p._2 -> work._2)) {
      row => row + (p._2 -> work._2)
    }, infections + work._3)
  }
}


@tailrec
def solve(state: State, count: Int): State = if (count == 0) state else {
  //  toStr(state)
  solve(state.next(), count - 1)
}

val s = solve(State((1, 1), Up(), parseGrid(test), 0), 10000000)
//toStr(s)
s.infections

val cesar = parseGrid("##.###.....##..#.####....\n##...#.#.#..##.#....#.#..\n...#..#.###.#.###.##.####\n..##..###....#.##.#..##.#\n###....#####..###.#..#..#\n.....#.#...#..##..#.##...\n.##.#.###.#.#...##.#.##.#\n......######.###......###\n#.....##.#....#...#......\n....#..###.#.#.####.##.#.\n.#.#.##...###.######.####\n####......#...#...#..#.#.\n###.##.##..##....#..##.#.\n..#.###.##..#...#######..\n...####.#...###..#..###.#\n..#.#.......#.####.#.....\n..##..####.######..##.###\n..#..#..##...#.####....#.\n.#..#.####.#..##..#..##..\n......#####...#.##.#....#\n###..#...#.#...#.#..#.#.#\n.#.###.#....##..######.##\n##.######.....##.#.#.#..#\n..#..##.##..#.#..###.##..\n#.##.##..##.#.###.......#")
//
//3/2
//cesar.size/2
//
val s2 = solve(State((12, 12), Up(), cesar, 0), 10000000)

s2.infections






