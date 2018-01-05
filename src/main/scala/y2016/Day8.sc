type Line = Vector[Boolean]
type Screen = Vector[Line]

sealed trait Instruction {}

case class Rect(a: Int, b: Int) extends Instruction

case class RotateRow(y: Int, n: Int) extends Instruction

case class RotateColumn(x: Int, n: Int) extends Instruction

val rectR = """rect ([\d]+)x([\d]+)"""
val rowR = """rotate row y=([\d]+) by ([\d]+)"""
val colR = """rotate column x=([\d]+) by ([\d]+)"""

def parse(s: String) : Instruction = {
  val rectMatch = rectR.r.findFirstMatchIn(s)
  val rowMatch = rowR.r.findFirstMatchIn(s)
  val colMatch = colR.r.findFirstMatchIn(s)
  if(rectMatch.isDefined){
    Rect(rectMatch.get.group(1).toInt, rectMatch.get.group(2).toInt)
  }else if(rowMatch.isDefined){
    RotateRow(rowMatch.get.group(1).toInt, rowMatch.get.group(2).toInt)
  }else if (colMatch.isDefined) {
    RotateColumn(colMatch.get.group(1).toInt, colMatch.get.group(2).toInt)
  } else {
    throw new IllegalArgumentException(s)
  }
}

def printScreen(screen: Screen) = {
  screen.foreach(row => {
    row.foreach(v => if(v) print("#") else print("."))
    println()
  })
  println()
}

def rotate[T](line: Vector[T], n: Int): Vector[T] = {
  line.zipWithIndex.map { case (v, i) => (v, (i + n) % line.length) }.sortBy(_._2).map(_._1)
}

rotate(Vector(1, 2, 3, 4), 4)

def rotateScreen(screen: Screen, at: Int, n: Int): Screen =
  screen.zipWithIndex.map { case (line, idx) =>
    if (idx == at) rotate(line, n) else line
  }

def apply(instruction: Instruction, screen: Screen): Screen = instruction match {
  case Rect(a, b) => {
    screen.zipWithIndex.map {
      case (line, y) => line.zipWithIndex.map {
        case (v, x) => if (x < a && y < b) true else v
      }
    }
  }
  case RotateRow(y, n) => rotateScreen(screen, y, n)

  case RotateColumn(x, n) => rotateScreen(screen.transpose, x, n).transpose
}


def seed(width: Int, height: Int) : Screen = {
  (0 until height).map(_ => (0 until width).map(_ => false).toVector).toVector
}

val test = seed(7, 3)

def applyInstructions(screen: Screen, instructions: Seq[Instruction]) : Screen = instructions.foldLeft(screen){
  (s, inst) => {
//    printScreen(s)
    apply(inst, s)
  }
}

printScreen(applyInstructions(test, List(Rect(3, 2), RotateColumn(1, 1), RotateRow(0, 4), RotateColumn(1, 1))))

val cesar = "rect 1x1\nrotate row y=0 by 5\nrect 1x1\nrotate row y=0 by 6\nrect 1x1\nrotate row y=0 by 5\nrect 1x1\nrotate row y=0 by 2\nrect 1x1\nrotate row y=0 by 5\nrect 2x1\nrotate row y=0 by 2\nrect 1x1\nrotate row y=0 by 4\nrect 1x1\nrotate row y=0 by 3\nrect 2x1\nrotate row y=0 by 7\nrect 3x1\nrotate row y=0 by 3\nrect 1x1\nrotate row y=0 by 3\nrect 1x2\nrotate row y=1 by 13\nrotate column x=0 by 1\nrect 2x1\nrotate row y=0 by 5\nrotate column x=0 by 1\nrect 3x1\nrotate row y=0 by 18\nrotate column x=13 by 1\nrotate column x=7 by 2\nrotate column x=2 by 3\nrotate column x=0 by 1\nrect 17x1\nrotate row y=3 by 13\nrotate row y=1 by 37\nrotate row y=0 by 11\nrotate column x=7 by 1\nrotate column x=6 by 1\nrotate column x=4 by 1\nrotate column x=0 by 1\nrect 10x1\nrotate row y=2 by 37\nrotate column x=19 by 2\nrotate column x=9 by 2\nrotate row y=3 by 5\nrotate row y=2 by 1\nrotate row y=1 by 4\nrotate row y=0 by 4\nrect 1x4\nrotate column x=25 by 3\nrotate row y=3 by 5\nrotate row y=2 by 2\nrotate row y=1 by 1\nrotate row y=0 by 1\nrect 1x5\nrotate row y=2 by 10\nrotate column x=39 by 1\nrotate column x=35 by 1\nrotate column x=29 by 1\nrotate column x=19 by 1\nrotate column x=7 by 2\nrotate row y=4 by 22\nrotate row y=3 by 5\nrotate row y=1 by 21\nrotate row y=0 by 10\nrotate column x=2 by 2\nrotate column x=0 by 2\nrect 4x2\nrotate column x=46 by 2\nrotate column x=44 by 2\nrotate column x=42 by 1\nrotate column x=41 by 1\nrotate column x=40 by 2\nrotate column x=38 by 2\nrotate column x=37 by 3\nrotate column x=35 by 1\nrotate column x=33 by 2\nrotate column x=32 by 1\nrotate column x=31 by 2\nrotate column x=30 by 1\nrotate column x=28 by 1\nrotate column x=27 by 3\nrotate column x=26 by 1\nrotate column x=23 by 2\nrotate column x=22 by 1\nrotate column x=21 by 1\nrotate column x=20 by 1\nrotate column x=19 by 1\nrotate column x=18 by 2\nrotate column x=16 by 2\nrotate column x=15 by 1\nrotate column x=13 by 1\nrotate column x=12 by 1\nrotate column x=11 by 1\nrotate column x=10 by 1\nrotate column x=7 by 1\nrotate column x=6 by 1\nrotate column x=5 by 1\nrotate column x=3 by 2\nrotate column x=2 by 1\nrotate column x=1 by 1\nrotate column x=0 by 1\nrect 49x1\nrotate row y=2 by 34\nrotate column x=44 by 1\nrotate column x=40 by 2\nrotate column x=39 by 1\nrotate column x=35 by 4\nrotate column x=34 by 1\nrotate column x=30 by 4\nrotate column x=29 by 1\nrotate column x=24 by 1\nrotate column x=15 by 4\nrotate column x=14 by 1\nrotate column x=13 by 3\nrotate column x=10 by 4\nrotate column x=9 by 1\nrotate column x=5 by 4\nrotate column x=4 by 3\nrotate row y=5 by 20\nrotate row y=4 by 20\nrotate row y=3 by 48\nrotate row y=2 by 20\nrotate row y=1 by 41\nrotate column x=47 by 5\nrotate column x=46 by 5\nrotate column x=45 by 4\nrotate column x=43 by 5\nrotate column x=41 by 5\nrotate column x=33 by 1\nrotate column x=32 by 3\nrotate column x=23 by 5\nrotate column x=22 by 1\nrotate column x=21 by 2\nrotate column x=18 by 2\nrotate column x=17 by 3\nrotate column x=16 by 2\nrotate column x=13 by 5\nrotate column x=12 by 5\nrotate column x=11 by 5\nrotate column x=3 by 5\nrotate column x=2 by 5\nrotate column x=1 by 5"

val instructions = cesar.split("\n").map(parse)

val s1 = applyInstructions(seed(50, 6), instructions)

printScreen(s1)

s1.map(line => line.count(x => x)).sum

s1.transpose.grouped(5).map(_.transpose).foreach(printScreen)