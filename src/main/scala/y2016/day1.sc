import java.util.regex.Pattern

import scala.annotation.tailrec

sealed trait Rotate {
  def rotate(orientation: Orientation): Orientation
}

type Coordinate = (Int, Int)

sealed trait Orientation {}

case class East() extends Orientation

case class West() extends Orientation

case class South() extends Orientation

case class North() extends Orientation

case class RotateRight() extends Rotate {
  override def rotate(orientation: Orientation) = orientation match {
    case East() => South()
    case South() => West()
    case West() => North()
    case North() => East()
  }
}

case class RotateLeft() extends Rotate {
  override def rotate(orientation: Orientation) = orientation match {
    case West() => South()
    case South() => East()
    case East() => North()
    case North() => West()
  }
}


case class Instruction(rotate: Rotate, n: Int)

case class Position(coordinate: Coordinate, orientation: Orientation) {
  def move(instruction: Instruction): Position = {
    val newOrientation = instruction.rotate.rotate(orientation)
    val newCoordinate = advance(coordinate, newOrientation, instruction.n)
    Position(newCoordinate, newOrientation)
  }

  def move(instruction: Instruction, visited: Set[Coordinate]): Either[Coordinate, (Position, Set[Coordinate])] = {
    val newOrientation = instruction.rotate.rotate(orientation)
    advanceVisit(coordinate, newOrientation, instruction.n, visited).map {
      case (c, set) => (Position(c, newOrientation), set)
    }
  }
}


def advance(coordinate: Coordinate, orientation: Orientation, n: Int): Coordinate = orientation match {
  case East() => (coordinate._1 + n, coordinate._2)
  case West() => (coordinate._1 - n, coordinate._2)
  case North() => (coordinate._1, coordinate._2 - n)
  case South() => (coordinate._1, coordinate._2 + n)
}

@tailrec
def advanceVisit(coordinate: Coordinate, orientation: Orientation, n: Int, visited: Set[Coordinate]): Either[Coordinate, (Coordinate, Set[Coordinate])] = {
  if (n == 0) Right((coordinate, visited)) else {
    val newCoordinate = orientation match {
      case East() => (coordinate._1 + 1, coordinate._2)
      case West() => (coordinate._1 - 1, coordinate._2)
      case North() => (coordinate._1, coordinate._2 - 1)
      case South() => (coordinate._1, coordinate._2 + 1)
    }
    if (visited.contains(newCoordinate)) Left(newCoordinate) else
      advanceVisit(newCoordinate, orientation, n - 1, visited + newCoordinate)
  }
}

val instructionPattern = Pattern.compile("([RL])([\\d]+)")

def parseInstruction(s: String): Instruction = {
  val m = instructionPattern.matcher(s)
  if (m.matches()) {
    if (m.group(1).equals("R")) Instruction(RotateRight(), m.group(2).toInt) else Instruction(RotateLeft(), m.group(2).toInt)
  } else {
    throw new IllegalArgumentException(s)
  }
}

def parse(s: String): Seq[Instruction] = s.split(",").map(_.trim).map(parseInstruction).toList


def distance(coordinate: Coordinate): Int = math.abs(coordinate._1) + math.abs(coordinate._2)

def solve1(position: Position, instructions: Seq[Instruction]): Int = {
  val p = instructions.foldLeft(position) {
    (p, i) => {
      p.move(i)
    }
  }
  distance(p.coordinate)
}

@tailrec
def solve2(position: Position, instructions: Seq[Instruction], visited: Set[Coordinate]): Coordinate = instructions match {
  case instruction :: t => {
    position.move(instruction, visited) match {
      case Right((p, set)) => solve2(p, t, set)
      case Left(p) => p
    }
  }
  case _ => {
    position.coordinate
  }
}

val start = Position((0, 0), North())
val t1 = "R5, L5, R5, R3"

parseInstruction("R5")

start.move(parseInstruction("R5"))

solve1(start, parse(t1))

val cesar = "L2, L5, L5, R5, L2, L4, R1, R1, L4, R2, R1, L1, L4, R1, L4, L4, R5, R3, R1, L1, R1, L5, L1, R5, L4, R2, L5, L3, L3, R3, L3, R4, R4, L2, L5, R1, R2, L2, L1, R3, R4, L193, R3, L5, R45, L1, R4, R79, L5, L5, R5, R1, L4, R3, R3, L4, R185, L5, L3, L1, R5, L2, R1, R3, R2, L3, L4, L2, R2, L3, L2, L2, L3, L5, R3, R4, L5, R1, R2, L2, R4, R3, L4, L3, L1, R3, R2, R1, R1, L3, R4, L5, R2, R1, R3, L3, L2, L2, R2, R1, R2, R3, L3, L3, R4, L4, R4, R4, R4, L3, L1, L2, R5, R2, R2, R2, L4, L3, L4, R4, L5, L4, R2, L4, L4, R4, R1, R5, L2, L4, L5, L3, L2, L4, L4, R3, L3, L4, R1, L2, R3, L2, R1, R2, R5, L4, L2, L1, L3, R2, R3, L2, L1, L5, L2, L1, R4"
solve1(start, parse(cesar))
solve2(start, parse(cesar), Set())
distance(solve2(start, parse(cesar), Set()))
