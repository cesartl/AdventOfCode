package y2016

import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec
import scala.collection.mutable

object Day11 extends App {

  sealed trait Element {
    def name: String

    def id: Int
  }

  case class Microchip(name: String, id: Int) extends Element

  case class Generator(name: String, id: Int) extends Element

  type Floor = Set[Element]

  case class State(floors: Seq[Floor], elevator: Int)

  type WinningCondition = State => Boolean

  sealed trait ElevatorLoad {
    def ids: Set[Int] = elements.map(_.id)

    def elements: Set[Element]
  }

  case class SingleLoad(element: Element) extends ElevatorLoad {
    override def elements: Set[Element] = Set(element)
  }

  case class DoubleLoad(e1: Element, e2: Element) extends ElevatorLoad {
    override def elements: Set[Element] = Set(e1, e2)
  }

  def doLoad(state: State, elevatorLoad: ElevatorLoad, to: Int): State = {
    val newFloors = state.floors.zipWithIndex.map {
      case (floor, idx) if idx == state.elevator => floor.filter(e => !elevatorLoad.ids.contains(e.id))
      case (floor, idx) if idx == to => floor ++ elevatorLoad.elements
      case (floor, _) => floor
    }
    State(newFloors, to)
  }

  def isFloorValid(floor: Floor): Boolean = {
    //if there is no generator or no chip floor is valid, otherwise must ensure all chip are connected
    if (floor.count { case Generator(_, _) => true case _ => false } == 0) true
    else if (floor.count { case Microchip(_, _) => true case _ => false } == 0) true
    else {
      floor.groupBy(_.name).forall {
        case (_, group) => group.count { case Microchip(_, _) => true case _ => false } <= group.count { case Generator(_, _) => true case _ => false }
      }
    }
  }

  def isValid(state: State): Boolean = {
    state.floors.forall(isFloorValid) && state.floors.map(f => f.nonEmpty).count(p => p) <= 3
  }

  //  def isElevatorLoadValid(elevatorLoad: ElevatorLoad): Boolean = elevatorLoad match {
  //    case DoubleLoad(Microchip(micro, _), Generator(gen, _)) => gen == micro
  //    case DoubleLoad(Generator(gen, _), Microchip(micro, _)) => gen == micro
  //    case _ => true
  //  }

  def uniquePairs[T](seq: Set[T]): Set[(T, T)] = {
    for {
      (x, idxX) <- seq.zipWithIndex
      (y, idxY) <- seq.zipWithIndex
      if idxX < idxY
    } yield (x, y)
  }

  def generateMoves(state: State): Set[State] = {
    var states = Set[State]()

    val floor = state.floors(state.elevator)
    val doubleLoads: Set[ElevatorLoad] = uniquePairs(floor).map { case (x, y) => DoubleLoad(x, y) }
    val singleLoads: Set[ElevatorLoad] = floor.map(e => SingleLoad(e))
    val allLoads: Set[ElevatorLoad] = singleLoads ++ doubleLoads

    //elevator can go down
    if (state.elevator > 0) {
      states = states ++ allLoads.map(load => doLoad(state, load, state.elevator - 1))
    }
    //elevator can go down
    if (state.elevator < state.floors.length - 1) {
      states = states ++ allLoads.map(load => doLoad(state, load, state.elevator + 1))
    }
    states.filter(isValid)
  }

  def score(state: State): Int = Int.MaxValue - state.floors.zipWithIndex.map { case (f, idx) => (idx + 1) * f.size }.sum

  implicit def orderState: Ordering[State] = Ordering.by(s => score(s))

  def solve1(state: State, goal: State): (mutable.Map[State, Long], mutable.Map[State, State]) = {
    var count = 0;
    val steps = mutable.Map[State, Long]()
    val prev = mutable.Map[State, State]()

    steps += (state -> 0)
    val queue: MinPriorityQueue[State] = new FibonacciHeap[State]()

    queue.insert(state, 0)

    var current: Option[State] = None

    while (!queue.isEmpty && !current.contains(goal)) {
      if (count % 1 == 0) {
        println(count)
      }
      count += 1
      current = Some(queue.extractMinimum())
      //      current.foreach(printState)
      generateMoves(current.get).foreach(n => {
        if (steps.get(n).isEmpty) {
          queue.insert(n, Long.MaxValue)
        }
        val alt = steps(current.get) + 1
        if (steps.get(n).forall(d => alt < d)) {
          steps += (n -> alt)
          prev += (n -> current.get)
          queue.decreasePriority(n, alt)
        }
      })
    }
    (steps, prev)
  }


  def findPath(state: State, prev: mutable.Map[State, State]): Seq[State] = {
    @tailrec
    def loop(from: State, path: Seq[State]): Seq[State] = {
      if (prev.get(from).isEmpty) path :+ from else {
        loop(prev(from), path :+ from)
      }
    }

    loop(state, List())
  }

  def printState(state: State) = {
    println("----")
    state.floors.zipWithIndex.reverse.foreach { case (f, n) => {
      if (n == state.elevator) print("E\t") else print(" \t")
      println(f.mkString(" "))
    }
    }
    println("----")
  }

  def end(state: State): State = {
    val all: Set[Element] = state.floors.flatten.toSet
    val finalFloors: Seq[Floor] = (0 until state.floors.size - 1).map(_ => Set[Element]()).toList :+ all
    State(finalFloors, state.floors.size - 1)
  }

  def solvePath(state: State): (mutable.Map[State, Long], mutable.Map[State, State]) = {
    val finish = end(state)
    val (steps, prev) = solve1(state, finish)
    findPath(finish, prev).reverse.foreach(printState)
    println(steps.get(finish))
    (steps, prev)
  }

  override def main(args: Array[String]): Unit = {
    val testFloors: Seq[Floor] = List(
      Set(Microchip("H", 1), Microchip("L", 2)),
      Set(Generator("H", 3)),
      Set(Generator("L", 4))
      //      Set()
    )

    val id = new AtomicInteger()

    val generator = (name: String) => {
      Generator(name, id.getAndIncrement())
    }

    val microchip = (name: String) => Microchip(name, id.getAndIncrement())

    val test = State(testFloors, 0)

    val cesarFloor: Seq[Floor] = List(
      Set(generator("Polonium"), generator("Thulium"), microchip("Thulium"), generator("Promethium"), generator("Ruthenium"), microchip("Ruthenium"), generator("Cobalt"), microchip("Cobalt")),
      Set(microchip("Polonium"), microchip("Promethium")),
      Set(),
      Set()
    )

    val cesarFloor2: Seq[Floor] = List(
      Set(generator("Polonium"), generator("Thulium"), microchip("Thulium"), generator("Promethium"), generator("Ruthenium"), microchip("Ruthenium"), generator("Cobalt"), microchip("Cobalt"), generator("dilithium"), microchip("dilithium"), generator("elerium"), microchip("elerium")),
      Set(microchip("Polonium"), microchip("Promethium"))
      , Set()
      , Set()
    )

    val cesar = State(cesarFloor2, 0)

    val (steps, prev) = solvePath(cesar)
  }
}
