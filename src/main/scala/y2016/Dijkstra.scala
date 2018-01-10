package y2016

import scala.annotation.tailrec
import scala.collection.mutable

object Dijkstra {

  type NodeGenerator[T] = T => Seq[T]
  type Distance[T] = (T, T) => Long

  case class PathingResult[T](steps: Map[T, Long], previous: Map[T, T])

  sealed trait Constraint

  case class StepConstraint(maxStep: Long) extends Constraint

  def traverse[T](start: T, end: T, nodeGenerator: NodeGenerator[T], distance: Distance[T], constraints: Seq[Constraint]): PathingResult[T] = {
    var count = 0
    val steps = mutable.Map[T, Long]()
    val prev = mutable.Map[T, T]()

    steps += (start -> 0)
    val queue: MinPriorityQueue[T] = new FibonacciHeap[T]()
    queue.insert(start, 0)

    var current: Option[T] = None

    while (!queue.isEmpty && !current.contains(end) && current.forall(c => {
      constraints.forall {
        case StepConstraint(maxSteps) => steps(c) < maxSteps
      }
    })) {
      count += 1
      current = Some(queue.extractMinimum())
      //      println(current)
      nodeGenerator(current.get).foreach(n => {
        if (!queue.contains(n)) {
          queue.insert(n, Long.MaxValue)
        }
        val alt = steps(current.get) + distance(current.get, n)
        if (steps.get(n).forall(d => alt < d)) {
          steps += (n -> alt)
          prev += (n -> current.get)
          queue.decreasePriority(n, alt)
        }
      })
    }
    PathingResult(steps.toMap, prev.toMap)
  }

  def findPath[T](end: T, pathingResult: PathingResult[T]): Seq[T] = findPath(end, pathingResult.previous)

  def findPath[T](end: T, prev: Map[T, T]): Seq[T] = {
    @tailrec
    def loop(from: T, path: Seq[T]): Seq[T] = {
      if (prev.get(from).isEmpty) path :+ from else {
        loop(prev(from), path :+ from)
      }
    }

    loop(end, List()).reverse
  }
}
