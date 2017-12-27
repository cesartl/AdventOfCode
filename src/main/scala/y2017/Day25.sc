type Tape = Map[Long, Int]

sealed trait State {
  def next(turingMachine: TuringMachine): TuringMachine
}

case class StateA() extends State {
  override def next(turingMachine: TuringMachine) = {
    if (turingMachine.current() == 0) {
      val tape = turingMachine.tape + (turingMachine.position -> 1)
      TuringMachine(tape, turingMachine.right(), StateB())
    } else {
      val tape = turingMachine.tape + (turingMachine.position -> 0)
      TuringMachine(tape, turingMachine.left(), StateC())
    }
  }
}

case class StateB() extends State {
  override def next(turingMachine: TuringMachine) = {
    if (turingMachine.current() == 0) {
      val tape = turingMachine.tape + (turingMachine.position -> 1)
      TuringMachine(tape, turingMachine.left(), StateA())
    } else {
      val tape = turingMachine.tape + (turingMachine.position -> 1)
      TuringMachine(tape, turingMachine.left(), StateD())
    }
  }

}

case class StateC() extends State {
  override def next(turingMachine: TuringMachine) = {
    if (turingMachine.current() == 0) {
      val tape = turingMachine.tape + (turingMachine.position -> 1)
      TuringMachine(tape, turingMachine.right(), StateD())
    } else {
      val tape = turingMachine.tape + (turingMachine.position -> 0)
      TuringMachine(tape, turingMachine.right(), StateC())
    }
  }
}

case class StateD() extends State {
  override def next(turingMachine: TuringMachine) = {
    if (turingMachine.current() == 0) {
      val tape = turingMachine.tape + (turingMachine.position -> 0)
      TuringMachine(tape, turingMachine.left(), StateB())
    } else {
      val tape = turingMachine.tape + (turingMachine.position -> 0)
      TuringMachine(tape, turingMachine.right(), StateE())
    }
  }
}

case class StateE() extends State {
  override def next(turingMachine: TuringMachine) = {
    if (turingMachine.current() == 0) {
      val tape = turingMachine.tape + (turingMachine.position -> 1)
      TuringMachine(tape, turingMachine.right(), StateC())
    } else {
      val tape = turingMachine.tape + (turingMachine.position -> 1)
      TuringMachine(tape, turingMachine.left(), StateF())
    }
  }
}

case class StateF() extends State {
  override def next(turingMachine: TuringMachine) = {
    if (turingMachine.current() == 0) {
      val tape = turingMachine.tape + (turingMachine.position -> 1)
      TuringMachine(tape, turingMachine.left(), StateE())
    } else {
      val tape = turingMachine.tape + (turingMachine.position -> 1)
      TuringMachine(tape, turingMachine.right(), StateA())
    }
  }
}

case class TuringMachine(tape: Tape, position: Long, state: State) {
  def current(): Int = tape.getOrElse(position, 0)

  def right(): Long = position + 1

  def left(): Long = position - 1

  def next(): TuringMachine = state.next(this)
}

def solve1(turingMachine: TuringMachine, count: Long): TuringMachine =
  if (count == 0) turingMachine else solve1(turingMachine.next(), count - 1)

val start = TuringMachine(Map(), 0, StateA())

val end = solve1(start, 12172063)

end.tape.count(p => p._2 == 1)


