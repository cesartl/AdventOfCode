import scala.annotation.tailrec

sealed trait Ref {
}

case class Register(name: String) extends Ref

case class Number(number: Int) extends Ref

def parseRef(s: String) = {
  if (s.forall(_.isDigit)) Number(s.toInt) else Register(s)
}

sealed trait Instruction {
  def execute(state: State): State
}

case class State(registers: Map[String, Int], history: Map[String, Int], terminated: Boolean, instructions: Seq[Instruction], position: Int) {

  def getValue(name: String) = registers.getOrElse(name, 0)

  def getValue(ref: Ref): Int = ref match {
    case Register(name) => getValue(name)
    case Number(number) => number
  }

  def terminate() = State(registers, history, terminated = true, instructions, position)

  def updateValue(name: String, value: Int): State = State(registers + (name -> value), history, terminated, instructions, position)

  def recover(): State = State(registers, history, terminated, instructions, position)

  def record(instruction: Instruction): State = State(registers, history + (instruction.getClass.getSimpleName -> (history.getOrElse(instruction.getClass.getSimpleName, 0) + 1)), terminated, instructions, position)

  def next(): State = next(1)

  def next(n: Int): State = if (position + n >= instructions.length || position + n < 0) terminate() else State(registers, history, terminated = true, instructions, position + n)

  def execute(): State = {
    instructions(position).execute(this)
  }
}

def init(instructions: Seq[Instruction]): State = State(Map(), Map(), terminated = false, instructions, 0)


case class Sub(x: String, y: Ref) extends Instruction {
  override def execute(state: State) = state.updateValue(x, state.getValue(x) - state.getValue(y)).record(this).next()
}

case class Set(x: String, y: Ref) extends Instruction {
  override def execute(state: State) = state.updateValue(x, state.getValue(y)).record(this).next()
}


case class Mul(x: String, y: Ref) extends Instruction {
  override def execute(state: State) = state.updateValue(x, state.getValue(x) * state.getValue(y)).record(this).next()
}

case class Jump(x: String, y: Ref) extends Instruction {
  override def execute(state: State) = if (state.getValue(x) != 0) state.record(this).next(state.getValue(y)) else state.record(this).next()
}

def parseInstruction(s: String): Instruction = {
  val strings = s.split(" ")
  strings(0) match {
    case "set" => Set(strings(1), parseRef(strings(2)))
    case "sub" => Sub(strings(1), parseRef(strings(2)))
    case "mul" => Mul(strings(1), parseRef(strings(2)))
    case "jgz" => Jump(strings(1), parseRef(strings(2)))
    case _ => throw new IllegalArgumentException(s)
  }
}

val test = "set b 57\nset c b\njnz a 2\njnz 1 5\nmul b 100\nsub b -100000\nset c b\nsub c -17000\nset f 1\nset d 2\nset e 2\nset g d\nmul g e\nsub g b\njnz g 2\nset f 0\nsub e -1\nset g e\nsub g b\njnz g -8\nsub d -1\nset g d\nsub g b\njnz g -13\njnz f 2\nsub h -1\nset g b\nsub g c\njnz g 2\njnz 1 3\nsub b -17\njnz 1 -23"

val i1 = test.split("\n").map(parseInstruction).toList

@tailrec
def solve(state: State): State = {
  println(state)
  if (state.terminated) state else solve(state.execute())
}

val s = solve(init(i1))

println(s)

s.history.get(Mul.getClass.getSimpleName)

println("cesar")

