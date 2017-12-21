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

case class State(registers: Map[String, Int], soundPlayed: Int, recovered: Boolean, instructions: Seq[Instruction], position: Int) {

  def getValue(name: String) = registers.getOrElse(name, 0)

  def getValue(ref: Ref): Int = ref match {
    case Register(name) => getValue(name)
    case Number(number) => number
  }

  def updateValue(name: String, value: Int): State = State(registers + (name -> value), soundPlayed, recovered, instructions, position)

  def recover(): State = State(registers, soundPlayed, recovered = true, instructions, position)

  def next(): State = next(1)

  def next(n: Int): State = State(registers, soundPlayed, recovered, instructions, position + n)

  def execute(): State = {
    instructions(position).execute(this)
  }
}

def init(instructions: Seq[Instruction]): State = State(Map(), 0, false, instructions, 0)


case class Snd(v: Ref) extends Instruction {
  override def execute(state: State) = State(state.registers, state.getValue(v), state.recovered, state.instructions, state.position).next()
}

case class Set(x: String, y: Ref) extends Instruction {
  override def execute(state: State) = state.updateValue(x, state.getValue(y)).next()
}


case class Add(x: String, y: Ref) extends Instruction {
  override def execute(state: State) = state.updateValue(x, state.getValue(x) + state.getValue(y)).next()
}

case class Mul(x: String, y: Ref) extends Instruction {
  override def execute(state: State) = state.updateValue(x, state.getValue(x) * state.getValue(y)).next()
}

case class Mod(x: String, y: Ref) extends Instruction {
  override def execute(state: State) = state.updateValue(x, state.getValue(x) % state.getValue(y)).next()
}

case class Rcv(v: Ref) extends Instruction {
  override def execute(state: State) = {
    val value = state.getValue(v)
    if (value != 0) state.recover().next() else state.next()
  }
}

case class Jump(x: String, y: Ref) extends Instruction {
  override def execute(state: State) = if (state.getValue(x) != 0) state.next(state.getValue(y)) else state.next()
}

def parseInstruction(s: String): Instruction = {
  val strings = s.split(" ")
  strings(0) match {
    case "snd" => Snd(parseRef(strings(1)))
    case "set" => Set(strings(1), parseRef(strings(2)))
    case "add" => Add(strings(1), parseRef(strings(2)))
    case "mul" => Mul(strings(1), parseRef(strings(2)))
    case "mod" => Mod(strings(1), parseRef(strings(2)))
    case "rcv" => Rcv(parseRef(strings(1)))
    case "jgz" => Jump(strings(1), parseRef(strings(2)))
    case _ => throw new IllegalArgumentException(s)
  }
}

val test = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"

val i1 = test.split("\n").map(parseInstruction).toList

@tailrec
def solve(state: State): State = {
  println(state)
  if (state.recovered) state else solve(state.execute())
}

solve(init(i1))
