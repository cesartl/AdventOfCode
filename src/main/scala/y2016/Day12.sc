import scala.annotation.tailrec

sealed trait Ref {
}

case class Register(name: String) extends Ref

case class Number(number: Long) extends Ref

def parseRef(s: String) : Ref = {
  if (s.contains("-")) Number(s.toLong)
  else if (s.forall(_.isDigit)) Number(s.toLong)
  else Register(s)
}


sealed trait Instruction {
}

case class Copy(x: Ref, y: String) extends Instruction

case class Inc(x: String) extends Instruction

case class Dec(x: String) extends Instruction

case class Jump(x: Ref, y: Ref) extends Instruction

def parseInstruction(s: String): Instruction = {
  val strings = s.split(" ")
  strings(0) match {
    case "cpy" => Copy(parseRef(strings(1)), strings(2))
    case "inc" => Inc(strings(1))
    case "dec" => Dec(strings(1))
    case "jnz" => Jump(parseRef(strings(1)), parseRef(strings(2)))
    case _ => throw new IllegalArgumentException(s)
  }
}

case class State(registers: Map[String, Long], history: Map[String, Long], terminated: Boolean, instructions: Seq[Instruction], position: Long) {

  private[this] def getValue(name: String): Long = registers.getOrElse(name, 0)

  def getValue(ref: Ref): Long = ref match {
    case Register(name) => getValue(name)
    case Number(number) => number
  }

  def terminate() = State(registers, history, terminated = true, instructions, position)

  def updateValue(name: String, value: Long): State = State(registers + (name -> value), history, terminated, instructions, position)

  private def next(): State = next(1)

  private def next(n: Long): State = if (position + n >= instructions.length || position + n < 0) terminate() else {
    State(registers, history, terminated, instructions, position + n)
  }

  def execute(): State = {
    instructions(position.toInt) match {
      case Copy(x, y) => updateValue(y, getValue(x)).next()
      case Inc(x) => updateValue(x, getValue(x) + 1).next()
      case Dec(x) => updateValue(x, getValue(x) - 1).next()
      case Jump(x, y) => if(getValue(x) != 0) next(getValue(y)) else next()
    }
  }
}

def initState(instructions: Seq[Instruction]): State = State(Map(), Map(), terminated = false, instructions, 0)


def executeProgram(prg: String) : Map[String, Long] = {
  val instructions = prg.split("\n").map(parseInstruction).toVector
  val state = initState(instructions)
  @tailrec
  def loop(s: State): State = if(s.terminated) s else loop(s.execute())
  loop(state).registers
}

def executeProgram2(prg: String) : Map[String, Long] = {
  val instructions = prg.split("\n").map(parseInstruction).toVector
  val state = initState(instructions).updateValue("c", 1)
  @tailrec
  def loop(s: State): State = if(s.terminated) s else loop(s.execute())
  loop(state).registers
}

executeProgram("cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a")

val cesar = "cpy 1 a\ncpy 1 b\ncpy 26 d\njnz c 2\njnz 1 5\ncpy 7 c\ninc d\ndec c\njnz c -2\ncpy a c\ninc a\ndec b\njnz b -2\ncpy c b\ndec d\njnz d -6\ncpy 18 c\ncpy 11 d\ninc a\ndec d\njnz d -2\ndec c\njnz c -5"

executeProgram2(cesar)


