package y2017

import y2017.Day18p2.{Number, Register}

object Day23 extends App {

  import scala.annotation.tailrec

  sealed trait Ref {
  }

  case class Register(name: String) extends Ref

  case class Number(number: Long) extends Ref

  def parseRef(s: String) = {
    if (s.contains("-")) Number(s.toLong)
    else if (s.forall(_.isDigit)) Number(s.toLong)
    else Register(s)
  }

  sealed trait Instruction {
    def execute(state: State): State
  }

  case class State(registers: Map[String, Long], history: Map[String, Long], terminated: Boolean, instructions: Seq[Instruction], position: Long) {

    private[this] def getValue(name: String): Long = registers.getOrElse(name, 0)

    def getValue(ref: Ref): Long = ref match {
      case Register(name) => getValue(name)
      case Number(number) => number
    }

    def terminate() = State(registers, history, terminated = true, instructions, position)

    def updateValue(name: String, value: Long): State = State(registers + (name -> value), history, terminated, instructions, position)

    def recover(): State = State(registers, history, terminated, instructions, position)

    def record(instruction: Instruction): State = State(registers, history + (instruction.getClass.getSimpleName -> (history.getOrElse(instruction.getClass.getSimpleName, 0L) + 1L)), terminated, instructions, position)

    def next(): State = next(1)

    def next(n: Long): State = if (position + n >= instructions.length || position + n < 0) terminate() else {
      State(registers, history, terminated, instructions, position + n)
    }

    def execute(): State = {
      val instr = instructions(position.toInt)
      instr.execute(this)
    }
  }

  def init(instructions: Seq[Instruction]): State = State(Map(), Map(), terminated = false, instructions, 0)

  def isPrime(n: Long): Long = {
    //check if n is a multiple of 2
    if (n % 2 == 0) return 0
    //if not, then just check the odds
    var i = 3
    while (i * i <= n) {
      if (n % i == 0) return 0
      i += 2
    }
    1
  }

  case class Sub(x: String, y: Ref) extends Instruction {
    override def execute(state: State) = state.updateValue(x, state.getValue(Register(x)) - state.getValue(y)).record(this).next()
  }

  case class Set(x: String, y: Ref) extends Instruction {
    override def execute(state: State) = state.updateValue(x, state.getValue(y)).record(this).next()
  }

  case class Mul(x: String, y: Ref) extends Instruction {
    override def execute(state: State) = state.updateValue(x, state.getValue(Register(x)) * state.getValue(y)).record(this).next()
  }

  case class Jump(x: Ref, y: Ref) extends Instruction {
    override def execute(state: State) = if (state.getValue(x) != 0) state.record(this).next(state.getValue(y)) else state.record(this).next()
  }

  case class Prime(x: String, y: Ref) extends Instruction {
    override def execute(state: State): State = state.updateValue(x, isPrime(state.getValue(y))).next()
  }

  def parseInstruction(s: String): Instruction = {
    val strings = s.split(" ")
    strings(0) match {
      case "set" => Set(strings(1), parseRef(strings(2)))
      case "sub" => Sub(strings(1), parseRef(strings(2)))
      case "mul" => Mul(strings(1), parseRef(strings(2)))
      case "jnz" => Jump(parseRef(strings(1)), parseRef(strings(2)))
      case "prime" => Prime(strings(1), parseRef(strings(2)))
      case _ => throw new IllegalArgumentException(s)
    }
  }


  @tailrec
  def solve(state: State): State = {
    println(state)
    if (state.terminated) state else solve(state.execute())
  }

  override def main(args: Array[String]): Unit = {
    val test = "set b 57\nset c b\njnz a 2\njnz 1 5\nmul b 100\nsub b -100000\nset c b\nsub c -17000\nset f 1\nset d 2\nset e 2\nset g d\nmul g e\nsub g b\njnz g 2\nset f 0\nsub e -1\nset g e\nsub g b\njnz g -8\nsub d -1\nset g d\nsub g b\njnz g -13\njnz f 2\nsub h -1\nset g b\nsub g c\njnz g 2\njnz 1 3\nsub b -17\njnz 1 -23"
    val part2 = "set b 57\nset c b\njnz a 2\njnz 1 5\nmul b 100\nsub b -100000\nset c b\nsub c -17000\nset f 1\nprime b x\njnz x 2\nset f 0\njnz f 2\nsub h -1\nset g b\nsub g c\njnz g 2\njnz 1 3\nsub b -17\njnz 1 -11"

    val i1 = test.split("\n").map(parseInstruction).toList
    val i2 = part2.split("\n").map(parseInstruction).toList

    val p2s = init(i2).updateValue("a", 1)
    val s = solve(p2s)
    println(s)

  }

}
