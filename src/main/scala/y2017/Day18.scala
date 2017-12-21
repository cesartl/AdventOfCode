package y2017

object Day18 extends App {

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

  case class State(registers: Map[String, Long], soundPlayed: Long, recovered: Boolean, instructions: Seq[Instruction], position: Int) {

    def getValue(name: String) : Long = registers.getOrElse(name, 0)

    def getValue(ref: Ref): Long = ref match {
      case Register(name) => getValue(name)
      case Number(number) => number
    }

    def updateValue(name: String, value: Long): State = State(registers + (name -> value), soundPlayed, recovered, instructions, position)

    def recover(): State = State(registers, soundPlayed, recovered = true, instructions, position)

    def next(): State = next(1)

    def next(n: Int): State = State(registers, soundPlayed, recovered, instructions, position + n)

    def execute(): State = {
      val instruction = instructions(position)
      println("doing " + instruction)
      instruction.execute(this)
    }
  }

  def init(instructions: Seq[Instruction]): State = State(Map(), 0, recovered = false, instructions, 0)


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
    override def execute(state: State) = {
      val dividend = state.getValue(x)
      val divisor = state.getValue(y)
      val v = ((dividend % divisor) + divisor) % divisor
      state.updateValue(x, state.getValue(x) % state.getValue(y)).next()
    }
  }

  case class Rcv(v: Ref) extends Instruction {
    override def execute(state: State) = {
      val value = state.getValue(v)
      if (value != 0) state.recover().next() else state.next()
    }
  }

  case class Jump(x: String, y: Ref) extends Instruction {
    override def execute(state: State) = if (state.getValue(x) > 0) state.next(state.getValue(y).toInt) else state.next()
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


  @tailrec
  def solve(state: State): State = {
    println(state)
    if (state.recovered) state else solve(state.execute())
  }

  override def main(args: Array[String]): Unit = {
    val test = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"
    val cesar = "set i 31\nset a 1\nmul p 17\njgz p p\nmul a 2\nadd i -1\njgz i -2\nadd a -1\nset i 127\nset p 622\nmul p 8505\nmod p a\nmul p 129749\nadd p 12345\nmod p a\nset b p\nmod b 10000\nsnd b\nadd i -1\njgz i -9\njgz a 3\nrcv b\njgz b -1\nset f 0\nset i 126\nrcv a\nrcv b\nset p a\nmul p -1\nadd p b\njgz p 4\nsnd a\nset a b\njgz 1 3\nsnd b\nset f 1\nadd i -1\njgz i -11\nsnd a\njgz f -16\njgz a -19"
    val i1 = test.split("\n").map(parseInstruction).toList
    val i2 = cesar.split("\n").map(parseInstruction).toList
    println(i2)
    var current = init(i2)
    //    for (i <- 0 until 34) {
    //      println(current)
    //      println()
    //      current = current.execute()
    //    }
    println(solve(init(i2)).soundPlayed)
  }
}
