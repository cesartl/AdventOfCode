package y2017

import scala.collection.immutable.Queue

object Day18p2 extends App {

  import scala.annotation.tailrec

  sealed trait Ref {
  }

  case class Register(name: String) extends Ref

  case class Number(number: BigInt) extends Ref

  def parseRef(s: String) = {
    if (s.contains("-")) Number(BigInt(s.toLong))
    else if (s.forall(_.isDigit)) Number(BigInt(s.toLong))
    else Register(s)
  }

  case class Env(queue0: Queue[BigInt], queue1: Queue[BigInt], count1: BigInt) {
    def sendTo0(v: BigInt) = Env(queue0.enqueue(v), queue1, count1 + 1)

    def sendTo1(v: BigInt) = Env(queue0, queue1.enqueue(v), count1)

    def receive0(): Option[(BigInt, Env)] = {
      receive(queue0).map(o => (o._1, Env(o._2, queue1, count1)))
    }

    def receive1(): Option[(BigInt, Env)] = {
      receive(queue1).map(o => (o._1, Env(queue0, o._2, count1)))
    }

    def receive(queue: Queue[BigInt]): Option[(BigInt, Queue[BigInt])] = {
      if (queue.nonEmpty) {
        Some(queue.dequeue)
      } else {
        None
      }
    }
  }

  sealed trait Instruction {
    def execute(env: Env, state: State): (Env, State)
  }

  case class State(registers: Map[String, BigInt], id: Int, position: Int, waiting: Boolean, terminated: Boolean, instructions: Seq[Instruction]) {

    private[this] def getValue(name: String): BigInt = registers.getOrElse(name, 0)

    def getValue(ref: Ref): BigInt = ref match {
      case Register(name) => getValue(name)
      case Number(number) => number
    }

    def terminate() = State(registers, id, position, waiting, terminated = true, instructions)

    def updateValue(name: String, value: BigInt): State = State(registers + (name -> value), id, position, waiting, terminated, instructions)

    def next(): State = next(1)

    def next(n: Int): State = if (position + n >= instructions.length || position + n < 0) terminate() else State(registers, id, position + n, waiting, terminated, instructions)

    def setWaiting(): State = State(registers, id, position, waiting = true, terminated, instructions)

    def notWaiting(): State = State(registers, id, position, waiting = false, terminated, instructions)

    def execute(env: Env): (Env, State) = {
      if (terminated) (env, this) else {
        val instruction = instructions(position)
//        println(id + " doing " + instruction)
        instruction.execute(env, this)
      }
    }
  }

  def init(id: Int, instructions: Seq[Instruction]): State = State(Map("p" -> id), id, 0, waiting = false, terminated = false, instructions)


  case class Snd(v: Ref) extends Instruction {
    override def execute(env: Env, state: State) = {
      if (state.id == 0) {
        (env.sendTo1(state.getValue(v)), state.next())
      } else {
        (env.sendTo0(state.getValue(v)), state.next())
      }
    }
  }

  case class Set(x: String, y: Ref) extends Instruction {
    override def execute(env: Env, state: State) = (env, state.updateValue(x, state.getValue(y)).next())
  }


  case class Add(x: String, y: Ref) extends Instruction {
    override def execute(env: Env, state: State) = (env, state.updateValue(x, state.getValue(x) + state.getValue(y)).next())
  }

  case class Mul(x: String, y: Ref) extends Instruction {
    override def execute(env: Env, state: State) = (env, state.updateValue(x, state.getValue(x) * state.getValue(y)).next())
  }

  case class Mod(x: String, y: Ref) extends Instruction {
    override def execute(env: Env, state: State) =
      (env, state.updateValue(x, state.getValue(x) % state.getValue(y)).next())

  }

  case class Rcv(name: String) extends Instruction {
    override def execute(env: Env, state: State): (Env, State) = {
      if (state.id == 0) {
        env.receive0().map(p => (p._2, state.updateValue(name, p._1).notWaiting().next())).getOrElse((env, state.setWaiting()))
      } else {
        env.receive1().map(p => (p._2, state.updateValue(name, p._1).notWaiting().next())).getOrElse((env, state.setWaiting()))
      }
    }
  }

  case class Jump(x: Ref, y: Ref) extends Instruction {
    override def execute(env: Env, state: State) = {
      //      println("jump " + state.getValue(y))
      if (state.getValue(x).compareTo(BigInt(0)) > 0) (env, state.next(state.getValue(y).toInt)) else (env, state.next())
    }
  }

  def parseInstruction(s: String): Instruction = {
    val strings = s.split(" ")
    strings(0) match {
      case "snd" => Snd(parseRef(strings(1)))
      case "set" => Set(strings(1), parseRef(strings(2)))
      case "add" => Add(strings(1), parseRef(strings(2)))
      case "mul" => Mul(strings(1), parseRef(strings(2)))
      case "mod" => Mod(strings(1), parseRef(strings(2)))
      case "rcv" => Rcv(strings(1))
      case "jgz" => Jump(parseRef(strings(1)), parseRef(strings(2)))
      case _ => throw new IllegalArgumentException(s)
    }
  }

  def solve2(instructions: Seq[Instruction]) = {
    val p0 = init(0, instructions)
    val p1 = init(1, instructions)
    val environment = Env(Queue(), Queue(), 0)
    val max = 800000

    @tailrec
    def inner(s1: State, s2: State, env: Env, waiting: Boolean, count: Int): (State, State, Env) = {
      val prefix = count + "\t"
//      println(prefix + env)
//      println(prefix + "s0: " + s1.registers)
//      println(prefix + "s1: " + s2.registers)
      if (count >= max || s1.terminated && s1.terminated) (s1, s2, env)
      else if (waiting && s1.waiting & s2.waiting) inner(s1.terminate(), s2.terminate(), env, waiting, count + 1)
      else if (s1.waiting && s2.waiting) {
        val (foo, s1New) = s1.execute(env)
//        println(prefix + foo)
        val (bar, s2New) = s2.execute(foo)
        inner(s1New, s2New, bar, waiting = true, count + 1)
      }
      else {
        val (foo, s1New) = s1.execute(env)
//        println(prefix + foo)
        val (bar, s2New) = s2.execute(foo)
        inner(s1New, s2New, bar, waiting = false, count + 1)
      }
    }
    inner(p0, p1, environment, waiting = false, 0)
  }

  override def main(args: Array[String]): Unit = {
    val test = "snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d"
    val test2 = "jgz p 2\nrcv a\nset x 1\nadd x 10\nmul x 2\nmod x 3\nsnd x\njgz x 10"
    val cesar = "set i 31\nset a 1\nmul p 17\njgz p p\nmul a 2\nadd i -1\njgz i -2\nadd a -1\nset i 127\nset p 622\nmul p 8505\nmod p a\nmul p 129749\nadd p 12345\nmod p a\nset b p\nmod b 10000\nsnd b\nadd i -1\njgz i -9\njgz a 3\nrcv b\njgz b -1\nset f 0\nset i 126\nrcv a\nrcv b\nset p a\nmul p -1\nadd p b\njgz p 4\nsnd a\nset a b\njgz 1 3\nsnd b\nset f 1\nadd i -1\njgz i -11\nsnd a\njgz f -16\njgz a -19"
    val i1 = test2.split("\n").map(parseInstruction).toList
    val i2 = cesar.split("\n").map(parseInstruction).toList

    val s = solve2(i2)
    println(s)
    println(s._3.count1)
  }
}
