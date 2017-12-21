import java.util.regex.Pattern

trait Operation {
  def test(value: Int): Boolean
}

case class Le(test: Int) extends Operation {
  def test(value: Int) = value < test
}

case class Lea(test: Int) extends Operation {
  override def test(value: Int) = value <= test
}

case class Ge(test: Int) extends Operation {
  override def test(value: Int) = value > test
}

case class Gea(test: Int) extends Operation {
  override def test(value: Int) = value >= test
}

case class Eq(test: Int) extends Operation {
  override def test(value: Int) = value == test
}

case class NotEq(test: Int) extends Operation {
  override def test(value: Int) = value != test
}

def parseOperation(p: (String, String)) = {
  val test = p._2.toInt
  p._1 match {
    case "<" => Le(test)
    case "<=" => Lea(test)
    case ">" => Ge(test)
    case ">=" => Gea(test)
    case "==" => Eq(test)
    case "!=" => NotEq(test)
    case _ => throw new IllegalArgumentException(p.toString())
  }
}


case class Condition(register: String, operation: Operation) {
  def test(map: Map[String, Int]): Boolean = {
    operation.test(map.getOrElse(register, 0))
  }
}

case class Instruction(register: String, inc: Boolean, amount: Int, condition: Condition)

val pattern = Pattern.compile("([\\w]+) ([\\w]+) ([0-9\\-]+) if ([\\w]+) ([><=!]+) ([0-9\\-]+)")

def parse(line: String): Instruction = {
  val m = pattern.matcher(line)
  if (m.matches()) {
    val reg = m.group(1)
    val inc = m.group(2).equals("inc")
    val amount = m.group(3).toInt
    val testReg = m.group(4)
    val op = parseOperation((m.group(5), m.group(6)))
    Instruction(reg, inc, amount, Condition(testReg, op))
  } else {
    throw new IllegalArgumentException(line)
  }
}


parse("b inc 5 if a > 1")
val input = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"
def split(input: String): Seq[Instruction] = input.split("\n").map(parse)
split(input)

def solve(instructions: Seq[Instruction]) = {
  var map = Map[String, Int]()
  var max = Integer.MIN_VALUE
  instructions.foreach(i => {
    if (i.condition.test(map)) {
      val newValue = map.getOrElse(i.register, 0) + i.amount * (if (i.inc) 1 else -1)
      max = Math.max(max, newValue)
      map = map + (i.register -> newValue)
    }
  })
  println("max: " + max)
  map.maxBy({case (_, v) => v})._2
}

def solve2(instructions: Seq[Instruction]) = instructions.foldLeft((Map[String, Int](), Integer.MIN_VALUE))((state, i) => {
  val map = state._1
  if (i.condition.test(map)) {
    val newValue = map.getOrElse(i.register, 0) + i.amount * (if (i.inc) 1 else -1)
    val max = Math.max(state._2, newValue)
    (map + (i.register -> newValue), max)
  }else{
    state
  }
})

def problem(input: String) = solve2(split(input))

val i1= "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"
problem(i1)


