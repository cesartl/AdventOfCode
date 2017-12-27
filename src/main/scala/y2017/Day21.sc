import scala.annotation.tailrec

type Row[A] = Vector[A]

type Matrix[A] = Vector[Row[A]]

def flipH[A](m: Matrix[A]): Matrix[A] = m.map(r => r.reverse)

def flipV[A](m: Matrix[A]): Matrix[A] = m.transpose.map(c => c.reverse).transpose

val m1: Matrix[Char] = Vector(Vector('.', '#', '.'), Vector('.', '.', '#'), Vector('#', '#', '#'))
val m2: Matrix[Char] = Vector(Vector('#', '#', '.', '#', '#', '.'), Vector('.', '.', '#', '.', '.', '#'), Vector('#', '#', '#', '#', '#', '#'), Vector('#', '#', '.', '#', '#', '.'), Vector('.', '.', '#', '.', '.', '#'), Vector('#', '#', '#', '#', '#', '#'))


case class RichMatrix[A](m: Matrix[A]) {
  def toStr = "\n" + m.map {
    _.map {
      "\t" + _
    }.reduceLeft(_ + _) + "\n"
  }.reduceLeft(_ + _)

  def flip1[A] = flipH(m)

  def flip2[A] = flipV(m)

  def rotate[A] = m.transpose

  def split(n: Int): Seq[Matrix[A]] = m.grouped(n).toVector.flatMap(g => g.transpose.grouped(n).map(_.transpose).toVector)

  def combine(m2: Matrix[A]): Matrix[A] = {
    if (m.size != m2.size) throw new IllegalArgumentException("Cannot combine " + m + " and  " + m2)
    else {
      m.zipWithIndex.map(p => p._1 ++ m2(p._2))
    }
  }

  def count(a : A) : Int = m.flatMap(r => r.filter(x => x.equals(a))).size

}

implicit def pimp[A](m: Matrix[A]) = new RichMatrix(m)

def combineBy[A](matrices: Seq[Matrix[A]], by: Int): Matrix[A] = {
  matrices.grouped(by).map(g => g.reduce((x, y) => x.combine(y))).toVector.flatten
}

def allCombination[A](m: Matrix[A]): Seq[Matrix[A]] = Vector(
  m, m.flip1, m.flip2,
  m.flip1.flip2,
  m.flip1.flip2.transpose,
  m.transpose, m.transpose.flip1, m.transpose.flip2,
  m.transpose.flip1.flip2,
  m.transpose.flip2.flip1.transpose,
  m.flip1.transpose,
  m.flip1.transpose.flip1,
  m.flip1.transpose.flip2,
  m.flip1.transpose.flip1.flip2,
  m.flip2.transpose,
  m.flip2.transpose.flip1,
  m.flip2.transpose.flip2,
  m.flip2.transpose.flip1.flip2,
  m.flip1.transpose.flip1.transpose,
  m.flip1.transpose.flip2.transpose,
  m.flip1.transpose.flip1.flip2.transpose,
  m.flip2.transpose.flip1.transpose,
  m.flip2.transpose.flip2.transpose,
  m.flip2.transpose.flip1.flip2.transpose,
  m.flip1.flip2.transpose.flip1,
  m.flip1.flip2.transpose.flip2,
  m.flip1.flip2.transpose.flip1.flip2
)

//allCombination(m1).foreach(m => println(m.toStr))

type Picture = Matrix[Char]

case class Rule(patterns: Seq[Picture], output: Picture) {
  override def toString = output.toString()
}

def parseRule(s: String): Rule = {
  val split = s.split("=>").map(_.trim).map(parse)
  Rule(allCombination(split(0)), split(1))
}


def parse(s: String): Picture = s.split("/").toVector.map(r => r.toCharArray.toVector)

def applyFirstMatch(picture: Picture, rules: Seq[Rule]): Picture = {
  val o = rules.find(r => r.patterns.contains(picture)).map(_.output)
  if (o.isEmpty) {
    throw new IllegalArgumentException("no match for " + picture)
  } else {
    o.get
  }
}


case class State(p: Picture, rules: Seq[Rule]) {
  def next(): State = {
    if (p.size % 2 == 0) {
      println("split by 2")
      val tmp = p.split(2)
//      tmp.foreach(x => println(x.toStr))
      val split = tmp.map(m => applyFirstMatch(m, rules))
      State(combineBy(split, p.size / 2), rules)
    } else if (p.size % 3 == 0) {
      println("split by 3")
      val tmp = p.split(3)
//      tmp.foreach(x => println(x.toStr))
      val split = tmp.map(m => applyFirstMatch(m, rules))
      State(combineBy(split, p.size / 3), rules)
    } else {
      throw new IllegalArgumentException("size " + p.size)
    }
  }
}


val test = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"

val rules = test.split("\n").map(parseRule)


m1.split(3)


m1.flatten.count(x => x.equals('#'))

State(m1, rules).next().next().p.count('#')

val cesar = "../.. => .../.../..#\n#./.. => #.#/..#/...\n##/.. => #.#/..#/#.#\n.#/#. => #../.../.##\n##/#. => ###/#.#/..#\n##/## => #.#/.../#..\n.../.../... => #..#/..../.##./....\n#../.../... => ..../.##./#.../.##.\n.#./.../... => .#../####/..##/#...\n##./.../... => ##.#/..#./####/...#\n#.#/.../... => ##.#/##../#.#./.#..\n###/.../... => #..#/#..#/##../##.#\n.#./#../... => #.##/##../.#.#/..##\n##./#../... => #.#./..../.###/.#.#\n..#/#../... => ..##/####/..##/....\n#.#/#../... => ..##/###./..##/#...\n.##/#../... => #.../####/#..#/##..\n###/#../... => ...#/..../..##/#...\n.../.#./... => ##../##../..##/....\n#../.#./... => #.../.#.#/.##./#..#\n.#./.#./... => ..##/#.../...#/###.\n##./.#./... => ####/.#.#/..##/####\n#.#/.#./... => ####/.#../#.##/#..#\n###/.#./... => ..#./#..#/.#.#/###.\n.#./##./... => ##../.#.#/#..#/#..#\n##./##./... => .###/####/#..#/..##\n..#/##./... => ###./.#../..#./#.##\n#.#/##./... => ##../#.#./#.../.#.#\n.##/##./... => #.../#.../.#.#/####\n###/##./... => .#../####/#.../#.#.\n.../#.#/... => .#../..../##../.###\n#../#.#/... => .##./...#/.###/...#\n.#./#.#/... => ...#/#.../...#/####\n##./#.#/... => #.##/..#./#..#/.#.#\n#.#/#.#/... => #..#/..../..##/..#.\n###/#.#/... => .#.#/#.#./##.#/#.#.\n.../###/... => ##../.##./###./###.\n#../###/... => ###./..##/.#../##.#\n.#./###/... => .#../##../..../..##\n##./###/... => #.#./...#/...#/##..\n#.#/###/... => ..../.#../#.../.#..\n###/###/... => ..#./.###/..../##.#\n..#/.../#.. => #.#./.#../...#/##.#\n#.#/.../#.. => ...#/##.#/#.#./#...\n.##/.../#.. => ...#/..##/#.##/##.#\n###/.../#.. => #..#/.#.#/.##./..#.\n.##/#../#.. => ##../..#./#.##/##..\n###/#../#.. => ..../###./#.#./##..\n..#/.#./#.. => #.#./.##./.##./#...\n#.#/.#./#.. => .#../#..#/#.#./#...\n.##/.#./#.. => .#.#/#..#/..#./....\n###/.#./#.. => #.##/####/#.../..#.\n.##/##./#.. => #.##/.#.#/..../.#..\n###/##./#.. => #.##/####/.###/##..\n#../..#/#.. => ###./#.##/..#./..##\n.#./..#/#.. => ##../.#../..#./..##\n##./..#/#.. => #..#/.#../..../##.#\n#.#/..#/#.. => .###/.##./..#./#.#.\n.##/..#/#.. => .#.#/..../####/.#..\n###/..#/#.. => .##./##../...#/.#..\n#../#.#/#.. => #.#./#.##/..../.###\n.#./#.#/#.. => ####/#.#./.#../#.##\n##./#.#/#.. => ..##/.###/###./..#.\n..#/#.#/#.. => .##./..#./..../#.#.\n#.#/#.#/#.. => .###/..../..../##..\n.##/#.#/#.. => #.#./#.../####/.###\n###/#.#/#.. => #.../..##/###./#..#\n#../.##/#.. => ..../#.#./..##/.#.#\n.#./.##/#.. => ..##/..##/#..#/###.\n##./.##/#.. => #.../.#../#.#./#.##\n#.#/.##/#.. => ...#/#.../...#/###.\n.##/.##/#.. => ###./..../..##/#..#\n###/.##/#.. => #.#./##.#/####/#.#.\n#../###/#.. => ##../##../###./#..#\n.#./###/#.. => #.##/###./####/..##\n##./###/#.. => ..../.###/###./.#..\n..#/###/#.. => .###/..../..#./....\n#.#/###/#.. => ####/#..#/.#.#/..##\n.##/###/#.. => ..../##.#/####/##.#\n###/###/#.. => #..#/.#.#/###./.##.\n.#./#.#/.#. => #.##/...#/###./....\n##./#.#/.#. => #..#/.#../..../#.#.\n#.#/#.#/.#. => .#.#/####/..../.#.#\n###/#.#/.#. => #.#./#.##/##.#/##..\n.#./###/.#. => ..#./..../##../####\n##./###/.#. => #.##/##.#/#.##/.#..\n#.#/###/.#. => .#.#/..##/##.#/####\n###/###/.#. => .#../...#/#..#/#.#.\n#.#/..#/##. => .##./..#./...#/##.#\n###/..#/##. => ..#./##.#/#..#/#..#\n.##/#.#/##. => ##.#/#.../#..#/...#\n###/#.#/##. => ##../.#../..../.##.\n#.#/.##/##. => #.##/##.#/.#../.###\n###/.##/##. => ..../#.#./##../##.#\n.##/###/##. => ###./.#.#/.##./.###\n###/###/##. => #..#/.###/#.../#...\n#.#/.../#.# => .###/#.##/.#.#/#.#.\n###/.../#.# => ...#/##../...#/##.#\n###/#../#.# => ..../..#./..#./####\n#.#/.#./#.# => ##../#.##/...#/#...\n###/.#./#.# => #.#./...#/.#../#...\n###/##./#.# => .#../..#./...#/##..\n#.#/#.#/#.# => ####/#.##/.#../##..\n###/#.#/#.# => #.../#.../###./.#..\n#.#/###/#.# => ####/.#.#/.##./.#.#\n###/###/#.# => #.##/.#.#/##.#/..##\n###/#.#/### => .###/#.##/..../..#.\n###/###/### => .###/#..#/##../.##."

val cesarRules = cesar.split("\n").map(parseRule)


def toHash(p: Picture) = p.map(s => s.mkString("")).mkString("/").hashCode()

@tailrec
def solve(state: State, n: Int) : State = {
//  println(toHash(state.p))
//  println(state.p.toStr)
  if (n == 0) state else solve(state.next(), n - 1)
}

val s =solve(State(m1, cesarRules), 18).p.count('#')
////s.p.toStr
//s.p.count('#')
//2265464
//2265464

//toHash(m1)










