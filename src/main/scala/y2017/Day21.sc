import scala.annotation.tailrec

type Row[A] = List[A]

type Matrix[A] = List[Row[A]]

def flipH[A](m: Matrix[A]): Matrix[A] = m.map(r => r.reverse)

def flipV[A](m: Matrix[A]): Matrix[A] = m.transpose.map(c => c.reverse).transpose

val m1: Matrix[Char] = List(List('.', '#', '.'), List('.', '.', '#'), List('#', '#', '#'))
val m2: Matrix[Char] = List(List('#', '#', '.', '#', '#', '.'), List('.', '.', '#', '.', '.', '#'), List('#', '#', '#', '#', '#', '#'), List('#', '#', '.', '#', '#', '.'), List('.', '.', '#', '.', '.', '#'), List('#', '#', '#', '#', '#', '#'))


case class RichMatrix[A](m: Matrix[A]) {
  def toStr = "\n" + m.map {
    _.map {
      "\t" + _
    }.reduceLeft(_ + _) + "\n"
  }.reduceLeft(_ + _)

  def flip1[A] = flipH(m)

  def flip2[A] = flipV(m)

  def rotate[A] = m.transpose

  def split(n: Int): Seq[Matrix[A]] = m.grouped(n).toList.flatMap(g => g.transpose.grouped(n).map(_.transpose).toList)

  def combine(m2: Matrix[A]): Matrix[A] = {
    if (m.size != m2.size) throw new IllegalArgumentException("Cannot combine " + m + " and  " + m2)
    else {
      m.zipWithIndex.map(p => p._1 ::: m2(p._2))
    }
  }

  def count(a : A) : Int = m.flatMap(r => r.filter(x => x.equals(a))).size

}

implicit def pimp[A](m: Matrix[A]) = new RichMatrix(m)

def combineBy[A](matrices: Seq[Matrix[A]], by: Int): Matrix[A] = {
  matrices.grouped(by).map(g => g.reduce((x, y) => x.combine(y))).toList.flatten
}

def allCombination[A](m: Matrix[A]): Seq[Matrix[A]] = List(
  m, m.flip1, m.flip2, m.flip1.flip2, m.transpose, m.transpose.flip1, m.transpose.flip2, m.transpose.flip1.flip2, m.transpose.flip2.flip1, m.flip1.transpose, m.flip2.transpose
)

//allCombination(m1).foreach(m => println(m.toStr))

type Picture = Matrix[Char]

case class Rule(pattern: Picture, output: Picture) {
  override def toString = pattern.toString() + " => " + output.toString()
}

def parseRule(s: String): Rule = {
  val split = s.split("=>").map(_.trim).map(parse)
  Rule(split(0), split(1))
}


def parse(s: String): Picture = s.split("/").toList.map(r => r.toCharArray.toList)

def applyFirstMatch(picture: Picture, rules: Seq[Rule]): Picture = {
  val o = rules.find(r => allCombination(r.pattern).contains(picture)).map(_.output)
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
      val x = p.size / 2
      val split = p.split(2).map(m => applyFirstMatch(m, rules))
      State(combineBy(split, 2), rules)
    } else if (p.size % 3 == 0) {
      println("split by 3")
      val x = p.size / 3
      val split = p.split(3).map(m => applyFirstMatch(m, rules))
      State(combineBy(split, 3), rules)
    } else {
      throw new IllegalArgumentException("size " + p.size)
    }
  }
}


val test = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"

val rules = test.split("\n").map(parseRule)

rules.map(r => r.pattern == m1)

m1.split(3)


m1.flatten.count(x => x.equals('#'))

State(m1, rules).next().next().p.count('#')

val cesar = "../.. => .../.../..#\n#./.. => #.#/..#/...\n##/.. => #.#/..#/#.#\n.#/#. => #../.../.##\n##/#. => ###/#.#/..#\n##/## => #.#/.../#..\n.../.../... => #..#/..../.##./....\n#../.../... => ..../.##./#.../.##.\n.#./.../... => .#../####/..##/#...\n##./.../... => ##.#/..#./####/...#\n#.#/.../... => ##.#/##../#.#./.#..\n###/.../... => #..#/#..#/##../##.#\n.#./#../... => #.##/##../.#.#/..##\n##./#../... => #.#./..../.###/.#.#\n..#/#../... => ..##/####/..##/....\n#.#/#../... => ..##/###./..##/#...\n.##/#../... => #.../####/#..#/##..\n###/#../... => ...#/..../..##/#...\n.../.#./... => ##../##../..##/....\n#../.#./... => #.../.#.#/.##./#..#\n.#./.#./... => ..##/#.../...#/###.\n##./.#./... => ####/.#.#/..##/####\n#.#/.#./... => ####/.#../#.##/#..#\n###/.#./... => ..#./#..#/.#.#/###.\n.#./##./... => ##../.#.#/#..#/#..#\n##./##./... => .###/####/#..#/..##\n..#/##./... => ###./.#../..#./#.##\n#.#/##./... => ##../#.#./#.../.#.#\n.##/##./... => #.../#.../.#.#/####\n###/##./... => .#../####/#.../#.#.\n.../#.#/... => .#../..../##../.###\n#../#.#/... => .##./...#/.###/...#\n.#./#.#/... => ...#/#.../...#/####\n##./#.#/... => #.##/..#./#..#/.#.#\n#.#/#.#/... => #..#/..../..##/..#.\n###/#.#/... => .#.#/#.#./##.#/#.#.\n.../###/... => ##../.##./###./###.\n#../###/... => ###./..##/.#../##.#\n.#./###/... => .#../##../..../..##\n##./###/... => #.#./...#/...#/##..\n#.#/###/... => ..../.#../#.../.#..\n###/###/... => ..#./.###/..../##.#\n..#/.../#.. => #.#./.#../...#/##.#\n#.#/.../#.. => ...#/##.#/#.#./#...\n.##/.../#.. => ...#/..##/#.##/##.#\n###/.../#.. => #..#/.#.#/.##./..#.\n.##/#../#.. => ##../..#./#.##/##..\n###/#../#.. => ..../###./#.#./##..\n..#/.#./#.. => #.#./.##./.##./#...\n#.#/.#./#.. => .#../#..#/#.#./#...\n.##/.#./#.. => .#.#/#..#/..#./....\n###/.#./#.. => #.##/####/#.../..#.\n.##/##./#.. => #.##/.#.#/..../.#..\n###/##./#.. => #.##/####/.###/##..\n#../..#/#.. => ###./#.##/..#./..##\n.#./..#/#.. => ##../.#../..#./..##\n##./..#/#.. => #..#/.#../..../##.#\n#.#/..#/#.. => .###/.##./..#./#.#.\n.##/..#/#.. => .#.#/..../####/.#..\n###/..#/#.. => .##./##../...#/.#..\n#../#.#/#.. => #.#./#.##/..../.###\n.#./#.#/#.. => ####/#.#./.#../#.##\n##./#.#/#.. => ..##/.###/###./..#.\n..#/#.#/#.. => .##./..#./..../#.#.\n#.#/#.#/#.. => .###/..../..../##..\n.##/#.#/#.. => #.#./#.../####/.###\n###/#.#/#.. => #.../..##/###./#..#\n#../.##/#.. => ..../#.#./..##/.#.#\n.#./.##/#.. => ..##/..##/#..#/###.\n##./.##/#.. => #.../.#../#.#./#.##\n#.#/.##/#.. => ...#/#.../...#/###.\n.##/.##/#.. => ###./..../..##/#..#\n###/.##/#.. => #.#./##.#/####/#.#.\n#../###/#.. => ##../##../###./#..#\n.#./###/#.. => #.##/###./####/..##\n##./###/#.. => ..../.###/###./.#..\n..#/###/#.. => .###/..../..#./....\n#.#/###/#.. => ####/#..#/.#.#/..##\n.##/###/#.. => ..../##.#/####/##.#\n###/###/#.. => #..#/.#.#/###./.##.\n.#./#.#/.#. => #.##/...#/###./....\n##./#.#/.#. => #..#/.#../..../#.#.\n#.#/#.#/.#. => .#.#/####/..../.#.#\n###/#.#/.#. => #.#./#.##/##.#/##..\n.#./###/.#. => ..#./..../##../####\n##./###/.#. => #.##/##.#/#.##/.#..\n#.#/###/.#. => .#.#/..##/##.#/####\n###/###/.#. => .#../...#/#..#/#.#.\n#.#/..#/##. => .##./..#./...#/##.#\n###/..#/##. => ..#./##.#/#..#/#..#\n.##/#.#/##. => ##.#/#.../#..#/...#\n###/#.#/##. => ##../.#../..../.##.\n#.#/.##/##. => #.##/##.#/.#../.###\n###/.##/##. => ..../#.#./##../##.#\n.##/###/##. => ###./.#.#/.##./.###\n###/###/##. => #..#/.###/#.../#...\n#.#/.../#.# => .###/#.##/.#.#/#.#.\n###/.../#.# => ...#/##../...#/##.#\n###/#../#.# => ..../..#./..#./####\n#.#/.#./#.# => ##../#.##/...#/#...\n###/.#./#.# => #.#./...#/.#../#...\n###/##./#.# => .#../..#./...#/##..\n#.#/#.#/#.# => ####/#.##/.#../##..\n###/#.#/#.# => #.../#.../###./.#..\n#.#/###/#.# => ####/.#.#/.##./.#.#\n###/###/#.# => #.##/.#.#/##.#/..##\n###/#.#/### => .###/#.##/..../..#.\n###/###/### => .###/#..#/##../.##."

val cesarRules = cesar.split("\n").map(parseRule)


@tailrec
def solve(state: State, n: Int) : State = if(n == 0) state else solve(state.next(), n -1)

val s =solve(State(m1, cesarRules), 18)
//s.p.toStr
s.p.count('#')














