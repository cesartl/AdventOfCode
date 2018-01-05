import scala.annotation.tailrec
import scala.io.Source

def findAbba(s: String): Option[String] = {
  if (s.length < 4) None
  else if (s(0) != s(1) && s(0) == s(3) && s(1) == s(2)) {
    Some(s.substring(0, 4))
  } else {
    findAbba(s.substring(1))
  }
}

val t1 = "abba[mnop]qrst"
val t2 = "abcd[bddb]xyyx"
val t3 = "aaaa[qwer]tyui"

val regex = """\[[\w]*\]"""

def solve1(s: String): Boolean = {
  val blocks = s.split("\\[[\\w]*\\]").toSeq
  val hypernets = regex.r.findAllMatchIn(s).map(_.group(0)).toSeq
  hypernets.forall(s => findAbba(s).isEmpty) && blocks.exists(s => findAbba(s).isDefined)
}

val cesar = Source.fromFile("/Users/Cesar/Day7.txt").getLines()
//cesar.map(solve1).count(x => x)

def findAba(input: String): Seq[String] = {
  @tailrec
  def loop(s: String, abas: Seq[String]): Seq[String] = {
    if (s.length < 3) abas
    else if (s(0) != s(1) && s(0) == s(2)) loop(s.substring(1), s.substring(0, 3) +: abas)
    else loop(s.substring(1), abas)
  }

  loop(input, List())
}

def abaToBab(s: String): String = {
  if (s.length != 3 || !(s(0) != s(1) && s(0) == s(2))) throw new IllegalArgumentException(s + " not a ABA")
  else List(s(1), s(0), s(1)).mkString
}


def solve2(s: String) : Boolean = {
  val blocks = s.split("\\[[\\w]*\\]").toSeq
  val hypernets = regex.r.findAllMatchIn(s).map(_.group(0)).toSeq

  val abas = blocks.flatMap(findAba)
  val babs = hypernets.flatMap(findAba).map(abaToBab).toSet

  abas.exists(aba => babs.contains(aba))
}

solve2("aba[bab]xyz")
solve2("xyx[xyx]xyx")
solve2("aaa[kek]eke")
solve2("zazbz[bzb]cdb")

cesar.map(solve2).count(x => x)

