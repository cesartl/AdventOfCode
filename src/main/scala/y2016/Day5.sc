import java.security.MessageDigest

import scala.annotation.tailrec

def md5(s: String) = {
  //  MessageDigest.getInstance("MD5").digest(s.getBytes).map(0xFF & _).map {
  //    "%02x".format(_)
  //  }.foldLeft("") {
  //    _ + _
  //  }
  MessageDigest.getInstance("MD5").digest(s.getBytes)
}

md5("abc168952").map(_.toHexString)
md5("abc1333069")
md5("abc3231929")
md5("abc5017308")
md5("abc5278568")


//
@tailrec
def solve1(input: String, i: Long, password: Seq[Char]): Seq[Char] = {
  if (password.lengthCompare(8) == 0) password else {
    val hash = md5(input + i)
    if (hash(0) == 0 && hash(1) == 0 && hash(2) < 16 && hash(2) >= 0) {
      println(i)
      solve1(input, i + 1, hash(2).toHexString(0) +: password)
    } else solve1(input, i + 1, password)
  }
}

//solve1("abbhdwsy", 0, List()).reverse.mkString


@tailrec
def solve2(input: String, i: Long, password: Map[Int, Char], length: Int): String = {
  if (password.size == length) password.toSeq.sorted.map(_._2).mkString else {
    val hash = md5(input + i)
    if (hash(0) == 0 && hash(1) == 0 && hash(2) < 16 && hash(2) >= 0) {
      println(i)
      val easy = hash.map(0xFF & _).map {
        "%02x".format(_)
      }.foldLeft("") {
        _ + _
      }
      println(easy)
      println(easy(5))
      if (easy(5).isDigit && easy(5).toInt >= 0 && easy(5).toInt < length) {
        val map = password + (easy(5).toInt -> easy(6))
        println((0 until 8).map(i => map.getOrElse(i, "_")).mkString)
        solve2(input, i + 1, map, length)
      } else {
        println("no")
        solve2(input, i + 1, password, length)
      }
    } else solve2(input, i + 1, password, length)
  }
}




