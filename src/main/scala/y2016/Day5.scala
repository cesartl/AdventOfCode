package y2016

import java.security.MessageDigest

import scala.annotation.tailrec

object Day5 extends App {

  def md5(s: String) = {
    //  MessageDigest.getInstance("MD5").digest(s.getBytes).map(0xFF & _).map {
    //    "%02x".format(_)
    //  }.foldLeft("") {
    //    _ + _
    //  }
    MessageDigest.getInstance("MD5").digest(s.getBytes)
  }

  @tailrec
  def solve2(input: String, i: Long, password: Map[Int, Char], length: Int): String = {
    if (password.size == length) password.toSeq.sorted.map(_._2).mkString else {
      val hash = md5(input + i)
      if (hash(0) == 0 && hash(1) == 0 && hash(2) < 16 && hash(2) >= 0) {
//        println(i)
        val easy = hash.map(0xFF & _).map {
          "%02x".format(_)
        }.foldLeft("") {
          _ + _
        }
        val p = easy(5)
        if (p.isDigit && p.asDigit >= 0 && p.asDigit < length && !password.contains(p.asDigit)) {
          val map = password + (p.asDigit -> easy(6))
          println((0 until 8).map(i => map.getOrElse(i, "_")).mkString)
          solve2(input, i + 1, map, length)
        } else {
//          println("no")
          solve2(input, i + 1, password, length)
        }
      } else solve2(input, i + 1, password, length)
    }
  }


  override def main(args: Array[String]): Unit = {
    solve2("abbhdwsy", 0, Map(), 8)
  }
}
