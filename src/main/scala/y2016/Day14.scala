package y2016

import java.security.MessageDigest

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day14 extends App {


  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02X".format(_).toLowerCase).mkString
  }

  case class Key(k: String, regex: Regex, iteration: Long)

  def keyPattern = """([\w])\1{2}""".r

  def generateKey(n: Long, salt: String): String = md5(salt + n)

  @tailrec
  def generateKeys(n: Long, candidates: Seq[Key], found: Seq[Key], salt: String, numberOfKeys: Int): Seq[Key] = {
    println(n)
    if (found.lengthCompare(numberOfKeys) >= 0) found else {
      val nextKey = generateKey(n, salt)
      val (newCandidates, newFound) = candidates.filter { case Key(_, _, it) => it + 1000 > n }.foldLeft((List[Key](), found)) {
        case ((candidateAcc, foundAcc), Key(k, regex, it)) => {
          if (regex.findFirstMatchIn(nextKey).isDefined) {
            println(s"Match $k($it) with $nextKey with regex $regex")
            (candidateAcc, foundAcc :+ Key(k, regex, it))
          }
          else (candidateAcc :+ Key(k, regex, it), foundAcc)
        }
      }

      val matchOption = keyPattern.findFirstMatchIn(nextKey)

      val keyOption = matchOption.map(p => {
        println("found match")
        val pattern = new Regex(p.group(1) + "{5}")
        Key(nextKey, pattern, n)
      }).map(List(_))
      println(keyOption)
      generateKeys(n + 1, newCandidates ++ keyOption.getOrElse(List()), newFound, salt, numberOfKeys)
    }
  }

  def solve1(salt: String, numberOfKeys: Int): Seq[Key] = {
    generateKeys(0, List(), List(), salt, numberOfKeys)
  }

  override def main(args: Array[String]): Unit = {
    println(solve1("abc", 1))
  }
}
