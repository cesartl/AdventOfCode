package y2016

import java.security.MessageDigest

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

object Day14 extends App {


  lazy val digest =  MessageDigest.getInstance("MD5")

  def md5(s: String) = {
    digest.digest(s.getBytes).map("%02X".format(_).toLowerCase).mkString
  }

  case class Cache[K, V](gen: K => V) {

    private[this] val map: mutable.Map[K, V] = mutable.Map[K, V]()
    private[this] var hit: Long = 0
    private[this] var miss: Long = 0

    def get(k: K): V = {
      val o = map.get(k)
      if (o.isDefined) {
        println("Hit!")
        printStat()
        hit += 1
        o.get
      } else {
        miss += 1
        val v = gen(k)
        map += (k -> v)
        v
      }
    }

    def printStat(): Unit = {
      val p: Double = hit.toDouble * 100 / (hit.toDouble + miss.toDouble)
      println(s"$p%")
    }
  }

  lazy val md5Cache: Cache[String, String] = Cache(md5)

  case class Key(k: String, iteration: Long)

  lazy val keyPattern = """([\w])\1{2}""".r

  def generateKey(n: Long, salt: String): Key = {
    //    println(s"generating key for $n")
    Key(md5(salt + n), n)
  }

  def generateStretchKey(n: Long, salt: String): Key = {
    val stream = Stream.iterate(generateKey(n, salt)) { case Key(k, it) => Key(md5(k), it) }
    stream.take(2017).last
  }

  @tailrec
  def generateKeys(valid: Seq[Key], keyStream: Stream[Key], numberOfKeys: Int): Seq[Key] = {
    if (valid.lengthCompare(numberOfKeys) >= 0) valid else {
      val nextKey = keyStream.head
      val matchOption = keyPattern.findFirstMatchIn(nextKey.k)

      val newValid = valid ++ matchOption.flatMap(r => {
        val pattern = new Regex(r.group(1) + "{5}")
        keyStream.slice(1, 1001).find { case Key(k, _) => pattern.findFirstMatchIn(k).isDefined }
      }
      ).map(_ => nextKey).toList
      if (newValid.lengthCompare(valid.length) > 0) println("found key at " + nextKey.iteration)
      generateKeys(newValid, keyStream.drop(1), numberOfKeys)
    }
  }

  def solve1(salt: String, numberOfKeys: Int): Seq[Key] = {
    val keyStream: Stream[Key] = Stream.iterate(generateKey(0, salt)) { case Key(_, it) => generateKey(it + 1, salt) }
    generateKeys(List(), keyStream, numberOfKeys)
  }

  def solve2(salt: String, numberOfKeys: Int): Seq[Key] = {
    val keyStream: Stream[Key] = Stream.iterate(generateStretchKey(0, salt)) { case Key(_, it) => generateStretchKey(it + 1, salt) }
    generateKeys(List(), keyStream, numberOfKeys)
  }

  override def main(args: Array[String]): Unit = {
    //        println(solve1("abc", 64))
        println(solve1("yjdafjpo", 64).last)
//    println(solve2("yjdafjpo", 64).last)
  }
}
