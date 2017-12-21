package y2017

import y2017.Day13Bis.{doNotGetCaught, parse}

object Day15 extends App {
  def N: Long = 2147483647L;

  def A: Long = 16807L;

  def B: Long = 48271L;

  def generate(seed: Long, factor: Long): Long = (seed * factor) % N


  def generatePicky(seed: Long, factor: Long, check: Long): Long = {
    var value = generate(seed, factor)
    while (value % check != 0) {
      value = generate(value, factor)
    }
    value
  }

//  def hash(l: Long) = {
//    val foo = l.toBinaryString.pre(32, 0).drop(16)
//    foo
//    //    foo.substring(foo.length-16, foo.length)
//  }

//  def compareTail(startA: Long, startB: Long, count: Long, total: Long, judged: Long): Long = {
//    if(count == total) judged
//    else {
//      val a = generate(startA, A)
//      val b = generate(startB, B)
//      if (((a ^ b) & 0x0000FFFF) == 0){
//
//      }
//    }
//  }

  def compare(startA: Long, startB: Long, total: Long): Long = {
    var a = startA
    var b = startB

    var count = 0L
    for (i <- 0L until total) {
      a = generate(a, A)
      b = generate(b, B)

      if (((a ^ b) & 0xFFFF) == 0){
        count += 1
      }
    }
    count
  }

  def compare2(startA: Long, startB: Long, total: Long): Long = {
    var a = startA
    var b = startB

    var count = 0L
    for (i <- 0L until total) {
      a = generatePicky(a, A, 4)
      b = generatePicky(b, B, 8)
      //      println(a)
      //      println(b)
      //      println()
      if (((a ^ b) & 0x0000FFFF) == 0){
        count += 1
      }
    }
    count
  }


  override def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis();
    println(compare(289, 629, 40000000L))
    val mid = System.currentTimeMillis();
    println(mid - start)
    println(compare2(289, 629, 5000000))
    val end = System.currentTimeMillis();
    println(end - mid)
  }

}
