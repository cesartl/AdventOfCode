import scala.annotation.tailrec

def doStep(arr: Seq[Int], steps: Int, insert: Int, pos: Int) = {
  if (arr.lengthCompare(1) == 0) (arr ++ Array(insert), 1) else {
    val newPos = (pos + steps + 1) % arr.length
    val (front, back) = arr.splitAt(newPos)
    //    println(front)
    //    println(back)

    val newArr = front ++ Array(insert) ++ back
    (newArr, newPos)
  }
}

@tailrec
def spinlock(arr: Seq[Int], steps: Int, insert: Int, pos: Int, count: Int): (Seq[Int], Int) = {
  println(arr.toList)
  println("pos: " + pos)
  val foo = doStep(arr, steps, insert, pos)
  if (count == 0) (arr, pos) else spinlock(foo._1, steps, insert + 1, foo._2, count - 1)
}

@tailrec
def solve2(afterZero: Int, steps: Int, insert: Int, pos: Int, length: Int, checkAt: Int, count: Int): (Int, Int) = {
  if (count == 0) (afterZero, insert) else {
    val newPos = (pos + steps + 1) % length
    if (newPos < checkAt) {
      solve2(afterZero, steps, insert + 1, newPos, length + 1, checkAt + 1, count - 1)
    } else if (newPos == checkAt) {
      solve2(insert, steps, insert + 1, newPos, length + 1, checkAt, count - 1)
    } else {
      solve2(afterZero, steps, insert + 1, newPos, length + 1, checkAt, count - 1)
    }
  }
}

doStep(List(0).toArray, 3, 1, 0)._1.toList

//val y = spinlock(Array(0), 328, 1, 0, 5)
//x._1(x._2 +1)


val x = spinlock(Array(0), 327, 1, 0, 25)

//solve2(1, 3, 2, 1, 2, 4)
//solve2(1, 3, 2, 1, 2, 5)
//solve2(1, 3, 2, 1, 2, 6)
//solve2(1, 3, 2, 1, 2, 7)
//solve2(1, 3, 2, 1, 2, 8)


//solve2(1, 328, 3, 0, 3, 2, 1)
//solve2(1, 328, 3, 0, 3, 2, 2)
//solve2(1, 328, 3, 0, 3, 2, 3)
//solve2(1, 328, 3, 0, 3, 2, 4)
//solve2(1, 328, 3, 0, 3, 2, 5)
//solve2(1, 328, 3, 0, 3, 2, 6)
//solve2(1, 328, 3, 0, 3, 2, 7)
//solve2(1, 328, 3, 0, 3, 2, 8)
//solve2(1, 328, 3, 0, 3, 2, 9)
//solve2(1, 328, 3, 0, 3, 2, 10)
//solve2(1, 328, 3, 0, 3, 2, 11)
//solve2(1, 328, 3, 0, 3, 2, 12)
//solve2(1, 328, 3, 0, 3, 2, 13)
//solve2(1, 328, 3, 0, 3, 2, 14)
//solve2(1, 328, 3, 0, 3, 2, 15)
//solve2(1, 328, 3, 0, 3, 2, 16)
//solve2(1, 328, 3, 0, 3, 2, 17)
//solve2(1, 328, 3, 0, 3, 2, 18)
//solve2(1, 328, 3, 0, 3, 2, 19)
solve2(1, 328, 3, 0, 3,2, 50000000 - 3)



