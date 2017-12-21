val example  = List(0, 2, 7, 0)
val cesar = "14\t0\t15\t12\t11\t11\t3\t5\t1\t6\t8\t4\t9\t1\t8\t4"

def findMax(l : Seq[Int]) : (Int, Int) = {
  var max = (-1, -1)
  l.zipWithIndex.foreach(t => {
    if (t._1 > max._1) {
      max = t;
    }
  })
  max
}

findMax(List(3,1,2,3))



def balance(l : Seq[Int]) = {
  val buffer = l.toBuffer
  val max = findMax(l)
  buffer(max._2) = 0
  for(i <- 1 to max._1){
    buffer((max._2 + i) % buffer.length) = buffer((max._2 + i) % buffer.size) + 1
  }
  buffer.toList
}

def solve(l : Seq[Int]) = {
  val map = scala.collection.mutable.Map[Int, Int]()
  var array = l
  var count = 0

  while(!map.contains(array.hashCode)){
    map.put(array.hashCode, count)
    array = balance(array)
    count += 1
  }
  (count, count - map.get(array.hashCode).getOrElse(0))
}

solve(example)

solve(cesar.split("\t").map(Integer.parseInt))
















