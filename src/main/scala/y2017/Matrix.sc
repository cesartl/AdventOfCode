type Row = List[Double]

type Matrix = List[Row]

def dotProd(v1: Row, v2: Row) =
  v1.zip(v2).
    map { t: (Double, Double) => Math.round(t._1 * t._2).toDouble }.sum

def transpose(m: Matrix): Matrix =
  if (m.head.isEmpty) Nil
  else m.map(_.head) :: transpose(m.map(_.tail))

def mXm(m1: Matrix, m2: Matrix) =
  for (m1row <- m1) yield
    for (m2col <- transpose(m2)) yield
      dotProd(m1row, m2col)

case class RichMatrix(m: Matrix) {

  def T = transpose(m)

  def *(that: RichMatrix) = mXm(this.m, that.m)

  def toStr = "\n"+m.map{
    _.map{"\t" + _}.reduceLeft(_ + _)+"\n"
  }.reduceLeft(_ + _)

}

implicit def pimp(m: Matrix) = RichMatrix(m)


val m1: Matrix = List(List(0, 1, 0), List(0, 0, 1), List(1, 1, 1))


def rotate(a: Double): Matrix = List(List(math.cos(a), -math.sin(a), 0), List(math.sin(a), math.cos(a), 0), List(0, 0, 1))

val m2: Matrix = m1 * rotate(math.Pi / 2)


m2.toStr




