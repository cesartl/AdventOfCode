val s = Stream.iterate(1)(n => {
  println("n")
  n + 1
})

val s2 = s.drop(2)

s(0)
s(1)
s(2)

s2(0)
s2(1)
s2(2)
s2(3)

s2(0)
s2(1)
s2(2)
s2(3)


val x : Option[Int] = None




