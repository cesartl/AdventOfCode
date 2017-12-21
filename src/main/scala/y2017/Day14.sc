val s = "0"

BigInt(s, 16).toString(2)

def toBinary(s: String) = s.split("").map(c => BigInt(c, 16).toString(2).reverse.padTo(4, 0).reverse.mkString("")).mkString("")

toBinary("a0c2017")


val l = 1092455L

l.toBinaryString.padTo(16, 0).mkString("")

"2".padTo(4, 0)

