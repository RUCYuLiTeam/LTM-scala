(1 to 10).toArray[Int].map(_.toDouble)

val matrix = Array.ofDim[Int](4)

Array(1,2,3).zip(Array(2,1,2)).map(x => x._1*x._2).zipWithIndex
Array(1,2,3).zip(Array(2,1,2)).map(x => x._1*x._2).zipWithIndex.minBy(_._1)
Array(1,2,3).zip(Array(2,1,2)).map(x => x._1*x._2).zipWithIndex.indexOf((2,2))

val e1 = Set(1,3)
Array(Array(1,2,1),Array(1,3,2)).find(e => Set(e(0),e(1)) == e1) != None

var x = Map(1 -> 0)
x.get(0).isEmpty
