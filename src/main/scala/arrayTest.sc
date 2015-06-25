var mountains: Array[Int] = Array.emptyIntArray
var mountains2: Array[Int] = (0 to 9).map(_ => 0).toArray
val mountains3: Array[Int] = new Array(10)

Array.tabulate(10)(_=>0)
mountains
mountains.size
mountains.isEmpty
mountains = mountains.union(Array(1))
mountains = mountains :+ 2
mountains3.update(0,1)
mountains3
mountains3(9)=10
mountains3
mountains3.max
