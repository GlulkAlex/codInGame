val sourceList =
  //(1 to 10 by 2).toList
  (1 to 11 by 2).toList
  //List()

val pairsFromSourceIter =
  sourceList
  .sliding(2)

def nextPair: List[Int] =
  if (pairsFromSourceIter.hasNext) {
    pairsFromSourceIter.next
  } else {
    List()
  }

nextPair
nextPair
nextPair
nextPair
nextPair

val horsesArray: Array[Int] =
  new Array[Int](4)