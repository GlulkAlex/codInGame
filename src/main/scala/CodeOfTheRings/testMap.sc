val someMap0 = Map.empty[Char,Int]

someMap0
val someMap1 =
  someMap0.updated('A',1)
val someMap2 =
  someMap1 + ('B'->2)

someMap1.get('A')
val (key, value) =
someMap2
  .find(_._2==2)
.getOrElse(Map.empty[Char,Int])

someMap2
  .find(_._2==5) match {
  case None => Map.empty[Char,Int]
  case Some((key, value)) => (key, value)
}
/*1,2,4,5*/
Range(1, 7, step= 2 )
(1 to 5).filter((x)=>x !=3)
Seq(1,2,4,5)
  .combinations(2)
  .toList
  .mkString("|")
"-----------------".length
"++++++++++++++++++++++++++".length
"------------------".length
"+++++++++++++".length
"-------------".length
"+++++++++++++".length
"-------------".length
1.max(2).max(3)
14.min(5).min(0)
0 % 29
1 % 29
15 % 29
28 % 29
29 % 29
(29+1) % 29
(29-1) % -29
(0-1) % -29
(0-1) % 29
(30-1+15 ) % 30
(30-1+0 ) % 30
(30-1+1 ) % 30
(30-1+29 ) % 30