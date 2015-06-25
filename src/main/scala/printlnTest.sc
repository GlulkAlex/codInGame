val r:String = "r val"
val l:Int = 7

println(
s"""
  |1st string
  |2nd string
  |3d string${r}
""".stripMargin
       )

13+2+7