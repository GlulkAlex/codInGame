val fullFileName0 = "index.html"
val fullFileName1 = "index."
val fullFileName2 = "index"
val fullFileName3 = ""
val fullFileName4 = "report..pdf"
val fullFileName5 = "b.wav.tmp"
val fullFileName6 = ".mp3."

fullFileName2
.indexOf(".")
fullFileName3
.indexOf(".")
fullFileName4
.indexOf(".")
fullFileName4
.lastIndexOf('.')
fullFileName5
.indexOf(".")
fullFileName5
.lastIndexOf('.')
fullFileName2
.lastIndexOf('.')
fullFileName3
.lastIndexOf('.')
fullFileName6
.lastIndexOf('.')
//fullFileName5
//.scanRight()

val fileExt0 =
  fullFileName0
.drop(fullFileName0
      .indexOf("."))

val (fileName1, fileExt1) =
  fullFileName0
  //.split(".")
  /*.slice(fullFileName0
         .indexOf("."),
         fullFileName0.length
         )*/
  .splitAt(fullFileName0
           .indexOf("."))

val (fileName2, fileExt2) =
  fullFileName0
  .span(_ != '.')
val (fileName3, fileExt3) =
  fullFileName4
  .span(_ != '.')
val (fileName5, fileExt5) =
  fullFileName5
  .span(_ != '.')
  //.partition(_ != '.')

//val fileNameComponents: Array[String] =
  fullFileName0
.split(".")

fullFileName0
.split('.')
fullFileName1
.split('.')
fullFileName2
.split('.')
fullFileName3
.split('.')

fullFileName4
.split('.')
fullFileName5
.split('.')

fullFileName6
.endsWith(".")
/*fullFileName6
.endsWith('.')*/
fullFileName6
.split('.')

case class ExtensionMIME(ext: String, mimeType: String)

val extTypeMap: Array[ExtensionMIME] =
  new Array(3)//[ExtensionMIME]

extTypeMap(0) =
  ExtensionMIME(
                 ext="html",
                 mimeType="text/html")
extTypeMap(1) =
  ExtensionMIME(
                 ext="png",
                 mimeType="image/png")
extTypeMap(2) =
  ExtensionMIME(
                 ext="gif",
                 mimeType="image/gif")
//.ext = "html"
extTypeMap
extTypeMap
.find(_.ext == "gif")
extTypeMap
.find(_.ext == "txt")
extTypeMap
.find(_.ext.toLowerCase == "txt")
"""3
  |10
  |wav audio/x-wav
  |mp3 audio/mpeg
  |pdf application/pdf
  |a
  |a.wav
  |b.wav.tmp
  |test.vmp3
  |pdf
  |mp3
  |report..pdf
  |defaultwav
  |.mp3.
  |final.
  |"""
.stripMargin
.split("\n")