import java.io._

import scala.io.Source
import scala.util.matching.Regex

class TestMarkdown {

  val HtmlCode = Map(
  "!" -> "&#33;",

  "\"" -> "&quot;",

  "#" -> "&#35;",

  "##" -> "&#35;&#35;",

  "###" -> "&#35;&#35;&#35;",

  "####" -> "&#35;&#35;&#35;&#35;&#35;",

  "#####" -> "&#35;&#35;&#35;&#35;&#35;&#35;",

  "######" -> "&#35;&#35;&#35;&#35;&#35;&#35;&#35;",

  "*" -> "&#42;",

  "**" -> "&#42;&#42;",

  "-" -> "&#45;",

  "----" -> "&#45;&#45;&#45;&#45;",

  ">" -> "&#62;",

  "_" -> "&#95;",

  "`" -> "&#96;",

  "~" -> "&#126;",

  "[" -> "&#91;",

  "]" -> "&#93;",

  "(" -> "&#40;",

  ")" -> "&#41;"

 )

  def getMarkdown(str: String):String ={
    var input = str

    input = this.replaceMail(input)
    input = this.replaceUrl(input)
    input = this.replaceAnchor(input)
    input = this.replaceCode(input)

    input = this.replaceHeading(input)
    input = this.replaceBold(input)
    input = this.replaceBlockquote(input)
    input = this.replaceItalic(input)
    input = this.replaceParagraph(input)
    input = this.replaceStrike(input)
    input = this.replaceUnderline(input)
    input = this.replaceHR(input)
    input = this.replaceBR(input)
    input = this.replaceImage(input)
    input = this.replaceList(input)
    input = this.replaceOptionalTags(input)

   input

  }

  def getPlainText(str: String, regex: Regex, marker1: String, marker2: String, groupNo: Int): String = {
    var input = str

    regex.findAllIn(str).matchData.foreach((m) => {
      val markedText = if(m.group(groupNo).trim().length != 0) {
        s"$marker1${m.group(groupNo).trim()}$marker2"
      } else {
        ""
      }
      input = input.replaceFirst(regex.toString(), markedText)
    })
    input
  }



  def setHtmlcode(str: String): String = {
    var input = str
    var regex1 = ""
  }

  def replaceBold(str: String): String = {
    val regex = new Regex("<(b|strong)>(.*?)</\\1>")
    val marker1 = HtmlCode("**")
    val marker2 = HtmlCode("**")

    this.getPlainText(str,regex,marker1,marker2,2)
  }

  def replaceItalic(str: String): String ={
    var input = str
    val regex = new Regex("<(em|italic)>(.*?)</\\1>")
    val marker1 = HtmlCode("-")
    val marker2 = HtmlCode("-")

    this.getPlainText(str,regex,marker1,marker2,2)
  }

  def replaceUnderline(str: String): String ={
    var input = str
    val regex = new Regex("<(?:u)>(.*?)</(?:u)>")
    val marker1 = HtmlCode("_")
    val marker2 = HtmlCode("_")

    this.getPlainText(str,regex,marker1,marker2,1)
  }

  def replaceStrike(str: String): String ={
    var input = str
    val regex = new Regex("<(del|strike)>(.*?)</\\1>")
    val marker1 = HtmlCode("~")
    val marker2 = HtmlCode("~")

    this.getPlainText(str,regex,marker1,marker2,2)
  }

  def replaceCode(str: String): String ={
    var input = str
    val regex = new Regex("<code>(.*?)</code>")
    val marker1 = HtmlCode("`")
    val marker2 = HtmlCode("`")

    this.getPlainText(str,regex,marker1,marker2,1)
  }

  def replaceBlockquote(str: String): String ={
    var input = str
    val regex = new Regex("(?s)<blockquote>(.*?)</blockquote>")
    val marker1 = HtmlCode(">")
    val marker2 = "\n"

    this.getPlainText(str,regex,marker1,marker2,1)
  }

  def replaceParagraph(str: String): String ={
    var input = str
    val regex = new Regex("(?s)<p\\s?(?:.*?)>(.*?)</p>")
    val marker1 = ""
    val marker2 = "\n"

    this.getPlainText(str,regex,marker1,marker2,1)
  }

  def replaceHeading(str: String): String ={
    var input = str
    val regex = new Regex("<(h[1-6])\\s?(?:.*?)>(.*?)</h[1-6]>")

    regex.findAllIn(str).matchData.foreach(m=>{
      if(m.group(2).length != 0) {

        m.group(1) match {
          case "h1" => input = input.replaceFirst(regex.toString(), s"${HtmlCode("#")} ${m.group(2)}\n")
          case "h2" => input = input.replaceFirst(regex.toString(), s"${HtmlCode("##")} ${m.group(2)}\n")
          case "h3" => input = input.replaceFirst(regex.toString(), s"${HtmlCode("###")} ${m.group(2)}\n")
          case "h4" => input = input.replaceFirst(regex.toString(), s"${HtmlCode("####")} ${m.group(2)}\n")
          case "h5" => input = input.replaceFirst(regex.toString(), s"${HtmlCode("#####")} ${m.group(2)}\n")
          case "h6" => input = input.replaceFirst(regex.toString(), s"${HtmlCode("######")} ${m.group(2)}\n")
        }

      }else{
        input = input.replaceFirst(regex.toString(), "")
      }
    })
    input
  }

  def replaceHR(str: String): String ={
    var input = str
    var regex = new Regex("<hr(|\\s?/)>")

    regex.replaceAllIn(input,HtmlCode("----"))
  }

  def replaceBR(str: String): String ={
    var input = str
    val regex = new Regex("<br(|\\s?/)>")

    regex.replaceAllIn(input,"")
  }

  def replaceOptionalTags(str: String): String ={
    var input = str
    val regex = new Regex("<.*?>")

    regex.replaceAllIn(input,"")
  }

  def getImgSrc(str: String): String ={
    var input = str
    val regex = new Regex(" src\\s?=\\s?[\"'](.*?)[\"']")

    val src = if(!regex.findFirstMatchIn(input).isEmpty) {
      regex.findFirstMatchIn(input).get.group(1)
    }else {
      ""
    }
    src
  }

  def getImgAlt(str: String): String ={
    var input = str
    val regex = new Regex(" alt\\s?=\\s?[\"'](.*?)[\"']")

    val alt = if(!regex.findFirstMatchIn(input).isEmpty) {
      regex.findFirstMatchIn(input).get.group(1)
    }else {
      ""
    }
    alt
  }

  def replaceImage(str: String): String ={
    var input = str
    val regex = new Regex("<img(.*?)\\s?/?>")

    regex.findAllIn(input).matchData.foreach( (m) => {
      if(m.group(1).length != 0) {
        val syntax = s"${HtmlCode("!")}${HtmlCode("[")}${this.getImgAlt(m.group(1))}${HtmlCode("]")}${HtmlCode("(")}${this.getImgSrc(m.group(1))}${HtmlCode(")")}"
        input = input.replaceFirst(regex.toString(), syntax)
      }else{
        input = input.replaceFirst(regex.toString(), "")
      }
    })
    input
  }

  def getHref(str: String): String ={
    var input = str
    val regex = new Regex("href\\s?=\\s?[\"'](.*?)[\"']")

    val href = if(!regex.findFirstMatchIn(input).isEmpty) {
      regex.findFirstMatchIn(input).get.group(1)
    }else {
      ""
    }
    href
  }

  def replaceAnchor(str: String): String ={
    var input = str
    val regex = new Regex("(?s)<a (.*?)\\s?>(.*?)</a>")

    regex.findAllIn(input).matchData.foreach((m) => {
      if(m.group(2).length != 0) {
        val syntax = s"${HtmlCode("[")}${this.getHref(m.group(1))}${HtmlCode("]")}${HtmlCode("(")}${m.group(2)}${HtmlCode(")")}"
        input = input.replaceFirst(regex.toString(), syntax)
      }else{
        input = input.replaceFirst(regex.toString(), "")
      }
    })
    input
  }

  def replaceMail(str: String): String ={
    var input = str
    val regex = new Regex("([a-zA-Z0-9._-]+@[a-zA-Z0-9-]+\\.[a-zA-Z]+)")

    regex.findAllIn(input).matchData.foreach((m) => {
      val syntax =  s"${HtmlCode("[")}${m.group(0)}${HtmlCode("]")}${HtmlCode("(")}${m.group(0)}${HtmlCode(")")}"
      input = input.replaceFirst(m.group(0), syntax)
    })
    input
  }

  def replaceUrl(str: String): String ={
    var input = str
    val regex = new Regex("((https?://)?(www.)[^\"<\\s]+)(?![^<>]*>|[^\"]*?<\\/a)")

    regex.findAllIn(input).matchData.foreach( (m) => {
      val syntax = s"${HtmlCode("[")}${m.group(0)}${HtmlCode("]")}${HtmlCode("(")}${m.group(0)}${HtmlCode(")")}"
      input = input.replaceFirst(m.group(0), syntax)
    })
    input
  }

  def replaceList(str: String): String ={
    var input = str
    val regex = new Regex("(?s)<(u|o)l\\s?(?:.*?)>(.*?)</(?:u|o)l>")

    regex.findAllIn(input).matchData.foreach((m) => {
      if(m.group(2).length != 0) {
        input = input.replaceFirst(regex.toString(),replaceLi(m.group(1),m.group(2))+"\n")
      } else {
        input = input.replaceFirst(regex.toString(), "")
      }
    })
    input
  }

  def replaceLi(listType: String, str:String ): String ={
    var input = str
    var index = 0
    val regex = new Regex("(?s)<li\\s?(?:.*?)>(.*?)</li>")

    regex.findAllIn(input).matchData.foreach(m=>{
      if(m.group(1).length != 0) {

        if(listType.equals("u")){
          input = input.replaceFirst(regex.toString(), s"${HtmlCode("*")} ${m.group(1)}\n")
        }else if(listType.equals("o")){
          index += 1
          input = input.replaceFirst(regex.toString(), s"$index. ${m.group(1)}\n")
        }
      }else{
        input = input.replaceFirst(regex.toString(), "")
      }
    })
    input
  }






  def Dummy(str: String): String ={
    var input = str
    val regex = new Regex("")

    regex.findAllIn(str).matchData.foreach(m=>{
      if(m.group(1).length != 0) {
        input = input.replaceFirst(regex.toString(), s">${m.group(1)}")
      }else{
        input = input.replaceFirst(regex.toString(), "")
      }
    })
    input
  }




}

object Test1{
  def main(str:Array[String]): Unit ={
    val testm = new TestMarkdown()


    val input = Source.fromFile("/home/test/Desktop/testRegex.html").mkString

    val writer = new PrintWriter(new File("/home/test/Desktop/result.html"))

    writer.write(testm.getMarkdown(input))
    writer.close()

    print(testm.getMarkdown(input))

//    scalaLMarkdown.html
//    TestMark.html
//    Markdown.html
// backstageMark.html
  }
}
