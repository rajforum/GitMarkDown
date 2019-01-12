import scala.io.Source
import scala.util.matching.Regex

class Markdown{

    val tags = Map(
      "<b>" -> "*",
      "</b>" -> "*",
      "<strong>" -> "*",
      "</strong>" -> "*",

      "<em>" -> "-",
      "</em>" -> "-",
      "<italic>" -> "-",
      "</italic>" -> "-",

      "<u>" -> "_",
      "</u>" -> "_",

      "<strike>" -> "~",
      "</strike>" -> "~",
      "<del>" -> "~",
      "<del>" -> "~",

      "<code>" -> "```",
      "</code>" -> "```",

      "<p>" -> "",
      "</p>" ->"\n",

      "<h1>" -> "#",
      "</h1>" -> "\n",
      "<h2>" -> "##",
      "</h2>" -> "\n",
      "<h3>" -> "###",
      "</h3>" -> "\n",
      "<h4>" -> "####",
      "</h4>" -> "\n",
      "<h5>" -> "#####",
      "</h5>" -> "\n",
      "<h6>" -> "######",
      "</h6>" -> "\n",

      "<blockquote>" -> ">",
      "</blockquote>" -> "\n",

      "<hr>" -> "---"
    )

  def getMarkdown(str: String): String= {
    var input = str

    input = this.getTag(input)
    input = this.getEmail(input)
    input = this.scrapUrl(input)
    input = this.scrapAnchor(input)
    return input

  }

  def getTag(str: String): String ={
    var input = str

    this.tags.keys.foreach{ key =>
      input = (key.r replaceFirstIn(input, this.tags(key)))
    }
    return input
  }

  def getEmail(str: String): String= {
    val regex = new Regex("([a-zA-Z0-9._-]+@[a-zA-Z0-9-]+\\.[a-zA-Z]+)")
    return regex.replaceFirstIn(str,"mailto:$1")
  }

  def getUrl(str: String): String ={
    val regex = new Regex("((https?://)?(www.)[^\"<\\s]+)(?![^<>]*>|[^\"]*?<\\/a)")
    return regex.replaceFirstIn(str,"$1")
  }

  def scrapAnchor(str: String): String ={
    val regex = new Regex("<a href=\"(.+)\">(.+)</a>")
    return regex.replaceFirstIn(str,"[$2]($1)")
  }

  def scrapUrl(str: String): String ={
    val regex = new Regex("((https?://)?(www.)[^\"<\\s]+)(?![^<>]*>|[^\"]*?<\\/a)")
    return regex.replaceFirstIn(str,"[$1]($1)")
  }

}


object Test{
  def main(args: Array[String]):Unit = {
   val test = new Markdown()

    var input = Source.fromFile("/home/test/Markdown.html").mkString


//    var input= "<b>Bold</b><em>EM</em><strike>Strike</strike><code><strong>Hi</strong> I am <h1>code</h1> block</code>\n" +
//      "rajk@gmail.com \n " +
//      "<a href=\"www.youtube.com\">Youtube</a> \n" +
//      "http://www.eventz.com"

    println(test.getMarkdown(input))



//   test.isEmail()
   // test.scrapUrl("www.goodfgle.com")

    //test.scrapAnchor("<a href=\"www.google.com\">goole</a>")
  }
}

