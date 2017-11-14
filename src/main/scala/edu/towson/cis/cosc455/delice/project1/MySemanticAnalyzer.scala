package edu.towson.cis.cosc455.delice.project1

import java.awt.Desktop
import java.io.{File, IOException, PrintWriter}
import edu.towson.cis.cosc455.delice.project1.Compiler.Parser

class MySemanticAnalyzer
{
  var htmltree = scala.collection.mutable.Stack[String]()
  var htmltreestring : String = ""
  var file_name = "project1code"
  //htmltree = Compiler.Parser.parseTree

  def getParseTreeHead() : String = {
    Parser.parseTree.head
  }

  def startSemantic () = {
    println("START SEMANTIC")
    //htmltree = htmltree.reverse
    println(Compiler.Parser.parseTree)
    htmlcode()

    //println(Compiler.Parser.parseTree)
    //println("string " + htmltree.reverse.toString()) //converts htmltree reversed to a string
    htmltreestring = htmltree.reverse.toString()
    htmltreestring = htmltreestring.replace("Stack(","")
    htmltreestring = htmltreestring.replace(",","")
    htmltreestring = htmltreestring.replace("</html>)","</html>")
    openHTMLFileInBrowser(file_name)
    //openHTMLFileInBrowser(htmltreestring)
  }

  //DOCUMENT
  // \BEGIN       = <html>
  // \END         = </html>

  //TITLE
  // \TITLE[text] = <head> <title> TEXT </title> </head>

  //HEADING
  //# text        = <h1> </h1>

  //PARAGRAPH
  //\PARAB        = <p>
  //\PARAE        = </p>

  //BOLD
  // * text *     = <b> </b>

  //UNORDERED LIST
  // + list item  = <li> </li>

  //NEW LINE
  //   //         = <br>

  //LINKS
  //[text](address)= <a href=" ADDRESS "> TEXT </a>

  //IMAGES
  //![text](address) = <img src="ADDRESS" alt="TEXT">

  private def htmlcode(): Unit ={
    println("htmlcode beginning")
    println((Compiler.Parser.parseTree.head))
    println("REVERSED HTML TREE " )
    println(htmltree.reverse)

    if (getParseTreeHead().equalsIgnoreCase(CONSTANTS.DOCB)){
      htmltree.push("<html>")
      Compiler.Parser.parseTree.pop()
      htmlcode()
    }
    else if (getParseTreeHead().equalsIgnoreCase(CONSTANTS.TITLEB)){
      htmltree.push("<head>").push("<title>")
      Compiler.Parser.parseTree.pop() //\TITLE[
      htmltree.push(Compiler.Parser.parseTree.head)
      Compiler.Parser.parseTree.pop() //The Simpsons
      //htmltree.push(Compiler.Parser.parseTree.head)
      Compiler.Parser.parseTree.pop()
      htmltree.push("</title").push("<head>")
      htmlcode()
    }
    else if (getParseTreeHead().equalsIgnoreCase(CONSTANTS.HEADING)){
      htmltree.push("<h1>")
      Compiler.Parser.parseTree.pop()
      htmltree.push(Compiler.Parser.parseTree.head)
      Compiler.Parser.parseTree.pop()
      htmltree.push("</h1>")
      htmlcode()
    }
    else if (getParseTreeHead().equalsIgnoreCase(CONSTANTS.PARAB)){
      htmltree.push("<p>")
      Compiler.Parser.parseTree.pop()
      htmlcode()
    }
    else if (getParseTreeHead().equalsIgnoreCase(CONSTANTS.LINKB)){
      htmltree.push("<a href=\" ")
      Compiler.Parser.parseTree.pop()
      htmltree.push(Compiler.Parser.parseTree.tail.tail.tail.head)
      htmltree.push("\">")
      htmltree.push(Compiler.Parser.parseTree.head)
      htmltree.push("</a>")
      Compiler.Parser.parseTree.pop()
      Compiler.Parser.parseTree.pop()
      Compiler.Parser.parseTree.pop()
      Compiler.Parser.parseTree.pop()
      Compiler.Parser.parseTree.pop()
      htmlcode()
    }
    else if (Compiler.Parser.parseTree.head.equalsIgnoreCase(CONSTANTS.PARAE)){
      htmltree.push("</p")
      Compiler.Parser.parseTree.pop()
      htmlcode()
    }
    else if (Compiler.Parser.parseTree.head.equalsIgnoreCase(CONSTANTS.LISTITEMB)){
      htmltree.push("<li>")
      Compiler.Parser.parseTree.pop() //pops +
      htmltree.push(Compiler.Parser.parseTree.head)
      Compiler.Parser.parseTree.pop() //pops fname lname
      htmltree.push("</li>")
      htmlcode()
    }
    else if (Compiler.Parser.parseTree.head.equalsIgnoreCase(CONSTANTS.NEWLINE)){
      htmltree.push("<br>")
      Compiler.Parser.parseTree.pop() //pop \\
      htmlcode()
    }
    else if (Compiler.Parser.parseTree.head.equals(CONSTANTS.IMAGEB)){
      htmltree.push("<img src=")
      Compiler.Parser.parseTree.pop() //pop ![
      htmltree.push(Compiler.Parser.parseTree.tail.tail.tail.head)
      htmltree.push("\"alt=")
      htmltree.push(Compiler.Parser.parseTree.head)
      htmltree.push("\"")
      Compiler.Parser.parseTree.pop()
      Compiler.Parser.parseTree.pop()
      Compiler.Parser.parseTree.pop()
      Compiler.Parser.parseTree.pop()
      Compiler.Parser.parseTree.pop()
      htmlcode()
    }
    else if (Compiler.Parser.parseTree.head.equals(CONSTANTS.DOCE)){
      htmltree.push("</html>")
      Compiler.Parser.parseTree.pop()
    }
    else { //(Compiler.Parser.parseTree.head.isText){
      htmltree.push(Compiler.Parser.parseTree.head)
      Compiler.Parser.parseTree.pop()
      htmlcode()
    }
  }

  /* * Hack Scala/Java function to take a String filename and open in default web browswer. */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    //println("HTML FS " + htmlFileStr) //htmlFileStr = project1code
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")
    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }
}