package edu.towson.cis.cosc455.delice.project1

import java.awt.Desktop
import java.io.{File, IOException, PrintWriter}

class MySemanticAnalyzer
{
  var htmltree = scala.collection.mutable.Stack[String]()
  //htmltree = Compiler.Parser.parseTree

  def startSemantic () = {
    println("START SEMANTIC")
    //htmltree = htmltree.reverse
    println(Compiler.Parser.parseTree)
    htmlcode()
    println(htmltree.reverse)
    println(Compiler.Parser.parseTree)
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
    println("made it")
    println((Compiler.Parser.parseTree.head))
    if (Compiler.Parser.parseTree.head.equalsIgnoreCase(CONSTANTS.DOCB)){
      htmltree.push("<html>")
      Compiler.Parser.parseTree.pop() //needs to keep moving to next things in stack? right logic?
      htmlcode()
    }
    if (Compiler.Parser.parseTree.head.equalsIgnoreCase(CONSTANTS.TITLEB)){
      htmltree.push("<head>").push("<title>")
      Compiler.Parser.parseTree.pop()//need to move to next position on stack (POP??)
      htmltree.push(Compiler.Parser.parseTree.head)
      Compiler.Parser.parseTree.pop()
      htmltree.push(Compiler.Parser.parseTree.head)
      Compiler.Parser.parseTree.pop()
      htmltree.push("</title").push("<head>")
      htmlcode()
    }
    if (Compiler.Parser.parseTree.head.equalsIgnoreCase(CONSTANTS.HEADING)){
      htmltree.push("<h1>")
      Compiler.Parser.parseTree.pop()
      htmltree.push(Compiler.Parser.parseTree.head)
      Compiler.Parser.parseTree.pop()
      htmltree.push("</h1>")
      htmlcode()
    }
    if (Compiler.Parser.parseTree.head.equalsIgnoreCase(CONSTANTS.PARAB)){
      htmltree.push("<p>")
      Compiler.Parser.parseTree.pop()
      htmlcode()
    }
    //println(Compiler.Parser.parseTree.head)
    if (Compiler.Parser.parseTree.head.isText){
      htmltree.push(Compiler.Parser.parseTree.head)
      Compiler.Parser.parseTree.pop()
      htmlcode()
    }
    if (Compiler.Parser.parseTree.head.equalsIgnoreCase(CONSTANTS.LINKB)){
      htmltree.push("<a href=\" ")
      Compiler.Parser.parseTree.pop()
      //keep going...
    }
  }

  /* * Hack Scala/Java function to take a String filename and open in default web browswer. */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
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