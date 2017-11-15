package edu.towson.cis.cosc455.delice.project1

import java.awt.Desktop
import java.io.{File, IOException, PrintWriter}
import edu.towson.cis.cosc455.delice.project1.Compiler.Parser

class MySemanticAnalyzer
{
  var htmltree = scala.collection.mutable.Stack[String]()
  var htmltreestring : String = ""
  var file_name : String = "project1code.html"
  var varname : String = ""
  var varvalue : String = ""

  def getParseTreeHead() : String = {
    Parser.parseTree.head
  }

  def startSemantic () = {
    htmlcode()

    htmltreestring = htmltree.reverse.toString()
    htmltreestring = htmltreestring.replace("Stack(","")
    htmltreestring = htmltreestring.replace(",","")
    htmltreestring = htmltreestring.replace("</html>)","</html>")

    //Write text to a file- adapted from alvinalexander.com
    var html = new PrintWriter(new File(file_name))
    html.write(htmltreestring)
    html.close()
    openHTMLFileInBrowser(file_name)
  }

  //Converts gittex language to HTML Code
  private def htmlcode(): Unit ={
    //DOCB
    if (getParseTreeHead().equalsIgnoreCase(CONSTANTS.DOCB)){
      htmltree.push("<html>")
      Compiler.Parser.parseTree.pop()
      htmlcode()
    }
    //TITLE
    else if (getParseTreeHead().equalsIgnoreCase(CONSTANTS.TITLEB)){
      htmltree.push("<head>").push("<title>")
      Compiler.Parser.parseTree.pop() //\TITLE[
      htmltree.push(Compiler.Parser.parseTree.head)
      Compiler.Parser.parseTree.pop() //The Simpsons
      //htmltree.push(Compiler.Parser.parseTree.head)
      Compiler.Parser.parseTree.pop()
      htmltree.push("</title>").push("<head>")
      htmlcode()
    }
    //HEADING
    else if (getParseTreeHead().equalsIgnoreCase(CONSTANTS.HEADING)){
      htmltree.push("<h1>")
      Compiler.Parser.parseTree.pop()
      htmltree.push(Compiler.Parser.parseTree.head)
      Compiler.Parser.parseTree.pop()
      htmltree.push("</h1>")
      htmlcode()
    }
    //PARAB
    else if (getParseTreeHead().equalsIgnoreCase(CONSTANTS.PARAB)){
      htmltree.push("<p>")
      Compiler.Parser.parseTree.pop()
      htmlcode()
    }
    //LINK
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
    //PARAE
    else if (Compiler.Parser.parseTree.head.equalsIgnoreCase(CONSTANTS.PARAE)){
      htmltree.push("</p>")
      Compiler.Parser.parseTree.pop()
      htmlcode()
    }
    //LIST ITEM
    else if (Compiler.Parser.parseTree.head.equalsIgnoreCase(CONSTANTS.LISTITEMB)){
      htmltree.push("<li>")
      Compiler.Parser.parseTree.pop() //pops +
      htmltree.push(Compiler.Parser.parseTree.head)
      Compiler.Parser.parseTree.pop() //pops fname lname
      htmltree.push("</li>")
      htmlcode()
    }
    //NEWLINE
    else if (Compiler.Parser.parseTree.head.equalsIgnoreCase(CONSTANTS.NEWLINE)){
      htmltree.push("<br>")
      Compiler.Parser.parseTree.pop() //pop \\
      htmlcode()
    }
    //IMAGEB
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
    //DOCE
    else if (Compiler.Parser.parseTree.head.equalsIgnoreCase(CONSTANTS.DOCE)){
      htmltree.push("</html>")
      Compiler.Parser.parseTree.pop()
    }
    //BOLD
    else if (Compiler.Parser.parseTree.head.equalsIgnoreCase(CONSTANTS.BOLD)){
      htmltree.push("<b>")
      Compiler.Parser.parseTree.pop()
      htmltree.push(Compiler.Parser.parseTree.head)
      Compiler.Parser.parseTree.pop()
      htmltree.push("</b>")
      Compiler.Parser.parseTree.pop()
      htmlcode()
    }
    //VARIABLE DEFINE
    else if (Compiler.Parser.parseTree.head.equalsIgnoreCase(CONSTANTS.DEFB)){
      htmltree.push(Compiler.Parser.parseTree.head) //\DEFB
      Compiler.Parser.parseTree.pop()
      htmltree.push(Compiler.Parser.parseTree.head) //variable name
      varname = Compiler.Parser.parseTree.head //stores variable name to varname
      println("VARNAME " + varname)
      Compiler.Parser.parseTree.pop()
      htmltree.push(Compiler.Parser.parseTree.head) // =
      Compiler.Parser.parseTree.pop()
      htmltree.push(Compiler.Parser.parseTree.head) // value
      varvalue = Compiler.Parser.parseTree.head //stores value to varvalue
      println("VARVALUE " + varvalue)
      Compiler.Parser.parseTree.pop()
      htmltree.push(Compiler.Parser.parseTree.head) // ]
      Compiler.Parser.parseTree.pop()
      htmlcode()
    }
    //VARIABLE USE
    else if (Compiler.Parser.parseTree.head.equalsIgnoreCase(CONSTANTS.USEB)){
      htmltree.push(Compiler.Parser.parseTree.head) // \USE[
      Compiler.Parser.parseTree.pop()
      println("HEAD " + Compiler.Parser.parseTree.head)
      if (varname.equals(Compiler.Parser.parseTree.head)) {
        htmltree.push(Compiler.Parser.parseTree.head) // variable name
        Compiler.Parser.parseTree.pop()
      }
      else{
        println("Semantic error, variable not declared anywhere: " + Compiler.Parser.parseTree.head)
        System.exit(1)
      }
      htmltree.push(Compiler.Parser.parseTree.head) // ]
      Compiler.Parser.parseTree.pop()
      htmlcode()
    }
    //TEXT
    else { //(Compiler.Parser.parseTree.head.isText){
      htmltree.push(Compiler.Parser.parseTree.head)
      Compiler.Parser.parseTree.pop()
      htmlcode()
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