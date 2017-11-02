package edu.towson.cis.cosc455.delice.project1

import scala.io.Source

object Compiler {
  var currentToken : String = ""
  var fileContents : String = ""

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer

  def main(args: Array[String]): Unit = {
    checkFile(args)
    readFile(args(0))


    Scanner.getNextToken()
  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    for (line <-Source.fromFile(file).getLines()){
      Scanner.start(line)

      println(line) //need to change to open up html page
      //Parser.gittex()
    }
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println(args.length)
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (!args(0).endsWith(".gtx")) {
      println(args(0))
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }
}
