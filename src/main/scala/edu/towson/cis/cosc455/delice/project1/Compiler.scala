package edu.towson.cis.cosc455.delice.project1

import java.io.File

object Compiler
{
  //Initial Declarations
  var currentToken : String = ""
  var fileContents : String = ""
  var isEnd : Boolean = false
  var isText : Boolean = false

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer

  def main(args: Array[String]): Unit = {
    checkFile(args)
    readFile(args(0))

    Scanner.start(fileContents)
    Parser.gittex()

    Parser.parseTree = Parser.parseTree.reverse
    SemanticAnalyzer.startSemantic()
  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of arguments.")
      System.exit(1)
    }
    else if (!args(0).endsWith(".gtx")) {
      println("USAGE ERROR: wrong extension, file should be a .gtx file.")
      System.exit(1)
    }
  }

  //Tried to do a method so that if filename is Text2.gtx, then the output file will be Test2.html
  /*
  def filename (args : Array[String]) = {
    if (args(0).endsWith("Test2.gtx")){
      SemanticAnalyzer.file_name = "Test2.html"
    }
    else if (args(0).endsWith("Test3.gtx")){
      SemanticAnalyzer.file_name = "Test3.html"
    }
  }
  */
}