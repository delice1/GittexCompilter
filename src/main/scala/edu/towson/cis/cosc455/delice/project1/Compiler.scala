package edu.towson.cis.cosc455.delice.project1

object Compiler
{
  var currentToken : String = ""
  var fileContents : String = ""

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer

  def main(args: Array[String]): Unit =
  {
    checkFile(args)
    readFile(args(0))

    Parser.gittex()
  }

  def readFile(file : String) =
  {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
    Scanner.start(fileContents)
  }

  def checkFile(args : Array[String]) =
  {
    if (args.length != 1)
    {
      println("USAGE ERROR: wrong number of arguments.")
      System.exit(1)
    }
    else if (!args(0).endsWith(".gtx"))
    {
      println("USAGE ERROR: wrong extension, the file should be a .gtx file.")
      System.exit(1)
    }
  }
}