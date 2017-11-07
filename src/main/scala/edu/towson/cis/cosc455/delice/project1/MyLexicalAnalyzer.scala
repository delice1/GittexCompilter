package edu.towson.cis.cosc455.delice.project1

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class MyLexicalAnalyzer extends LexicalAnalyzer {
  private var lexLength: Int = _
  //private var lexeme = new ListBuffer[Char]()
  private var lexeme: Array[Char] = new Array[Char](100)
  private var lexems = new ListBuffer[String]() //https://alvinalexander.com/scala/how-add-elements-to-a-list-in-scala-listbuffer-immutable
  lexems.toList //Converts ListBuffer to a List
  private var nextChar : Char = _
  private var sourceLine: String = _
  private var position: Int = _
  var parseTree = new scala.collection.mutable.Stack[String]

  //Gets first lexeme
  def start(line: String): Unit = {
    initializeLexems()
    sourceLine = line
    position = 0

    nextChar = getChar() //getChar gets the next character from the program string
    println("nextchar in start is " + nextChar)

    //getNextToken()

    getToken()
  }

  //adds current charcter to token after making sure length is not too long
  override def addChar(): Unit = {
    if (lexLength <= 98){
      lexeme({lexLength = lexLength + 1; lexLength-1}) = nextChar
      lexeme(lexLength) = 0
    }
    else {
      println("LEXICAL ERROR- lexem too long")
      if (!isSpace(nextChar)){
        while (!isSpace(nextChar)) getChar()
      }
      lexLength = 0
      getNonBlank()
      addChar()
    }
  }

  private def getToken() : Unit = {
    //Compiler.currentToken = "\\BEGIN"

    /*
    lexLength = 0
    //Ignore spaces and add the first character to token
    getNonBlank()
    addChar()
    nextChar = getChar() //originally just getChar
    */

    //Continue gathering characters for token
    while ((nextChar != '\n') && (nextChar != ' ') && (nextChar != '\t')) { //added not equal to tab //it finds a space so it stops processing token
      addChar()
      nextChar = getChar() //originally just getChar
      //println(nextChar)
    }
    println("testing 123")
    //Convert gathered character array token into a String

    //val testing = lexeme.toArray
    val newTokens: String = new String(lexeme)
    //println("new token " + newTokens)
    //val stringfromarray = newTokens.mkString("")
    //println("stringfromarray" + stringfromarray.length)
    //println("new token converting to a string is  : " + newTokens.mkString(""))
    //println("new token converting to a string length is : " + newTokens.mkString.length)
    if (lookup(newTokens.substring(0, lexLength)))
      Compiler.currentToken_$eq(newTokens.substring(0, lexLength))
    println("current token in newTokens is " + newTokens)

    val str : String = newTokens.head.toString
    println("str is " + str)
    caseswitch(str, newTokens)
    /*
    nextChar match {
      case '!' => image(); lookup(Compiler.currentToken)
      case '+' => listitem(); lookup(Compiler.currentToken)
      case '[' => link(); lookup(Compiler.currentToken)
      case '\\' => newline(); lookup(Compiler.currentToken)
      case '#' => heading(); lookup(Compiler.currentToken)
      case '*' => bold(); lookup(Compiler.currentToken)
      case default => text() //somethings wrong with text! need to change!
    }
    */

    /*
    var newToken : String = new String(lexeme)
    if (lookup(newToken.substring(0,lexLength)))
      Compiler.currentToken_$eq(newToken.substring(0,lexLength))
    //Compiler.currentToken = "\\BEGIN"
    println("current token in getToken is " + Compiler.currentToken)
    println("nextchar in getToken is " + nextChar)
    */

    /*
      nextChar match {
        case '!' => image(); lookup(Compiler.currentToken)
        case '+' => listitem(); lookup(Compiler.currentToken)
        case '[' => link(); lookup(Compiler.currentToken)
        case '\\' => newline(); lookup(Compiler.currentToken)
        case '#' => heading(); lookup(Compiler.currentToken)
        case '*' => bold(); lookup(Compiler.currentToken)
        case default => text() //somethings wrong with text! need to change!
    }
    */
  }

  private def caseswitch(str : String, newTokens : String) = {
    str match{
      case "!" => image(newTokens); lookup(newTokens)
      case "+" => listitem(newTokens); lookup(newTokens)
      case "[" => link(newTokens); lookup(newTokens)
      case "\\" => newline(newTokens); lookup(newTokens)
      case "#" => heading(newTokens); lookup(newTokens)
      case "*" => bold(newTokens); lookup(newTokens)
      case default => println("default case reached"); text(newTokens) //somethings wrong with text! need to change!
    }
  }

  //Checks if current char is a space
  private def isSpace(c: Char): Boolean = {
    c == ' '
  }

  override def lookup(candidateToken: String): Boolean = {
    println("Candidate token in lookup is '" + candidateToken + "'")
    //println("lexems in lookup are : " + lexems)
    if (!lexems.contains(candidateToken)) {
      Compiler.Parser.setError()
      println("LEXICAL ERROR-'" + candidateToken + "' not recognized.")
      //System.exit(1)
      false
    }
    else{
      true
    }
  }
  private def getNonBlank(): Unit = {
    while (isSpace(nextChar))
      nextChar = getChar()
  }

  override def getNextToken(): Unit = {
    /*
    lexLength = 0
    //Ignore spaces and add the first character to token
    getNonBlank()
    addChar()
    nextChar = getChar()
    // nextChar = getChar() //originally just getChar
    */

    //Continue gathering characters for token
    while ((nextChar != '\n') && (nextChar != ' ') && (nextChar != '\t')){ //added not equal to tab
      addChar()
      nextChar = getChar() //originally just getChar
    }

    //Convert gathered character array token into a String
    //val testing = lexeme.toArray
    val newToken: String = new String(lexeme)

    if (lookup(newToken.substring(0, lexLength)))
      Compiler.currentToken_$eq(newToken.substring(0,lexLength))
    println("current token in getNexToken is " + Compiler.currentToken)
  }

  override def getChar(): Char = {
    if(position < sourceLine.length){
      sourceLine.charAt({position = position + 1; position - 1})
    }
    else
      '\n'
  }

  //add legal lexems to language
  private def initializeLexems(): Unit = {
    //val validLexems : List[String] = List (CONSTANTS.DOCB)
    lexems += CONSTANTS.DOCB
    lexems += CONSTANTS.DOCE
    lexems += CONSTANTS.TITLEB
    lexems += CONSTANTS.BRACKETE
    lexems += CONSTANTS.HEADING
    lexems += CONSTANTS.PARAB
    lexems += CONSTANTS.PARAE
    lexems += CONSTANTS.BOLD
    lexems += CONSTANTS.LISTITEMB
    lexems += CONSTANTS.NEWLINE
    lexems += CONSTANTS.LINKB
    lexems += CONSTANTS.ADDRESSB
    lexems += CONSTANTS.ADDRESSE
    lexems += CONSTANTS.IMAGEB
    lexems += CONSTANTS.DEFB
    lexems += CONSTANTS.EQSIGN
    lexems += CONSTANTS.USEB
    lexems ++= CONSTANTS.LETTERS
    lexems ++= CONSTANTS.NUMBERSETC
    lexems ++= CONSTANTS.WHITESPACE
    lexems ++= CONSTANTS.VALIDTEXT

    //lexems += "\n"
    //NEED TO CONTINUE WITH ALL OF THEM
    /*
    CONSTANTS.DOCB :: lexems
    CONSTANTS.DOCE :: lexems
    CONSTANTS.TITLEB :: lexems
    CONSTANTS.BRACKETE :: lexems
    CONSTANTS.HEADING :: lexems
    CONSTANTS.PARAB :: lexems
    CONSTANTS.PARAE :: lexems
    CONSTANTS.BOLD :: lexems
    CONSTANTS.LISTITEM :: lexems
    CONSTANTS.NEWLINE :: lexems
    CONSTANTS.LINKB :: lexems
    CONSTANTS.ADDRESSB :: lexems
    CONSTANTS.ADDRESSE :: lexems
    CONSTANTS.IMAGEB :: lexems
    CONSTANTS.DEFB :: lexems
    CONSTANTS.EQSIGN :: lexems
    CONSTANTS.USEB :: lexems
    CONSTANTS.LETTERS :: lexems
    CONSTANTS.NUMBERSETC :: lexems
    CONSTANTS.WHITESPACE :: lexems
    CONSTANTS.VALIDTEXT :: lexems
    */
  }
  //called if token is text
  private def text (newTokens : String) : Unit = {
    println("tell ma i made it to text")
    nextChar = getChar()
    addChar()
    //parseTree.push(Compiler.currentToken)
  }

  //called if token is image
  private def image(newTokens : String): Unit = {
    //lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      println("hello1")
      //parseTree.push(Compiler.currentToken)
      //lookup(Compiler.currentToken)
      //getNextToken()
      println("current token now is " + newTokens)
      if (Compiler.currentToken == CONSTANTS.VALIDTEXT) {
        println("hello2")
        //parseTree.push(Compiler.currentToken)
        //getNextToken(newTokens)
        //Compiler.currentToken = "]"
        println("current token here is " + newTokens)
        if (Compiler.currentToken == CONSTANTS.BRACKETE) {
          print("contains brackete")
          //parseTree.push(Compiler.currentToken)
          //getNextToken()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
            parseTree.push(Compiler.currentToken)
            //getNextToken()
            if (Compiler.currentToken == CONSTANTS.VALIDTEXT) {
              parseTree.push(Compiler.currentToken)
              //getNextToken()
              if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
                parseTree.push(Compiler.currentToken)
                //getNextToken() // not sure if i need
                //true
              }
              else println("looking for addresse in image")
            }
            else println("looking for valid text in image")
          }
          else println("looking for addressb in image")
        }
        else println("looking for brackete in image")
        }
        else println("looking for valid text in image")
      }
      else println("looking for imageb in image")
    }

  //called if token is list
  private def listitem(newTokens : String): Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB)){
      parseTree.push(Compiler.currentToken)
      //getNextToken()
      inneritem(newTokens)
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB)){
        listitem(newTokens)
      }
    }
  }

  //inner item for the grammar
  private def inneritem(newTokens : String) : Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken.equals(CONSTANTS.USEB)) {
      variableuse(newTokens)
      inneritem(newTokens)
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      bold(newTokens)
      inneritem(newTokens)
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      link(newTokens)
      inneritem(newTokens)
    }
    else if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
      text(newTokens)
      inneritem(newTokens)
    }
  }

  private def variableuse(newTokens : String) : Unit = {
    //lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      parseTree.push(Compiler.currentToken)
      //getNextToken()
      if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
        parseTree.push(Compiler.currentToken)
        //getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
          parseTree.push(Compiler.currentToken)
          //getNextToken()
        }
        else println ("error- brackete not found in variableuse")
      }
      else println ("error- looking for text in variableuse")
    }
    else println ("error- looking for useb in variableuse")
  }

  //called if token is link
  private def link(newTokens : String): Unit = {
    //lookup(Compiler.currentToken)
    if (newTokens.equals(CONSTANTS.LINKB)){
      parseTree.push(Compiler.currentToken)
      //getNextToken()
      if (newTokens == CONSTANTS.VALIDTEXT){
        parseTree.push(Compiler.currentToken)
        //getNextToken()
        if (newTokens.equalsIgnoreCase(CONSTANTS.BRACKETE)){
          parseTree.push(Compiler.currentToken)
          //getNextToken()
          if (newTokens.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
            parseTree.push(Compiler.currentToken)
            //getNextToken()
            if (newTokens == CONSTANTS.VALIDTEXT){
              parseTree.push(Compiler.currentToken)
              //getNextToken()
              if (newTokens.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
                parseTree.push(Compiler.currentToken)
                //getNextToken()
              }
              else     println("Error- looking for addresse in link")
            }
            else   println("Error- looking for required text in link")
          }
          else   println(("error- looking for addressb in link"))
        }
        else  print("error- looking for brackete in link")
      }
      else   println("error- looking for required text in link")
    }
    else  println("looking for linkb in link")
  }

  //called if token newline, title, paragraph, variable define, or begin/end
  private def newline(newTokens : String) : Unit = {
    println("current token in newline " + newTokens)
    println("substring : " + newTokens.substring(0,7))
    //lookup(Compiler.currentToken)
    //Compiler.currentToken = "\\BEGIN"
    //getNextToken()
    if (Compiler.currentToken == CONSTANTS.NEWLINE) {
      parseTree.push(Compiler.currentToken)
      //getNextToken()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      parseTree.push(Compiler.currentToken)
      /*
      val newToken: String = new String(lexeme)

      if (lookup(newToken.substring(0, lexLength)))
        Compiler.currentToken_$eq(newToken.substring(0,lexLength))
      */
      //getToken()
      //getNextToken()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      parseTree.push(Compiler.currentToken)
      //getNextToken()
    }
    else if (newTokens.substring(0,7).equalsIgnoreCase(CONSTANTS.TITLEB)){
      title(newTokens)
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      paragraph(newTokens)
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
      variabledefine(newTokens)
    }
    else{
      println("error- newline error")
    }
  }

  private def paragraph(newTokens : String) : Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      parseTree.push(Compiler.currentToken)
      //getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
        variabledefine(newTokens)
        //getNextToken()
        if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) || (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) || (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) || (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB)) || (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) || (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) || (Compiler.currentToken == CONSTANTS.VALIDTEXT)) {
          innertext(newTokens)
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
            parseTree.push(Compiler.currentToken)
            //getNextToken()
          }
          else println ("parae expected in paragraph")
        }
        else println ("inner text expected in paragraph")
      }
      else println ("defb expected in paragraph")
    }
    else println ("parab expected in paragraph")
  }

  private def innertext(newTokens : String) : Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableuse(newTokens)
      //getNextToken()
      innertext(newTokens)
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      heading(newTokens)
      //getNextToken()
      innertext(newTokens)
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold(newTokens)
      //getNextToken()
      innertext(newTokens)
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB)) {
      listitem(newTokens)
      //getNextToken()
      innertext(newTokens)
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      image(newTokens)
      //getNextToken()
      innertext(newTokens)
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      link(newTokens)
      //getNextToken()
      innertext(newTokens)
    }
    else if (Compiler.currentToken == CONSTANTS.VALIDTEXT) {
      text(newTokens)
      //getNextToken()
    }
  }


  private def variabledefine(newTokens : String) : Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
      parseTree.push(Compiler.currentToken)
      //getNextToken()
      if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
        text(newTokens)
        //getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)){
          parseTree.push(Compiler.currentToken)
          //getNextToken()
          if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
            text(newTokens)
            //getNextToken()
            if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
              parseTree.push(Compiler.currentToken)
              //getNextToken()
              if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
                variabledefine(newTokens)
                //getNextToken()
              }
              else println ("defb not found in variabledefine")
            }
            else println ("brackete not found in variabledefine")
          }
          else println("validtext not found in variable define")
        }
        else println("equal sign not found in variable define")
      }
      else println ("valid text not found in variable define")
    }
    else println("defb not found in variable define")
  }

  private def title(newTokens : String) : Unit = {
    if (newTokens.substring(0,7).equalsIgnoreCase(CONSTANTS.TITLEB)){
      println("new tokens is " + newTokens)
      var removeBracket = newTokens.replace("]","")
      println("remove bracket " + removeBracket)
      println("token trimmed is " + removeBracket.substring(7,newTokens.length - 1))
      //println("substring " + newTokens.substring(7,removeBracket.length))
      //parseTree.push(Compiler.currentToken)
      //getNextToken()
      //println(removeBracket.substring(7,newTokens.length() - 1) == CONSTANTS.VALIDTEXT)

      /*
      if (removeBracket.substring(7,newTokens.length() - 1) == CONSTANTS.VALIDTEXT){
        println("made it to text in title")
        text(newTokens)
        //getNextToken()
        */
      //var arraytolist = lexeme.toList
      //var string1 = lexeme.toString

      //val trying = scala.collection.mutable.ListBuffer lexeme
      val converArrayToListBuffer = ListBuffer(lexeme:_*)
      //println("convertArrayToListBuffer " + converArrayToListBuffer)
      val convertListBufferToString = converArrayToListBuffer.mkString(" ")
      //println("convertListBufferToString '" + convertListBufferToString + "'")
      val trimConverListBufferToString = convertListBufferToString.trim
      //println("trimConvertListBufferToString '" + trimConverListBufferToString + "'")
      val trimConvertListBufferToStringReplacingSpaces = trimConverListBufferToString.replaceAll(" ","")
      //println("trimConvertListBufferToStringRplacingSpcaes '" + trimConvertListBufferToStringReplacingSpaces + "'")
      //val tryingmoremore = tryingevenmore.toList
      //println("tryingmoremore '" + tryingmoremore + "'" )
      //println("tryingevenmoremore length " + trimConvertListBufferToStringReplacingSpaces.length)
      var finalLength : Int = trimConvertListBufferToStringReplacingSpaces.length-1
      //println("tryinevenmoremore length minus 1 " + finalLength)

      //val newlexeme = scala.collection.mutable.Set()++lexeme //a set cannot contain duplicate elements... that's why it isn't working
      println("NEW LEXEME IS " + trimConvertListBufferToStringReplacingSpaces)
      println("THE NEW LEXEME SIZE IS " + finalLength)
      
        if (newTokens.charAt(trimConvertListBufferToStringReplacingSpaces.size-1).toString == (CONSTANTS.BRACKETE)){
          println("magically found a bracket")
          //parseTree.push(Compiler.currentToken)
          //getNextToken()
        }
        else println ("brackete missing in title")
      }
      /*
      else println("Error: text missing in title")
    }
    */
    else println ("titleb missing in title")
  }

  //called if token heading
  private def heading(newTokens : String) : Unit = {
    //lookup(Compiler.currentToken)
    if (Compiler.currentToken == CONSTANTS.HEADING){
      println("made it")
      parseTree.push(Compiler.currentToken)
      //getNextToken()
      if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
        text(newTokens)
        //getNextToken()
      }
      else println("Error: looking for text in heading")
    }
  }

  //called if token bold
  private def bold(newTokens : String) : Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      //parseTree.push(Compiler.currentToken)
      //getNextToken()
      if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
        text(newTokens)
        //getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
          //parseTree.push(Compiler.currentToken)
          //getNextToken()
        }
        else {
          println("Looking for bold in bold function")
          Compiler.Parser.setError()
          System.exit(1)
        }
      }
      else   println("Looking for bold in bold function")
    }
  }
}
