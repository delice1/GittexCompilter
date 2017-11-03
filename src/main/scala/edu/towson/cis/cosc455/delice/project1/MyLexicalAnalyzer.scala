package edu.towson.cis.cosc455.delice.project1

import scala.collection.mutable.ListBuffer

class MyLexicalAnalyzer extends LexicalAnalyzer {
  private var lexLength: Int = _
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
    nextChar = getChar()
    getNextToken()
  }

  override def addChar(): Unit = {
    if (lexLength <= 98){
      lexeme({lexLength += 1; lexLength-1}) = nextChar
      lexeme(lexLength) = 0
    }
    else {
      println("LEXICAL ERROR- lexem too long")
      if (!isSpace(nextChar)){
        while (!isSpace(nextChar)) getChar
      }
      lexLength = 0
      getNonBlank
      addChar()
    }
  }

  //called if token is text
  private def text () : Unit = {
    getChar()
    addChar()
    parseTree.push(Compiler.currentToken)
  }

  //called if token is image
  private def image(): Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken == CONSTANTS.IMAGEB){
      parseTree.push(Compiler.currentToken)
      getNextToken()
      if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
        parseTree.push(Compiler.currentToken)
        getNextToken()
        if (Compiler.currentToken == CONSTANTS.BRACKETE){
          parseTree.push(Compiler.currentToken)
          getNextToken()
          if (Compiler.currentToken == CONSTANTS.ADDRESSB){
            parseTree.push(Compiler.currentToken)
            getNextToken()
            if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
              parseTree.push(Compiler.currentToken)
              getNextToken()
              if (Compiler.currentToken == CONSTANTS.ADDRESSE){
                parseTree.push(Compiler.currentToken)
                getNextToken() // not sure if i need
                true
              }
              else println ("looking for addresse in image")
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
  private def listitem(): Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB)){
      parseTree.push(Compiler.currentToken)
      getNextToken()
      inneritem()
      listitem()
    }
  }

  //inner item for the grammar
  private def inneritem() : Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken.equals(CONSTANTS.USEB)) {
      variableuse()
      inneritem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      bold()
      inneritem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      link()
      inneritem()
    }
    else if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
      text()
      inneritem()
    }
  }

  private def variableuse() : Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)){
      parseTree.push(Compiler.currentToken)
      getNextToken()
      if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
        parseTree.push(Compiler.currentToken)
        getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
          parseTree.push(Compiler.currentToken)
          getNextToken()
        }
        else println ("error- brackete not found in variableuse")
      }
      else println ("error- looking for text in variableuse")
    }
    else println ("error- looking for useb in variableuse")
  }

  //called if token is link
  private def link(): Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken.equals(CONSTANTS.LINKB)){
      parseTree.push(Compiler.currentToken)
      getNextToken()
      if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
        parseTree.push(Compiler.currentToken)
        getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
          parseTree.push(Compiler.currentToken)
          getNextToken()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)){
            parseTree.push(Compiler.currentToken)
            getNextToken()
            if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
              parseTree.push(Compiler.currentToken)
              getNextToken()
              if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)){
                parseTree.push(Compiler.currentToken)
                getNextToken()
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
  private def newline() : Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken == CONSTANTS.NEWLINE) {
      parseTree.push(Compiler.currentToken)
      getNextToken()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      parseTree.push(Compiler.currentToken)
      getNextToken()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      parseTree.push(Compiler.currentToken)
      getNextToken()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
    title()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      //call paragraph
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
      //call variabledefine
    }
    else{
      println("error- newline error")
    }
  }

  private def paragraph() : Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)){
      parseTree.push(Compiler.currentToken)
      getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
        variabledefine()
        getNextToken()
        if ((Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) || (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) || (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) || (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB)) || (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) || (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) || (Compiler.currentToken == CONSTANTS.VALIDTEXT)) {
          innertext()
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
            parseTree.push(Compiler.currentToken)
            getNextToken()
          }
          else println ("parae expected in paragraph")
        }
        else println ("inner text expected in paragraph")
      }
      else println ("defb expected in paragraph")
    }
    else println ("parab expected in paragraph")
  }

  private def innertext() : Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableuse()
      getNextToken()
      innertext()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      heading()
      getNextToken()
      innertext()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
      getNextToken()
      innertext()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEMB)) {
      listitem()
      getNextToken()
      innertext()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      image()
      getNextToken()
      innertext()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      link()
      getNextToken()
      innertext()
    }
    else if (Compiler.currentToken == CONSTANTS.VALIDTEXT) {
      text()
      getNextToken()
    }
  }


  private def variabledefine() : Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
      parseTree.push(Compiler.currentToken)
      getNextToken()
      if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
        text()
        getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)){
          parseTree.push(Compiler.currentToken)
          getNextToken()
          if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
            text()
            getNextToken()
            if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
              parseTree.push(Compiler.currentToken)
              getNextToken()
              if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)){
                variabledefine()
                getNextToken()
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

  private def title() : Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)){
      parseTree.push(Compiler.currentToken)
      getNextToken()
      if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
        text()
        getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)){
          parseTree.push(Compiler.currentToken)
          getNextToken()
        }
        else println ("brackete missing in title")
      }
      else println("text missing in title")
    }
    else println ("titleb missing in title")
  }

  //called if token heading
  private def heading() : Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken == CONSTANTS.HEADING){
      parseTree.push(Compiler.currentToken)
      getNextToken()
      if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
        text()
        getNextToken()
      }
    }
  }

  //called if token bold
  private def bold() : Unit = {
    lookup(Compiler.currentToken)
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      parseTree.push(Compiler.currentToken)
      getNextToken()
      if (Compiler.currentToken == CONSTANTS.VALIDTEXT){
        text()
        getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
          parseTree.push(Compiler.currentToken)
          getNextToken()
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

  //Checks if current char is a space
  private def isSpace(c: Char): Boolean = {
    c == ' '
  }

  override def lookup(candidateToken: String): Boolean = {
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
      getChar
  }

  override def getNextToken(): Unit = {
    lexLength = 0
    //Ignore spaces and add the first character to token
    getNonBlank
    addChar()
    nextChar = getChar() //originally just getChar

    //Continue gathering characters for token
    while ((nextChar != '\n') && (nextChar != ' ') && (nextChar != '\t')){ //added not equal to tab
      addChar()
      nextChar = getChar() //originally just getChar
    }

    //Convert gathered character array token into a String
    val newToken: String = new String(lexeme)

    if (lookup(newToken.substring(0, lexLength)))
      Compiler.currentToken_$eq(newToken.substring(0,lexLength))

    nextChar match{
      case '!' => image()
      case '+' => listitem()
      case '[' => link()
      case '\\' => newline()
      case '#' => heading()
      case '*' => bold()
      case default => text() //sometehings wrong with text! need to change!
    }
  }

  override def getChar(): Char = {
    if (position < sourceLine.length){
      sourceLine.charAt({position+=1; position-1})
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
}
