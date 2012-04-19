package com.github.oxlisp.lisp

import java.io.File
import java.io.FileReader
import java.io.FileWriter

import com.github.oxlisp.assembly.Instructions._

object Main extends App {

  val file: File = if ( args contains "-f" ) {
    val fileName = args( args.indexOf("-f") + 1)
    new File( fileName )
  }
  else {
    showHelp
    exit()
  }
  
  val inPath = file.getAbsolutePath()
  
  val outfile = new File( inPath.take(1 + inPath.lastIndexOf(".") ) + "dasm16" )
    
  val parsed = LispParsers.parse( new FileReader( file ) )
  
  if ( parsed.isEmpty ) {
    System.exit(1)
  }
  
  val syntaxTree = parsed.get
   
  if ( args.contains( "--printAst" ) ) {
    new AstPrinter().handleTree(syntaxTree, 0)
  }
  
  val compiler = new Compiler(Scope.defaultScope)
  
  val instructions = compiler.compile( syntaxTree )
  
  if ( compiler.errors.length > 0 ) {
    compiler.errors foreach { System.err.println _ }
  }
  else {
    val writer = new FileWriter( outfile )
    writer.write( instructions.mkString( "\n" ) ) 
    writer.close()
  }
  
  
  def showHelp() {
    println("""
0xLisp Compiler: java -jar oxlisp.jar [-f <FileName>] [handlers]
-f <filename> : read commands from <filename>

Additional Handlers:
   --printAst : Prints the generated AST to stdout. Useful for testing.
    """)
  }
}