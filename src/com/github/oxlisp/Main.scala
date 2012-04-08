package com.github.oxlisp
import java.io.File
import com.github.oxlisp.Parser.LispParsers
import java.io.FileReader
import com.github.oxlisp.handlers.Handler
import com.github.oxlisp.handlers.AstPrinter
import com.github.oxlisp.handlers.AssemblyWriter
import java.io.FileWriter

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
  
  var handlers : List[Handler] = new AssemblyWriter( new FileWriter( outfile ) ) :: Nil
  
  if ( args.contains( "--printAst" ) ) {
    handlers = new AstPrinter() :: handlers
  }
  
  handlers foreach { _.handleTree( parsed, 0 ) }
  
  def showHelp() {
    println("""
0xLisp Compiler: java -jar oxlisp.jar [-f <FileName>] [handlers]
-f <filename> : read commands from <filename>

Additional Handlers:
   --printAst : Prints the generated AST to stdout. Useful for testing.
    """)
  }
}