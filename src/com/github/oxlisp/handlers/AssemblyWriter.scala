package com.github.oxlisp.handlers
import java.io.Writer
import scala.collection.immutable.HashMap
import com.github.oxlisp.Call
import com.github.oxlisp.Element
import com.github.oxlisp.Expr
import com.github.oxlisp.Num
import scala.io.Source

class AssemblyWriter( val writer: Writer ) extends Handler {
  
  var labelMap = new HashMap[String, String]
  labelMap = labelMap + ( "+" -> "sum" )

  override def handleTree( tree: List[Element], depth: Int ) {
    super.handleTree( tree, depth )
    if ( depth == 0 ) end();
  }
  
  def handleElement( elem: Element, depth: Int ) = elem match {
    case num: Num => handleNum( num, depth )
    case call : Call => handleCall( call, depth )
    case _ =>
  }
  
  def handleNum( num: Num, depth: Int ) {
    writeln( "SET B, " + num.v, depth );
    writeln( "JSR cons", depth );
  }
  
  def handleCall( call: Call, depth: Int ) {
    def handleArgs( args: List[Expr] ) {
      if ( args.length == 0 ) {
        return;
      }
      handleArgs( args.tail )
      handleElement( args.head, depth + 1 )
    }
       
    writeln( "SET PUSH, A", depth )
    writeln( "SET A, 0", depth )
    handleArgs( call.arguments )
    
    if ( depth > 0 ) {
    	writeln( "SET X, A", depth );
    }
    
    writeln( "JSR " + labelMap( call.operation ), depth )
    
    if ( depth > 0 ) {
      writeln( "SET B, A", depth )
      writeln( "SET A, X", depth )
      writeln( "JSR freeList", depth )
      writeln( "SET A, POP", depth )
      writeln( "JSR cons", depth )
  	}
  }
  
  def write( str: String, depth: Int ) {
    writer.write( ( "\t" * depth ) + str )
  }
  
  def writeln( str: String, depth: Int ) {
    write( str + "\n", depth )
  }
  
  def end() {
    Source.fromFile( "base.dasm16" ).getLines().foreach { writeln( _, 0 ) }
    writer.close()
  }
}