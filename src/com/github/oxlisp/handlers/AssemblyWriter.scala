package com.github.oxlisp.handlers
import java.io.Writer
import scala.collection.immutable.HashMap
import com.github.oxlisp.Call
import com.github.oxlisp.Element
import com.github.oxlisp.Expr
import com.github.oxlisp.Num
import scala.io.Source

//TODO: Add proper compile error support.
class AssemblyWriter( val writer: Writer ) extends Handler {
  
  var callMap = new HashMap[String, (Call, Int) => Unit]
  callMap += ( "add1" -> add1 )
  callMap += ( "sub1" -> sub1 )
  callMap += ( "zero?" -> isZero )
  callMap += ( "null?" -> isZero )
  callMap += ( "not" -> not )

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
    writeln( "SET A, " + num.v, depth );
  }
  
  def handleCall( call: Call, depth: Int ) {
      if ( callMap.contains( call.operation ) ) {
        handleArgs( call.arguments, depth )
        callMap.get( call.operation ).get.apply( call, depth )
      }
      else {
        println( "Call to unknown function: " + call.operation )
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
  
  def handleArgs( args: List[Expr], depth: Int ) {
    if ( args.length == 0 ) {
      return;
    }
    handleArgs( args.tail, depth )
    handleElement( args.head, depth + 1 )
  }
  
  def sum( call: Call, depth: Int ) {
       
    writeln( "SET PUSH, A", depth )
    writeln( "SET A, 0", depth )
    handleArgs( call.arguments, depth )
    
    if ( depth > 0 ) {
        writeln( "SET X, A", depth );
    }
    
    writeln( "JSR sum", depth )
    
    if ( depth > 0 ) {
      writeln( "SET B, A", depth )
      writeln( "SET A, X", depth )
      writeln( "JSR freeList", depth )
      writeln( "SET A, POP", depth )
      writeln( "JSR cons", depth )
    }
  }
  
  def add1( call: Call, depth: Int ) {
    assertArgumentCount( 1, call )
    writeln( "ADD A, 1", depth )
  }
  
  def sub1( call: Call, depth: Int ) {
    assertArgumentCount( 1, call )
    writeln( "SUB A, 1", depth )
  }
  
  def isZero( call: Call, depth: Int ) {
    assertArgumentCount( 1, call )
    //TODO: Need to come up with a better way to do this.
    writeln( "SUB A, 1", depth )
    writeln( "SET A, 0", depth )
    writeln( "SUB A, 0", depth )
  }
  
  def not( call: Call, depth: Int ) {
    assertArgumentCount( 1, call )
    writeln( "SET B, A", depth )
    writeln( "SET A, 65535", depth )
    writeln( "SUB A, B", depth )
  }
  
  def assertArgumentCount( count: Int, call: Call ) = {
    if ( call.arguments.length != count ) {
      emitError( call.operation + " must have " + count + " arguments. " + call.arguments.length + " arguments were passed." );
    }
  }
}