package com.github.oxlisp.lisp

import com.github.oxlisp.assembly.Instructions._
import com.github.oxlisp.assembly.Values._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class Compiler {
  
  var callMap = new HashMap[String, Call => List[Instruction] ]
  callMap += ( "add1" -> add1 )
  callMap += ( "sub1" -> sub1 )
  callMap += ( "zero?" -> isZero )
  callMap += ( "null?" -> isZero )
  callMap += ( "not" -> not )
  callMap += ( "+" -> add )
  callMap += ( "-" -> subtract )
  callMap += { "/" -> divide }
  callMap += { "%" -> mod }
  callMap += { "*" -> mult }
  
  var errors: List[String] = Nil
  
  def emitError( error: String ) {
    errors = error :: errors
  }

  def compile( tree: List[LispElement] ) : List[Instruction] = {
    val compiled = new ListBuffer[Instruction]
    for ( element <- tree ) {
      val evalElement = handleElement( element )
      compiled ++= evalElement
    }
    compiled.toList
  }
  
  def handleElement( elem: LispElement ) : List[Instruction] = elem match {
    case num: Num => handleNum( num )
    case call : Call => handleCall( call )
    case x => emitError( "Unknown element: " + x ); Nil
  }
  
  def handleNum( num: Num ) : List[Instruction] = {
    SET( A, num.v ) :: Nil
  }
  
  def handleCall( call: Call ) : List[Instruction] = {
      if ( callMap.contains( call.operation ) ) {
        val args = handleArgs( call.arguments )
        val doCall = callMap.get( call.operation ).get.apply( call )
        val cleanup = if ( call.arguments.length > 3 ) {
          ADD( SP, call.arguments.length - 3 ) :: Nil
        }
        else {
          Nil
        }
        args ::: doCall ::: cleanup
      }
      else {
        println( "Call to unknown function: " + call.operation )
        Nil
      }
  }
  
  def handleArgs( args: List[Expr] ) : List[Instruction] = {
    val argumentExpressions = args.reverse.flatMap { handleElement( _ ) ::: List( SET( PUSH, A ) ) }.init
    var popInstructions : List[Instruction] = Nil
    if ( args.length >= 3 ) { popInstructions = SET( C, POP ) :: popInstructions }
    if ( args.length >= 2 ) { popInstructions = SET( B, POP ) :: popInstructions }
    argumentExpressions ::: popInstructions
  }
  
  def add1( call: Call ) : List[Instruction] = {
    assertArgumentCount( 1, call )
    ADD( A, 1 ) :: Nil
  }
  
  def sub1( call: Call ) : List[Instruction] = {
    assertArgumentCount( 1, call )
    SUB( A, 1 ) :: Nil
  }
  
  def isZero( call: Call ) : List[Instruction] = {
    assertArgumentCount( 1, call )
    List( SUB( A, 1 ),
          SET( A, 0 ),
          SUB( A, 0 ) )
  }
  
  def not( call: Call ) : List[Instruction] = {
    assertArgumentCount( 1, call )
    List( SET( B, A ),
          SET( A, 65535 ),
          SUB( A, B ) )
  }
  
  def add( call: Call ) : List[Instruction] = {
    assertArgumentCount( 2, call )
    ADD( A, B ) :: Nil
  }
  
  def subtract( call: Call ) : List[Instruction] = {
    assertArgumentCount( 2, call )
    SUB( A, B ) :: Nil
  }
  
  def divide( call: Call ) : List[Instruction] = {
    assertArgumentCount( 2, call );
    DIV( A, B ) :: Nil
  }
  
  def mult( call: Call ) : List[Instruction] = {
    assertArgumentCount( 2, call )
    MUL( A, B ) :: Nil
  }
  
  def mod( call: Call ) : List[Instruction] = {
    assertArgumentCount( 2, call )
    MOD( A, B ) :: Nil
  }
  
  def assertArgumentCount( count: Int, call: Call ) = {
    if ( call.arguments.length != count ) {
      emitError( call.operation + " must have " + count + " arguments. " + call.arguments.length + " arguments were passed." );
    }
  }
}