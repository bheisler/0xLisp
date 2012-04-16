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
        args ::: doCall
      }
      else {
        println( "Call to unknown function: " + call.operation )
        Nil
      }
  }
  
  def handleArgs( args: List[Expr] ) : List[Instruction] = {
    args match {
      case Nil => Nil
      case x => handleArgs( args.tail ) ::: handleElement( args.head )
    }
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
  
  def assertArgumentCount( count: Int, call: Call ) = {
    if ( call.arguments.length != count ) {
      emitError( call.operation + " must have " + count + " arguments. " + call.arguments.length + " arguments were passed." );
    }
  }
}