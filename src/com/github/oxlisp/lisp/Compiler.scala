package com.github.oxlisp.lisp

import com.github.oxlisp.assembly.Instructions._
import com.github.oxlisp.assembly.Values._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class Compiler(val scope: Scope) {
    
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
    case v: Var => handleVar( v )
    case let : Let => {
      val child = new Compiler( scope.newSubScope )
      val instructions = child.handleLet(let)
      errors = errors ::: child.errors
      instructions
    }
    case x => emitError( "Unknown element: " + x ); Nil
  }
  
  def handleNum( num: Num ) : List[Instruction] = {
    SET( A, num.v ) :: Nil
  }
  
  def handleVar( variable: Var ) : List[Instruction] = {
    val offset = scope.getVariable( variable )
    offset match {
      case Some(o) => SET( A, o ) :: Nil
      case None => emitError( "Undefined variable " + variable ); Nil
    }
  }
  
  def handleCall( call: Call ) : List[Instruction] = {
    val target = scope.getCall(call.operation)
    if ( target.isDefined ) {
      val args = handleArgs( call.arguments )
      val doCall = target.get.apply( call, this )
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
    
  def handleLet( let: Let ) : List[Instruction] = {
    val argumentExpressions = let.variables.flatMap{ x => handleLetVariable( x._1, x._2 ) }
    val subexpression = handleElement(let.body) 
    List( SET( PUSH, J ), SET( J, SP ) ) ::: argumentExpressions ::: subexpression ::: List( SET( SP, J ) )
  }
  
  def handleLetVariable( variable: Var, expression: Expr ) : List[Instruction] = {
    scope.putVariable( variable, Offset( scope.nextOffset, J ) )
    handleElement(expression) ::: List( SET( PUSH, A ) )
  }
  
  def assertArgumentCount( count: Int, call: Call ) = {
    if ( call.arguments.length != count ) {
      emitError( call.operation + " must have " + count + " arguments. " + call.arguments.length + " arguments were passed." );
    }
  }
}