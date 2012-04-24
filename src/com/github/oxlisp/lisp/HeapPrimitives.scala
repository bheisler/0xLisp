package com.github.oxlisp.lisp

import com.github.oxlisp.lisp.Types._
import com.github.oxlisp.assembly.Instructions._
import com.github.oxlisp.assembly.Values._

object HeapPrimitives {

  val callMap = Map[String, Procedure] (
    ( "cons" -> cons ),
    ( "head" -> head ),
    ( "tail" -> tail ) )
      
  def cons = new Procedure( List( ANY, ANY ), CONS( ANY, ANY ) ) { //TODO: Really need a better type checking system.
    override def implementation = List(
      SET( PUSH, A ),
      SET( A, 2 ),
      JSR( Label( "malloc" ) ),
      SET( WordAt( A ), POP ),
      SET( Offset( 1, A ), B ) )    
  }
  
  def tail = new Procedure( List( CONS( ANY, ANY ) ), ANY ) {
    override def shortImpl = SET( A, Offset( 1, A ) )
  }
  
  def head = new Procedure( List( CONS( ANY, ANY ) ), ANY ) {
    override def shortImpl = SET( A, WordAt( A ) )
  }
}