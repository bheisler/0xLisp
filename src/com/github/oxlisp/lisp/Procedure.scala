package com.github.oxlisp.lisp

import com.github.oxlisp.lisp.Types._
import com.github.oxlisp.assembly.Instructions._
import com.github.oxlisp.assembly.Values._

abstract class Procedure( val args: List[Type], val ret: Type ) {
  
  def implementation : List[Instruction] = shortImpl :: Nil
  
  protected def shortImpl : Instruction = throw new RuntimeException
}

abstract class Test( override val args: List[Type] ) extends Procedure( args, BOOL )

class LabelCall( val name: String, val argCount: Int ) extends Procedure( List.fill(argCount)(ANY), ANY ) {
  override def shortImpl = JSR( Label( name ) ) 
}