package com.github.oxlisp.lisp

import com.github.oxlisp.lisp.Types._
import com.github.oxlisp.assembly.Instructions._

abstract class Procedure( val args: List[Type], val ret: Type ) {
  
  def implementation() : List[Instruction] = shortImpl :: Nil
  
  protected def shortImpl : Instruction = throw new RuntimeException
}

abstract class Test( val arg: Type ) extends Procedure( List(arg), BOOL )