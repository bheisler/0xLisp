package com.github.oxlisp.lisp

object Types {
  sealed trait Type {
    def matches( other: Type ) : Boolean = {
      other.eq( this ) || other.eq(ANY)
    }
  }
  
  case object INT extends Type
  case object BOOL extends Type
  
  case object VOID extends Type
  
  case object ANY extends Type {
    override def matches( other: Type ) : Boolean =  {
      true
    }
  }
  
  case class CONS( val left: Type, val right: Type ) extends Type
}