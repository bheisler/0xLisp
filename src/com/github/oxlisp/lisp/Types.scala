package com.github.oxlisp.lisp

object Types {
  sealed trait Type {
    def matches( other: Type ) : Boolean = {
      other.eq( this ) || other.eq(UNKNOWN)
    }
  }
  
  case object INT extends Type
  case object BOOL extends Type
  
  case object VOID extends Type
  
  case object UNKNOWN extends Type {
    override def matches( other: Type ) : Boolean =  {
      true
    }
  }
}