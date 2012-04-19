package com.github.oxlisp.lisp

sealed trait  LispElement {
  
}

sealed trait Expr extends LispElement

case class Call( val operation: String, val arguments: List[Expr] ) extends Expr
case class Num( val v: Int ) extends Expr
case class Str( val v: String ) extends Expr

case class Def( val name: String, val args: List[Var], val defn: Expr ) extends LispElement
case class DefAsm( val name:String, val args: List[Var], val defn: String ) extends LispElement
case class Var( val name: String ) extends Expr

case class Let( val variables : List[(Var, Expr)], body: Expr ) extends Expr

case class Comment( val text: String ) extends LispElement