package com.github.oxlisp

sealed trait Element

sealed trait Expr extends Element

case class Call( val operation: String, val arguments: List[Expr] ) extends Expr
case class Num( val v: Short ) extends Expr
case class Str( val v: String ) extends Expr

case class Def( val name: String, val args: List[Var], val defn: Expr ) extends Element
case class Var( val name: String ) extends Expr

case class Comment( val text: String ) extends Element