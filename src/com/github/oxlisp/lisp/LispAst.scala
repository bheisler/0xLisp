package com.github.oxlisp.lisp

sealed trait  LispElement {
  
}

sealed trait Expr extends LispElement

case class Call( val operation: String, val arguments: List[Expr] ) extends Expr
case class FunCall( val closure: Expr, val args: List[Expr] ) extends Expr
case class Num( val v: Int ) extends Expr
case class Str( val v: String ) extends Expr

case class Var( val name: String ) extends Expr

case class Let( val variables : List[(Var, Expr)], body: Expr ) extends Expr

case class If( val test: Expr, val conseq: Expr, val altern: Expr ) extends Expr

case class Comment( val text: String ) extends LispElement

//The following are objects which the compiler can handle but which are not parsed, since they are constructed from other elements.
case class Closure( val target: String, val captures: List[Var] ) extends Expr
case class Code( val args: List[Var], val body: Expr) extends LispElement
case class Labels( val labels: Map[String, Code], val defn: Expr ) extends LispElement
//case class CodeAsm( val args: List[Var], val defn: String ) extends LispElement //TODO: Build an ASM parser and replace this string definition.

//The following are objects which are parsed but which the compiler cannot handle since they are converted to the above.
case class Lambda( val params: List[Var], val defn: Expr ) extends Expr