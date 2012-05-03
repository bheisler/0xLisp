package com.github.oxlisp.lisp

class AstPrinter( ) {
    
  def handleTree( tree: List[LispElement], depth: Int ) : Unit = {
    tree.foreach { x => handleElement(x, depth) }
  }
  
  def handleElement( elem: LispElement, depth: Int ) : Unit = elem match {
    case x : Num => handleNum( x, depth )
    case x : Str => handleStr( x, depth )
    case call : Call => handleCall( call, depth )
    case comment : Comment => handleComment( comment )
    case v: Var => handleVar( v, depth )
    case let: Let => handleLet( let, depth )
    case cond: If => handleIf( cond, depth )
    case lambda: Lambda => handleLambda( lambda, depth )
    case funCall: FunCall => handleFunCall( funCall, depth )
  }
  
  val indentStr = "  "
  
  def printIndent(depth: Int) {
    print( indentStr * depth )
  }
    
  def handleNum( num: Num, depth: Int ) = {
    printIndent( depth )
    println( num.v )
  }
  
  def handleStr( str: Str, depth: Int ) = {
    printIndent( depth )
    println( " \"" + str.v + "\"" )
  }
  
  def handleVar( v: Var, depth:Int ) = {
    printIndent( depth )
    println( v.name )
  }
  
  def handleCall( call: Call, depth: Int ) = {
    printIndent( depth )
    println( "Call " + call.operation + "(" )
    handleTree(call.arguments, depth + 1 )
    printIndent( depth )
    println( ")" )
  }
  
  def handleComment( comment: Comment ) = {
    println( "//" + comment.text )
  }
  
  def handleLet( let: Let, depth: Int ) {
    print( "Let:" )
    let.variables.foreach { x=> print( x._1.name + "=" + x._2 + " " ) }
    print( "\n" )
    handleElement(let.body, depth + 1)
  }
  
  def handleIf( cond: If, depth: Int ) {
    printIndent( depth )
    print( "If:" )
    handleElement( cond.test, depth + 1 )
    printIndent( depth )
    print( "Then: " )
    handleElement( cond.conseq, depth + 1 )
    printIndent( depth )
    print( "Else: " )
    handleElement( cond.altern, depth + 1 )
  }
  
  def handleLambda( lambda: Lambda, depth: Int ) {
    printIndent(depth)
    print( "Lambda" )
    print( "(" + lambda.params.mkString(", ") + ")\n" )
    handleElement( lambda.defn, depth + 1)
  }
  
  def handleFunCall( funCall: FunCall, depth: Int ) {
    printIndent(depth)
    print( "Closure call to " + funCall.closure + "\n" )
    printIndent(depth)
    print( "Args:\n" )
    handleTree(funCall.args, depth + 1 )
  }
}