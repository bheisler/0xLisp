package com.github.oxlisp.lisp

class AstPrinter( ) {
    
  def handleTree( tree: List[LispElement], depth: Int ) : Unit = {
    tree.foreach { x => handleElement(x, depth) }
  }
  
  def handleElement( elem: LispElement, depth: Int ) : Unit = elem match {
    case x : Num => handleNum( x, depth )
    case x : Str => handleStr( x, depth )
    case call : Call => handleCall( call, depth )
    case defn : Def => handleDef( defn, depth )
    case defAsm: DefAsm => handleDefAsm( defAsm, depth )
    case comment : Comment => handleComment( comment )
    case v: Var => handleVar( v, depth )
    case let: Let => handleLet( let, depth )
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
  
  def handleDef( defn: Def, depth: Int ) = {
    printIndent( depth )
    print( "Define " + defn.name + " " )
    print( "[" )
    print( defn.args.map{ _.name }.mkString( ", " ) )
    println( "]" )
    handleElement( defn.defn, depth + 1 )
    println( "\n" )
  }
  
  def handleComment( comment: Comment ) = {
    println( "//" + comment.text )
  }
  
  def handleDefAsm( defn: DefAsm, depth: Int ) {
    print( "Define " + defn.name + " " )
    print( "[" )
    print( defn.args.map{ _.name }.mkString( ", " ) )
    println( "]" )
    print( defn.defn );
    println( "\n")
  }
  
  def handleLet( let: Let, depth: Int ) {
    print( "Let:" )
    let.variables.foreach { x=> print( x._1.name + "=" + x._2 + " " ) }
    handleElement(let.body, depth + 1)
  }
}