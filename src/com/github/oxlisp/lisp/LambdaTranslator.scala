package com.github.oxlisp.lisp

import scala.collection.mutable.ListBuffer
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.HashMap

object LambdaTranslator {
    def apply( tree: List[LispElement] ) = new LambdaTranslator( ).translate( tree )
    
    val labelCounter = new AtomicInteger
    
    def nextLabel = "f" + labelCounter.getAndIncrement
}
class LambdaTranslator( ) {
  
  val undefinedVars = ListBuffer[Var]()
  
  val definedVars = ListBuffer[Var]()
  
  val labels = HashMap[String, Code]()
  
  def translate( tree: List[LispElement] ) : List[LispElement] = tree.map { translateElement }
  
  def translateElement( element: LispElement ) = {
   val translated = element match {
      case x: Expr => translateExpr( x )
      case x => x
    } 
   if ( translated.isInstanceOf[Expr] && !labels.isEmpty ) {
     val labelMap = labels.toMap
     labels.clear
     Labels( labelMap, translated.asInstanceOf[Expr] )
   }
   else translated
  }
  
  def translateArgs( args: List[Expr] ) = args.map{ translateExpr }
  
  def translateExpr( element: Expr ) : Expr = element match {
    case Call( target, args ) => Call( target, translateArgs( args ) )
    case FunCall( closure, args ) => FunCall( translateExpr( closure ), translateArgs( args ) )
    case x: Var => { foundVar( x ); x}
    case Let( vars, body ) => { 
      val tVars = vars.map { x => ( (x._1, translateExpr( x._2 ) ) ) };
      Let( tVars, inSubScope( body, tVars.map { _._1 } )._2 )
    }
    case If( test, then, els ) => If( translateExpr(test), translateExpr(then), translateExpr( els ))
    case x: Lambda => handleLambda( x ) 
    case x => x
  }
  
  def inSubScope( expr: Expr, predefs: List[Var] ) = {
    val subTranslator = new LambdaTranslator( )
    predefs.foreach{ subTranslator.defineVar( _ ) };
    val translated = subTranslator.translateExpr( expr )
    subTranslator.undefinedVars foreach{ foundVar( _ ) }
    labels ++= subTranslator.labels
    
    ( subTranslator.undefinedVars, translated )
  }
  
  def handleLambda( lambda: Lambda ) = {
    val ( undefinedVars, translated ) = inSubScope( lambda.defn, lambda.params )
    
    val code = Code( undefinedVars.toList ::: lambda.params, translated )
    val label = LambdaTranslator.nextLabel
    
    labels += ( label -> code )
    
    Closure( label, undefinedVars.toList )
  }
  
  def foundVar( v: Var ) {
    if ( !definedVars.contains( v ) ) {
      undefinedVars += v
    }
  }
  
  def defineVar( v: Var ) {
    if ( undefinedVars.contains( v ) ) {
      undefinedVars -= v
    }
    definedVars += v
  }
}