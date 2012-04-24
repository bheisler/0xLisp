package com.github.oxlisp.lisp

import com.github.oxlisp.assembly.Instructions._
import com.github.oxlisp.assembly.Values._
import com.github.oxlisp.lisp.Types._

object NumericPrimitives {

  val callMap = Map[String, Procedure] (
    ( "add1" -> add1 ),
    ( "sub1" -> sub1 ),
    ( "not" -> not ), 
    ( "+" -> add ),
    ( "-" -> subtract ),
    { "/" -> divide },
    { "%" -> mod },
    { "*" -> mult },
    ( "zero?" -> isZero ),
    ( "null?" -> isZero ),
    ( "even?" -> isEven ),
    ( "odd?" -> isOdd ),
    ( "=" -> isEqual ),
    ( "<" -> isLesser ),
    ( ">" -> isGreater ),
    ( "<=" -> isLesserEqual ),
    ( ">=" -> isGreaterEqual ) )
  
  def add1 : Procedure = new Procedure( List( INT ), INT ){
    override def shortImpl = ADD( A, 1 )
  }
    
  def sub1 : Procedure = new Procedure( List( INT ), INT ){
    override def shortImpl = SUB( A, 1 )
  }
    
  def not : Procedure = new Procedure( List( INT ), INT ){
    override def shortImpl = XOR( A, 0xFFFF )
  }
  
  def add : Procedure = new Procedure( List( INT, INT ), INT ){
    override def shortImpl = ADD( A, B )
  }
  
  def subtract : Procedure = new Procedure( List( INT, INT ), INT ){
    override def shortImpl = SUB( A, B )
  }
  
  def divide : Procedure = new Procedure( List( INT, INT ), INT ){
    override def shortImpl = DIV( A, B )
  }
  
  def mult : Procedure = new Procedure( List( INT, INT ), INT ){
    override def shortImpl = MUL( A, B )
  }
  
  def mod : Procedure = new Procedure( List( INT, INT ), INT ){
    override def shortImpl = MOD( A, B )
  }
  
  def isZero : Test = new Test( List( INT ) ) {
    override def shortImpl = IFE( A, 0 )
  }
  
  def isOdd : Test = new Test( List( INT ) ) {
    override def implementation = List( MOD( A, 2 ), IFE( A, 1 ) )
  }
  
  def isEven : Test = new Test( List( INT ) ) {
    override def implementation = List( MOD( A, 2 ), IFE( A, 0 ) )
  }
  
  def isEqual : Test = new Test( List( INT, INT ) ) {
    override def shortImpl = IFE( A, B )
  }
  
  def isGreater : Test = new Test( List( INT, INT ) ) {
    override def shortImpl = IFG( A, B )
  }
  
  def isLesserEqual : Test = new Test( List( INT, INT ) ) {
    override def shortImpl = IFG( B, A )
  }
  
  def isGreaterEqual : Test = new Test( List( INT, INT ) ) {
    override def implementation = List( IFN( A, B ), IFG( A, B ) )
  }
  
  def isLesser : Test = new Test( List( INT, INT ) ) {
    override def implementation = List( IFG( A, B ), IFN( A, B ) )
  }
}