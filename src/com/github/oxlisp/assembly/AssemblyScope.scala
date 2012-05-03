package com.github.oxlisp.assembly

import scala.collection.mutable.HashMap
import com.github.oxlisp.assembly.Values._
import com.github.oxlisp.assembly.Instructions._
import com.github.oxlisp.lisp.Var
import com.github.oxlisp.lisp.Scope

class AssemblyScope {

  private val localVariableMap = HashMap[(Scope, Var), Offset]()
  
  private val parameterMap = HashMap[Var, Value]()
  
  private var preparedParams = 0;
  
  def getVariable( scope: Scope, variable: Var ) : Value = localVariableMap.get( (scope, variable ) ).orElse( parameterMap.get( variable ) ).get
  
  def putLocalVar( scope: Scope, v: Var ) : Value = {
    val offset = getNextLVarOffset
    localVariableMap += ( (scope, v) -> offset )
    offset
  }
  
  def freeLocalVar( scope: Scope, v: Var ) {
    localVariableMap -= ( (scope, v) )
  }
  
  def putParam( v: Var ) = {
    val value = getNextParamValue;
    parameterMap += ( v -> value )
    value
  }
  
  def prepareParam : Unit = {
    preparedParams += 1
    putLocalVar( null, Var( "param" + preparedParams ) )
  }
  
  def freePrepared() : Unit = {
    freeLocalVar( null, Var( "param" + preparedParams ) )
    preparedParams -= 1
  }
  
  def freePrepared( num: Int ) : Unit = {
    0 to num foreach( x => freePrepared() )
  }
  
  private def getNextParamValue = parameterMap.size match {
    case 0 => A
    case 1 => B
    case 2 => C
    case x => Offset( (x - 1).toShort, J )
  }
  
  private def getNextLVarOffset = {
    if ( localVariableMap.isEmpty ) Offset( -1, J )
    else Offset( (localVariableMap.values.map{ _.value }.min - 1 ).toShort, J )
  }
}