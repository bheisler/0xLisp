package com.github.oxlisp.lisp

import scala.collection.mutable.Map
import com.github.oxlisp.assembly.Values.Value
import com.github.oxlisp.assembly.Instructions._

class Scope(val parent: Option[Scope]) {

  private var variableMap: Map[Var, Value] = Map()
  
  private var callMap: Map[String, Procedure] = Map()
  
  def getVariable( v: Var ) : Option[Value] = variableMap.get(v).orElse{ parent.flatMap{ x => x.getVariable( v ) } }
  
  def getProcedure( name:String ) : Option[Procedure] = callMap.get( name ).orElse( parent.flatMap{ _.getProcedure( name ) } )
  
  def putVariable( v: Var, value: Value ) = variableMap.put(v, value)
  
  def nextOffset : Short = ( -( variableMap.size + 1 ) ).toShort
  
  def newSubScope = new Scope( Some(this) )
}

object Scope {
  def defaultScope : Scope = {
    val scope = new Scope( None )
    scope.callMap = scope.callMap ++ NumericPrimitives.callMap
    scope
  }
}