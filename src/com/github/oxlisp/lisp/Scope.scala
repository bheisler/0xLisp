package com.github.oxlisp.lisp

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import com.github.oxlisp.assembly.Values.Value
import com.github.oxlisp.assembly.AssemblyScope
import com.github.oxlisp.assembly.AssemblyScope

class Scope(val parent: Option[Scope], val asmScope: AssemblyScope ) {
    
  private val variableSet = HashSet[Var]();
  
  private val callMap = HashMap[String, Procedure]()
  
  val labels = HashMap[String, Code]()
  
  private val labelCount = new AtomicInteger
  
  def getVariable( v: Var ) : Option[Value] = {
    if ( variableSet.contains(v) ) {
      return Some( asmScope.getVariable(this, v) );
    }
    else parent.flatMap( _.getVariable( v ) )
  }
  
  def getProcedure( name:String ) : Option[Procedure] = callMap.get( name ).orElse( parent.flatMap{ _.getProcedure( name ) } )
  
  def getLabel( target: String ) : Option[Code] = labels.get( target ).orElse( parent.flatMap{ _.getLabel( target ) } )
  
  def putVariable( v: Var ) = {
    variableSet += v
    asmScope.putLocalVar(this, v)
  }
  
  def freeVariable( v: Var ) = asmScope.freeLocalVar(this, v)
  
  def putParam( v: Var ) = {
    variableSet += v
    asmScope.putParam( v )
  }
  
  def prepareParam = asmScope.prepareParam;
  def freePrepared = asmScope.freePrepared
  def freePrepared( num: Int ) = asmScope.freePrepared( num )
      
  def putLabels( labels: TraversableOnce[(String, Code)]) = this.labels ++= labels
  
  def newSubScope = new Scope( Some(this), asmScope )
  
  def newAssemblyScope = Scope.defaultScope
  
  def nextLabelCount = labelCount.incrementAndGet
}

object Scope {
  def defaultScope : Scope = {
    val scope = new Scope( None, new AssemblyScope )
    scope.callMap ++= NumericPrimitives.callMap
    scope.callMap ++= HeapPrimitives.callMap
    scope
  }
}