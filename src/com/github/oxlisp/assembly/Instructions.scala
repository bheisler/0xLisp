package com.github.oxlisp.assembly

import com.github.oxlisp.assembly.Values._

object Instructions {

  sealed trait Instruction extends AsmElement
  
  sealed class BasicInstruction( val name: String, val a: Value, val b: Value ) extends Instruction {
    override def toString = name + " " + a + ", " + b
  }
  
  case class SET( override val a: Value, override val b: Value ) extends BasicInstruction( "SET", a, b )
  case class ADD( override val a: Value, override val b: Value ) extends BasicInstruction( "ADD", a, b )
  case class SUB( override val a: Value, override val b: Value ) extends BasicInstruction( "SUB", a, b )
  case class MUL( override val a: Value, override val b: Value ) extends BasicInstruction( "MUL", a, b )
  case class DIV( override val a: Value, override val b: Value ) extends BasicInstruction( "DIV", a, b )
  case class MOD( override val a: Value, override val b: Value ) extends BasicInstruction( "MOD", a, b )
  case class SHL( override val a: Value, override val b: Value ) extends BasicInstruction( "SHL", a, b )
  case class SHR( override val a: Value, override val b: Value ) extends BasicInstruction( "SHR", a, b )
  case class AND( override val a: Value, override val b: Value ) extends BasicInstruction( "AND", a, b )
  case class BOR( override val a: Value, override val b: Value ) extends BasicInstruction( "BOR", a, b )
  case class XOR( override val a: Value, override val b: Value ) extends BasicInstruction( "XOR", a, b )
  case class IFE( override val a: Value, override val b: Value ) extends BasicInstruction( "IFE", a, b )
  case class IFN( override val a: Value, override val b: Value ) extends BasicInstruction( "IFN", a, b )
  case class IFG( override val a: Value, override val b: Value ) extends BasicInstruction( "IFG", a, b )
  case class IFB( override val a: Value, override val b: Value ) extends BasicInstruction( "IFB", a, b )
  
  case class DefineLabel( val name: String ) extends Instruction {
    override def toString = name + ":"
  }
  
  case class DefineLocalLabel( override val name: String ) extends DefineLabel( name ) {
    override def toString = "." + super.toString
  }
  
  sealed class SpecialInstruction( val name: String, val a: Value ) extends Instruction {
    override def toString = name + " " + a
  }
  
  case class JSR( override val a: Value ) extends SpecialInstruction( "JSR", a )
}