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
  case class MLI( override val a: Value, override val b: Value ) extends BasicInstruction( "MLI", a, b )
  case class DIV( override val a: Value, override val b: Value ) extends BasicInstruction( "DIV", a, b )
  case class DVI( override val a: Value, override val b: Value ) extends BasicInstruction( "DVI", a, b )
  case class MOD( override val a: Value, override val b: Value ) extends BasicInstruction( "MOD", a, b )
  case class MDI( override val a: Value, override val b: Value ) extends BasicInstruction( "MDI", a, b )
  case class SHL( override val a: Value, override val b: Value ) extends BasicInstruction( "SHL", a, b )
  case class SHR( override val a: Value, override val b: Value ) extends BasicInstruction( "SHR", a, b )
  case class ASR( override val a: Value, override val b: Value ) extends BasicInstruction( "ASR", a, b )
  case class AND( override val a: Value, override val b: Value ) extends BasicInstruction( "AND", a, b )
  case class BOR( override val a: Value, override val b: Value ) extends BasicInstruction( "BOR", a, b )
  case class XOR( override val a: Value, override val b: Value ) extends BasicInstruction( "XOR", a, b )
  case class IFE( override val a: Value, override val b: Value ) extends BasicInstruction( "IFE", a, b )
  case class IFN( override val a: Value, override val b: Value ) extends BasicInstruction( "IFN", a, b )
  case class IFG( override val a: Value, override val b: Value ) extends BasicInstruction( "IFG", a, b )
  case class IFA( override val a: Value, override val b: Value ) extends BasicInstruction( "IFA", a, b )
  case class IFL( override val a: Value, override val b: Value ) extends BasicInstruction( "IFL", a, b )
  case class IFU( override val a: Value, override val b: Value ) extends BasicInstruction( "IFU", a, b )
  case class IFB( override val a: Value, override val b: Value ) extends BasicInstruction( "IFB", a, b )
  case class IFC( override val a: Value, override val b: Value ) extends BasicInstruction( "IFC", a, b )
  case class ADX( override val a: Value, override val b: Value ) extends BasicInstruction( "ADX", a, b )
  case class SBX( override val a: Value, override val b: Value ) extends BasicInstruction( "SBX", a, b )
  case class STI( override val a: Value, override val b: Value ) extends BasicInstruction( "STI", a, b )
  case class STD( override val a: Value, override val b: Value ) extends BasicInstruction( "STD", a, b )
  
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
  case class INT( override val a: Value ) extends SpecialInstruction( "INT", a )
  case class IAG( override val a: Value ) extends SpecialInstruction( "IAG", a )
  case class IAS( override val a: Value ) extends SpecialInstruction( "IAS", a )
  case class RFI( override val a: Value ) extends SpecialInstruction( "RFI", a )
  case class IAQ( override val a: Value ) extends SpecialInstruction( "IAQ", a )
  case class HWN( override val a: Value ) extends SpecialInstruction( "HWN", a )
  case class HWQ( override val a: Value ) extends SpecialInstruction( "HWQ", a )
  case class HWI( override val a: Value ) extends SpecialInstruction( "HWI", a )
}