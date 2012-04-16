package com.github.oxlisp.assembly

object Values {
  trait AsmElement

  sealed trait Value extends AsmElement

  sealed case class Register( val name: String ) extends Value {
    override def toString = name
  }

  case object A extends Register( "A" )
  case object B extends Register( "B" )
  case object C extends Register( "C" )
  case object I extends Register( "I" )
  case object J extends Register( "J" )
  case object X extends Register( "X" )
  case object Y extends Register( "Y" )
  case object Z extends Register( "Z" )

  case class Literal( private val value : Int ) extends Value {
    override def toString = value.toString
  }
  
  implicit def short2Literal( value: Int ) : Literal = {
    if ( ( value < 0 ) || ( value > 65535 ) ) { 
      throw new IllegalArgumentException
    }
    Literal( value )
  }
 
  case object Pop extends Value {
    override def toString = "POP"
  }
  
  case object Push extends Value {
    override def toString = "PUSH"
  }
  
  case object Peek extends Value {
    override def toString = "PEEK"
  }
  
  case object SP extends Value {
    override def toString = "SP"
  }
  
  case object PC extends Value {
    override def toString = "PC"
  }
  
  case object O extends Value {
    override def toString = "O"
  }
  
  case class Offset( val value: Literal, val reg: Register ) {
    override def toString = "[" + value + "+" + reg + "]"
  }
  
  case class WordAt( private[Values] val value : Value ) extends Value {
    override def toString = "[" + value + "]"
  }
  object WordAt {
    def apply( reg: Register ) = new WordAt( reg )
    def apply( lit: Literal ) = new WordAt( lit )
  }
  
  case class Label( val name: String ) extends Value {
    override def toString = name
  }
  
  case class LocalLabel( override val name: String ) extends Label( name ){
    override def toString = "." + super.toString
  }
}