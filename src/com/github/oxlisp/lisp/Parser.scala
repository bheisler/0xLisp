package com.github.oxlisp.lisp
import scala.util.parsing.combinator._
import com.sun.corba.se.spi.orbutil.fsm.Input
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import java.io.Reader

object LispParsers extends JavaTokenParsers {
    
  class Wrap[+T](parser:Parser[T]) extends Parser[T] {
    def apply(in: Input): ParseResult[T] = {
      val first = in.first
      val pos = in.pos
      val offset = in.offset
      val t = parser.apply(in)
      if ( t.isInstanceOf[NoSuccess] ) {
        println( t )
        val failure = t.asInstanceOf[NoSuccess]
        Failure( "Failed to parse token " + first + " at position " + pos + " offset " + offset + ". Error Message: " + failure.msg, in.rest )
      }
      else {
        t
      }
    }
  }
   
  class Debug[+T](name:String,parser:Parser[T]) extends Parser[T] {
    def apply(in: Input): ParseResult[T] = {
      val first = in.first
      val pos = in.pos
      val offset = in.offset
      val t = parser.apply(in)
      println(name+".apply for token "+first+
        " at position "+pos+" offset "+offset+" returns "+t)
      t
    }
  }
  
  implicit def toDebug( name: String ) = new {
    def !!![T](p: Parser[T]) = new Debug( name, p)
  }
  
  class DefNameParser extends Parser[String] {
    val letters = "abcdefghijklmnopqrstuvwxyz";
    val numbers = "0123456789";
    val symbols = "-+/*!@#$%^&?";
    val allowed = letters + letters.toUpperCase() + numbers + symbols;
    
    def discardWhitespace( in: Input ): Input = {
      in.first match {
        case x if x.isWhitespace => discardWhitespace( in.rest )
        case x => in
      }
    }
    
    def getDefName ( in: Input ) : ParseResult[String] = {
      in.first match {
        case x if allowed.contains( x ) => getDefName( in.rest ).map( x + _ )
        case x if x.isWhitespace => Success( "", in.rest )
        case x => Failure( x + " is not a valid character for an operation", in.rest )
      }
    }
    
    def apply( in: Input ) : ParseResult[String] = {
        getDefName( discardWhitespace( in ) ) 
    }
  }
  
  //This just copies the string until it reaches a close paren. This will be
  //replaced with an actual parser whenever I get round to building an assembler.
  class DefAsmParser extends Parser[String] {
    def apply( in: Input ) : ParseResult[String] = {
      
      in.first match{ 
        case x if x.equals( ')' ) => Success( "", in )
        case x => apply( in.rest ).map( x + _ )
      }
    }
  }
  
  class NumberLiteralParser extends Parser[Num] {
    val delegate = wholeNumber ^^ { x => Num( x.toInt ) }
    
    def apply( in: Input ) : ParseResult[Num] = {
      delegate( in ) match {
        case Success( Num(x), next ) if ( x >= 0 && x <= 65535 ) => Success( Num( x ), next )
        case Success( Num(x), next ) => Failure( "Integer literals must be between 0 and 65535. Value was " + x, next )
        case x => x
      }
    }
  }
  
  def num: Parser[Num] = new NumberLiteralParser()
  def str: Parser[Str] = stringLiteral ^^ { x => Str( x.substring( 1, x.length - 1 ) ) }
        
  def defName: Parser[String] = new DefNameParser()
    
  def asm: Parser[String] = new DefAsmParser()
    
  def variable: Parser[Var] = ident ^^ { x => Var( x ) }
  
  def letVariable: Parser[(Var, Expr)] = "(" ~ variable ~ expr ~ ")" ^^
  { case "("~variable~expression~")" => (variable, expression) }
    
  def call: Parser[Call] = "(" ~ defName ~ rep( expr ) ~ ")" ^^
  { case "("~ident~expressions~")" => Call( ident, expressions ) }
    
  def let: Parser[Let] = "(let" ~ "(" ~ rep( letVariable ) ~ ")" ~ expr ~ ")" ^^
  { case "(let"~"("~variables~")"~expression~")" => Let( variables, expression ) }
  
  def condIf: Parser[If] = "(if"~call~expr~expr~")" ^^
  { case "(if"~test~conseq~altern~")" => If( test, conseq, altern ) }
  
  def funCall: Parser[FunCall] = "(funcall"~expr~rep(expr)~")" ^^
  { case "(funcall"~closure~args~")" => FunCall( closure, args ) }
  
  def lambda: Parser[Lambda] = "(lambda"~"("~rep(variable)~")"~expr~")" ^^
  { case "(lambda"~"("~params~")"~defn~")" => Lambda( params, defn ) }
  
  def expr: Parser[Expr] =  let | condIf | num | str | variable | funCall | lambda | call
  
  def comment: Parser[Comment] = """;.*""".r ^^ { x => Comment( x.replace( ";", "" ) ) }
  
  def elem: Parser[LispElement] = comment | expr
    
  def program: Parser[List[LispElement]] = new Wrap( rep( elem ) )
  
  def parse( text: String ) : List[LispElement] = {
    parseAll( program, text ).get
  }
  
  def parse( input: Reader ) : Option[List[LispElement]] = {
    val result = parseAll( program, input )
    input.close()
    if ( result.isEmpty ) {
      System.err.println( result )
      None
    }
    else {
      Some(result.get)
    }
  }
}