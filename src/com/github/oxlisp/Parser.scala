package com.github.oxlisp
import scala.util.parsing.combinator._
import com.sun.corba.se.spi.orbutil.fsm.Input
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import java.io.Reader

object Parser {
  
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
    
    class DefNameParser extends Parser[String] {
      val letters = "abcdefghijklmnopqrstuvwxyz";
      val numbers = "0123456789";
      val symbols = "!@#$%^&*?";
      val allowed = letters + letters.toUpperCase() + numbers + symbols;
      
      def apply( in: Input ) : ParseResult[String] = {
        in.first match {
          case x if allowed.contains( x ) => apply( in.rest ).map( x + _ )
          case x if x.isWhitespace => Success( "", in.rest )
          case x => Failure( x + " is not a valid character for an operation", in.rest )
        }
      }
    }
    
    def num: Parser[Num] = wholeNumber ^^ { x => Num( x.toInt.toShort ) }
    def str: Parser[Str] = stringLiteral ^^ { x => Str( x.substring( 1, x.length - 1 ) ) }
        
    def defName: Parser[String] = new DefNameParser()
    
    def variable: Parser[Var] = ident ^^ { x => Var( x ) }
    
    def call: Parser[Call] = "(" ~ defName ~ rep( expr ) ~ ")" ^^
    	{ case "("~ident~expressions~")" => Call( ident, expressions ) }
    
    def expr: Parser[Expr] = num | str | variable | call
    
    def defn: Parser[Def] = "(def" ~ defName ~ "[" ~ rep( variable) ~ "]" ~ expr ~ ")" ^^
    { case "(def"~name~"["~args~"]"~expr~")" => Def(name, args, expr) }
    
    def comment: Parser[Comment] = """;.*""".r ^^ { x => Comment( x.replace( ";", "" ) ) } 
    
    def elem: Parser[Element] = comment | defn | expr
      
    def program: Parser[List[Element]] = new Wrap( rep( elem ) )
    
    def parse( text: String ) : List[Element] = {
      parseAll( program, text ).get
    }
    
    def parse( input: Reader ) : Option[List[Element]] = {
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
}