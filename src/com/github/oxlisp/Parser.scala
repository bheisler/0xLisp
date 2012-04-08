package com.github.oxlisp
import scala.util.parsing.combinator._
import com.sun.corba.se.spi.orbutil.fsm.Input
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import java.io.Reader

object Parser {
  
  object LispParsers extends JavaTokenParsers {
    
    class Wrap[+T](name:String,parser:Parser[T]) extends Parser[T] {
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
    
    def num: Parser[Num] = wholeNumber ^^ { x => Num( x.toInt.toShort ) }
    def str: Parser[Str] = stringLiteral ^^ { x => Str( x.substring( 1, x.length - 1 ) ) }
        
    def defName: Parser[String] = """[a-zA-Z_+-\\%\\*/]\w*""".r
    
    def variable: Parser[Var] = ident ^^ { x => Var( x ) }
    
    def call: Parser[Call] = "(" ~ defName ~ rep( expr ) ~ ")" ^^
    	{ case "("~ident~expressions~")" => Call( ident, expressions ) }
    
    def expr: Parser[Expr] = num | str | variable | call
    
    def defn: Parser[Def] = "(def" ~ defName ~ "[" ~ rep( variable) ~ "]" ~ expr ~ ")" ^^
    { case "(def"~name~"["~args~"]"~expr~")" => Def(name, args, expr) }
    
    def comment: Parser[Comment] = """;.*""".r ^^ { x => Comment( x.replace( ";", "" ) ) } 
    
    def elem: Parser[Element] = comment | defn | expr
      
    def program: Parser[List[Element]] = rep( elem )
    
    def parse( text: String ) : List[Element] = {
      parseAll( program, text ).get
    }
    
    def parse( input: Reader ) : List[Element] = {
      val result = parseAll( program, input )
      input.close()
      result.get
    }
  }
}