package com.github.oxlisp.handlers
import com.github.oxlisp._
import scala.collection.immutable.List

trait Handler {
  
  var errors: List[String] = Nil

  def handleTree( tree: List[Element], depth: Int ) = {
    tree.foreach { x => handleElement(x, depth) }
  }
  
  def handleElement( elem: Element, depth: Int )
  
  def emitError( error: String ) {
    errors = error :: errors
  }
  
}