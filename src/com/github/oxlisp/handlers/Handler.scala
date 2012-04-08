package com.github.oxlisp.handlers
import com.github.oxlisp._

trait Handler {

  def handleTree( tree: List[Element], depth: Int ) = {
    tree.foreach { x => handleElement(x, depth) }
  }
  
  def handleElement( elem: Element, depth: Int )
}