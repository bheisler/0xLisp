package com.github.oxlisp.lisp

import com.github.oxlisp.assembly.Instructions._
import com.github.oxlisp.assembly.Values._
import Types._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

class Compiler(val scope: Scope) {
    
  val errors = ListBuffer[String]()
    
  def emitError( error: String ) {
    errors += error
  }

  def compile( tree: List[LispElement] ) : List[Instruction] = {
    val compiled = new ListBuffer[Instruction]
    
    for ( element <- tree ) {
      val evalElement = handleElement( element )
      compiled ++= evalElement
    }
    compiled += SUB( PC, 1 )
    for ( (name, code) <- scope.labels ) {
      compiled ++= DefineLabel( name ) :: handleElement( code )
    }
    compiled.toList
  }
  
  def handleElement( elem: LispElement ) : List[Instruction] = elem match {
    case num: Num => handleNum( num )
    case call : Call => handleCall( call )
    case v: Var => handleVar( v )
    case let : Let => inNewScope{ _.handleLet( let ) }
    case code: Code => inNewAssemblyScope{ _.handleCode( code ) }
    case labels: Labels => inNewScope{ _.handleLabels( labels) }
    case funcall: FunCall => handleFunCall( funcall )
    case closure: Closure => handleClosure( closure )
    case cond: If => handleIf( cond )
    case comment: Comment => Nil
    case x => emitError( "Unknown element: " + x ); Nil
  }
  
  def inNewScope( call: Compiler => List[Instruction] ) : List[Instruction] = {
    val child = new Compiler( scope.newSubScope );
    val instructions = call( child );
    scope.putLabels(child.scope.labels)
    errors ++= child.errors
    instructions
  }
  
  def inNewAssemblyScope( call: Compiler => List[Instruction] ) : List[Instruction] = {
    val child = new Compiler( scope.newAssemblyScope );
    val instructions = call( child );
    scope.putLabels(child.scope.labels)
    errors ++= child.errors
    List( SET( PUSH, J ), SET( J, SP ) ) ::: instructions ::: List( SET( SP, J ), SET( J, POP ), SET( PC, POP ) )
  }
  
  def handleNum( num: Num ) : List[Instruction] = {
    SET( A, num.v ) :: Nil
  }
  
  def handleVar( variable: Var ) : List[Instruction] = {
    val offset = scope.getVariable( variable )
    offset match {
      case Some(o) => SET( A, o ) :: Nil
      case None => emitError( "Undefined variable " + variable ); Nil
    }
  }
  
  def getType( expr: Expr ) : Type = expr match {
    case num: Num => INTEGER
    case v: Var => ANY
    case call: Call => scope.getProcedure( call.operation ).map( _.ret ).getOrElse(VOID)
    case let: Let => getType( let.body )
    case closure: Closure => CLOSURE
    case funCall: FunCall => ANY
  }
  
  def validateCall( call: Call ) : Boolean = {
    val optProcedure = scope.getProcedure( call.operation )
    if ( !optProcedure.isDefined ) {
      emitError( "Call to unknown procedure: " + call.operation )
      return false
    }
    
    val procedure = optProcedure.get
    val argumentTypes = call.arguments.map(getType)
    val zipped = procedure.args.zip( argumentTypes )
    if ( !zipped.forall( x => x._1.matches( x._2 ) ) ) {
      emitError( "Procedure " + call.operation + " expects arguments of type: " + procedure.args.mkString(", ") +
          " but found " + argumentTypes.mkString(", ") )
      return false;
    }
    true
  }
  
  def handleCall( call: Call ) : List[Instruction] = {
    if ( validateCall( call ) ) {
      val target = scope.getProcedure(call.operation).get
      val args = handleArgs( call.arguments )
      val doCall = target.implementation
      val cleanup = if ( call.arguments.length > 3 ) {
        scope.freePrepared(call.arguments.length - 3)
        ADD( SP, call.arguments.length - 3 ) :: Nil
      }
      else {
        Nil
      }
      args ::: doCall ::: cleanup
    }
    else {
      Nil
    }
  }
  
  def handleClosure( closure: Closure ) : List[Instruction] = {
      val start = List(
            SET( A, closure.captures.length + 3 ),
            JSR( Label( "malloc" ) ),
            SET( WordAt( A ), closure.captures.length )
      )  
      
      def putCaptures( captures : List[Var] ) : List[Instruction] = captures match {
        case Nil => Nil
        case (head::tail) => putCaptures( tail ) ::: SET( Offset( ( tail.length + 1 ).toShort, A ), scope.getVariable( head ).get ) :: Nil
      }
      val captures = putCaptures( closure.captures )
      
      val code = scope.getLabel( closure.target ).getOrElse{
        emitError( "Could not find label: " + closure.target );
        Code( List(), Num( 0 ) )
      }
      
      val putArgCount = SET( Offset( (closure.captures.length + 1).toShort, A ), code.args.length ) :: Nil
      val putLabel = SET( Offset( (closure.captures.length + 2).toShort, A ), Label( closure.target ) ) :: Nil
      start ::: captures ::: putArgCount ::: putLabel
  }
  
  def handleFunCall( funCall: FunCall ) : List[Instruction] = {
    val reserveVars = SUB( SP, 2 ) :: Nil
    val target = scope.putVariable( Var( "target" ) )
    val argCount = scope.putVariable( Var( "argCount" ) )
    
    val argumentExpressions = funCall.args.reverse.flatMap { handleElement( _ ) ::: List( SET( PUSH, A ) ) }
    val evaluateClosure = handleElement( funCall.closure )
    val loop = List(
            SET( B, WordAt( A ) ),
            IFE( 0, B ),
            ADD( PC, 4 ),
            ADD( A, 1 ),
            SET( PUSH, WordAt( A ) ),
            SUB( B, 1 ),
            SUB( PC, 5 )
        )
    
    val setTarget = List(
            ADD( A, 1 ),
            SET( target, Offset( 1, A ) )
        )
        
    val popInstructions = List(
            SET( argCount, WordAt( A ) ),
            IFG( argCount, 0 ),
            SET( A, POP ),
            IFG( argCount, 1 ),
            SET( B, POP ),
            IFG( argCount, 2 ),
            SET( C, POP )
        )
        
    val call = JSR( target ) :: Nil
    
    val cleanup = ADD( SP, 2 ) :: Nil
    scope.freeVariable( Var( "target" ) )
    scope.freeVariable( Var( "argCount" ) )
        
    reserveVars ::: argumentExpressions ::: evaluateClosure ::: loop ::: setTarget ::: popInstructions ::: call ::: cleanup
  }
  
  def handleIf( cond: If ) : List[Instruction] = {
    //TODO: Change this to use relative jumps rather than labels
    val testInstruction = handleElement( cond.test )
    val thenInst = handleElement( cond.conseq )
    val elseInst = handleElement( cond.altern )
    val thenLabel = LocalLabel( "then" + scope.nextLabelCount )
    val endLabel = LocalLabel( "end" + scope.nextLabelCount );
    testInstruction ::: List( SET( PC, thenLabel ) ) ::: elseInst ::: List( SET( PC, endLabel ), DefineLocalLabel( thenLabel.name ) ) ::: 
      thenInst ::: List( DefineLocalLabel( endLabel.name ) )    
  }
  
  def handleArgs( args: List[Expr] ) : List[Instruction] = {
    val argumentExpressions = args.reverse.flatMap { x =>
        val instructions = handleElement( x ) ::: List( SET( PUSH, A ) )
        scope.prepareParam
        instructions
      }.init
    var popInstructions : List[Instruction] = Nil
    if ( args.length >= 3 ) { scope.freePrepared; popInstructions = SET( C, POP ) :: popInstructions }
    if ( args.length >= 2 ) { scope.freePrepared; popInstructions = SET( B, POP ) :: popInstructions }
    scope.freePrepared
    argumentExpressions ::: popInstructions
  }
      
  def handleLet( let: Let ) : List[Instruction] = {
    val argumentExpressions = let.variables.flatMap{ x => handleLetVariable( x._1, x._2 ) }
    val subexpression = handleElement(let.body) 
    argumentExpressions ::: subexpression
  }
  
  def handleLetVariable( variable: Var, expression: Expr ) : List[Instruction] = {
    scope.putVariable( variable )
    handleElement(expression) ::: List( SET( PUSH, A ) )
  }
  
  def handleCode( code: Code ) : List[Instruction] = {
    code.args.foreach { scope.putParam( _ ) }
    handleElement( code.body )
  }
  
  def handleLabels( labels: Labels ) : List[Instruction] = {
    scope.putLabels( labels.labels )
    handleElement(labels.defn);
  }
  
  def assertArgumentCount( count: Int, call: Call ) = {
    if ( call.arguments.length != count ) {
      emitError( call.operation + " must have " + count + " arguments. " + call.arguments.length + " arguments were passed." );
    }
  }
}