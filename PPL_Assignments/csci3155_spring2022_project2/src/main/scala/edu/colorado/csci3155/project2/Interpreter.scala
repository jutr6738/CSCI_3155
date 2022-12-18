package edu.colorado.csci3155.project2

object Interpreter {

    def binaryExprEval(expr: Expr, expr1: Expr, env: Environment)(fun: (Value, Value) => Value): Value = {
        val v1 = evalExpr(expr, env)
        val v2 = evalExpr(expr1, env)
        fun(v1, v2)
    }

    def evalExpr(e: Expr, env: Environment): Value = e match {
        case Const(d) => NumValue(d)
        case ConstBool(b) => BoolValue(b)
        case Ident(s) => env.lookup(s)
        case Line(l) => evalExpr(l, env) match {
            case NumValue(len) => { // TODO: Create a polygon with two vertices (0,0) and (len, 0)
                // Make sure to return a FigValue(canvas).
                val poly= Polygon( List((0,0),(len,0)))
                val canv= new MyCanvas(List(poly) )
                FigValue(canv)
            }
            case _ => throw new IllegalArgumentException("Cannot create line with non numerical length")
        }
        case EquiTriangle(sideLength) => evalExpr(sideLength,env) match { // TODO: Handle an equilateral triangle.
            case NumValue(v) => {
                val tri= Polygon( List( (0,0),(v,0),(v/2, math.sqrt(3)/2*v) ) )
                val canv= new MyCanvas(List(tri) )
                FigValue(canv)
            }
            case _ => throw new IllegalArgumentException("Cannot create a triangle with non numerical length")
        }
        case Rectangle(sideLength) => evalExpr(sideLength,env) match { // TODO: Handle square given the side length
            case NumValue(v) => {
                val rect= Polygon( List( (0,0),(0,v),(v,v), (v,0) ) )
                val canv= new MyCanvas(List(rect) )
                FigValue(canv)
            }
            case _ => throw new IllegalArgumentException("Cannot create a rectangle with non numerical length")
        }
        case Circle(rad) => evalExpr(rad,env) match { // TODO: Handle circle
            case NumValue(v) => {
                val cir= MyCircle( (0,0), v )
                val canv= new MyCanvas(List(cir) )
                FigValue(canv)
            }
            case _ => throw new IllegalArgumentException("Cannot create a circle with non numerical length")
        }
        case Plus (e1, e2) => { // TODO: Handle addition of numbers or figures
            val v1= evalExpr(e1,env)
            val v2= evalExpr(e2,env)
            (v1,v2) match {
                case (NumValue(n1), NumValue(n2))  => { NumValue(n1+n2) }
                case (FigValue(f1), FigValue(f2)) => {
                    FigValue(f1.overlap(f2))
                }
                case _ => throw new IllegalArgumentException("Cannot add a number and a figure")
            }
        }
        case Minus (e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.minus)
        case Mult(e1, e2) => {// TODO: Handle multiplication of numbers or figures
            val v1= evalExpr(e1,env)
            val v2= evalExpr(e2,env)
            (v1,v2) match {
                case (NumValue(n1), NumValue(n2))  => { binaryExprEval(e1, e2, env) (ValueOps.mult)   }
                case (FigValue(f1), FigValue(f2)) => {
                    FigValue(f1.placeRight(f2))
                }
                case _ => throw new IllegalArgumentException("Cannot multiply a number and a figure")
            }
        }
        case Div(e1, e2) => {// TODO: Handle division
            val v1= evalExpr(e1,env)
            val v2= evalExpr(e2,env)
            (v1,v2) match {
                case (NumValue(n1), NumValue(n2))  => { binaryExprEval(e1, e2, env) (ValueOps.mult)   }
                case (FigValue(f1), NumValue(n2)) => {
                    FigValue(f1.rotate(n2))
                }
                case (FigValue(f1), FigValue(f2)) => {
                    FigValue(f2.placeTop(f1))
                }
                case _ => throw new IllegalArgumentException("Cannot divide a number by a figure")
            }
        }
        case Geq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.geq)
        case Gt(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.gt)
        case Eq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.equal)
        case Neq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.notEqual)
        case And(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("And applied to a non-Boolean value")
                    }
                }
                case BoolValue(false) => BoolValue(false)
                case _ => throw new IllegalArgumentException("And applied to a non-boolean value")
            }
        }
        case Or(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => BoolValue(true)
                case BoolValue(false) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean value")
                    }
                }
                case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean Value")
            }
        }
        case Not(e) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(b) => BoolValue(!b)
                case _ => throw new IllegalArgumentException("Not applied to a non-Boolean Value")
            }
        }
        case IfThenElse(e, e1, e2) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(true) => evalExpr(e1, env)
                case BoolValue(false) => evalExpr(e2,env)
                case _ => throw new IllegalArgumentException("If then else condition is not a Boolean value")
            }
        }
        case Let(x, e1, e2) => {
            val v1 = evalExpr(e1, env)
            val env2 = Extend(x, v1, env)
            evalExpr(e2, env2)
        }
        case FunDef(x, e) => {//TODO: Handle function definitions
            Closure(x,e,env)
        }
        case LetRec(f, x, e1, e2) => { // TODO: Handle recursive functions -- look at Environment.scala
            val newenv= ExtendREC(f,x,e1,env)
            evalExpr(e2, newenv)
        }
        case FunCall(fCallExpr, arg) => { // TODO: Handle function calls
            val v1 = evalExpr(fCallExpr, env)
            val v2 = evalExpr(arg, env)
            v1 match {
                case Closure(x, closExp, closenv) => {
                    // First extend closenv by binding x to v2
                    val newenv = Extend(x, v2, closenv)
                    // Evaluate the body of the closure under the newenv
                    evalExpr(closExp, newenv)
                }
                case _ => throw new IllegalArgumentException(s"Function call error: expression $fCallExpr does not evaluate to a closure")
            }

        }
    }

    def evalProgram(p: Program): Value = p match {
        case TopLevel(e) => evalExpr(e, EmptyEnvironment)
    }

}
