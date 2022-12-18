package edu.colorado.csci3155.project1


class RuntimeError(msg: String) extends Exception {
    override def toString(): String = {
        s"Error: $msg"
    }
}


object Interpreter {


    type Environment = Map[String, Value]


    /*--
        TODO: Complete the evalExpr Function below.
        Please write refactored code and use ValueOps.plus, ValueOps.minus,..
        defined in Value.scala for evaluating the expressions.

        If you encounter error, you should throw a RuntimeError exception defined above.
        Please do not use other exception types.
     */
    def evalExpr(e: Expr, env: Environment): Value = e match {
        case Const(d) => NumValue(d)
        case Ident(s) => {
            if (env.contains(s)) {
                env(s)
            } else {
                throw new RuntimeError(s"Environment does not contain mapping for $s")
            }
        }
        case Plus(a, b) => {
            val v1 = evalExpr(a, env)
            val v2 = evalExpr(b, env)
            ValueOps.plus(v1, v2)
        }
        case Minus(e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            ValueOps.minus(v1, v2)
        }
        case Mult(e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            ValueOps.mult(v1, v2)
        }
        //if denominator is 0, throw error
        case Div(e1, e2) => {
            val v2 = evalExpr(e2, env)
            v2 match {
              case (NumValue(0.0)) => throw new RuntimeError("Divide by zero error in divisor ${e2}")
              case (denominator) =>
                  val numerator = evalExpr(e1, env)
                  ValueOps.div(numerator, denominator)
          }
        }
        case Geq(e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            ValueOps.geq(v1, v2)
        }
        case Gt(e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            ValueOps.gt(v1, v2)
        }
        case Eq(e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            ValueOps.eq(v1, v2)
        }
        //short circuiting
        case And(e1,e2) => {
            val v1= evalExpr(e1,env)
            v1 match{
                case BoolValue(false) => BoolValue(false)
                case BoolValue(true) => {
                    val v2= evalExpr(e2,env)
                    v2 match{
                        case BoolValue(_) => v2
                        case _ => throw new RuntimeError("And of a boolean and non-boolean expr: ${e2} which evaluated to ${v2}")
                    }
                }
                case _ => throw new RuntimeError("And of a non-boolean expr: ${e1} which evaluated to ${v1}")
            }
        }
        //short circuiting
        case Or(e1,e2) => {
            val v1= evalExpr(e1,env)
            v1 match{
                case BoolValue(true) => BoolValue(true)
                case BoolValue(false) => {
                    val v2= evalExpr(e2,env)
                    v2 match{
                        case BoolValue(_) => v2
                        case _ => throw new RuntimeError("Or of a boolean and non-boolean expr: ${e2} which evaluated to ${v2}")
                    }
                }
                case _ => throw new RuntimeError("Or of a non-boolean expr: ${e1} which evaluated to ${v1}")
            }
        }
        case Not(e1) => {
            val v1= evalExpr(e1, env)
            v1 match {
                case BoolValue(b) => BoolValue(!b)
                case _ => throw new RuntimeError("Not of a non-boolean expr: ${e1} which evaluated to ${v1}")
            }
        }
        case IfThenElse(condExp, thenBranch, elseBranch) => {
            val valCond = evalExpr(condExp, env)
            valCond match {
                case BoolValue(true) => {
                    evalExpr(thenBranch, env)
                }
                case BoolValue(false) => {
                    evalExpr(elseBranch, env)
                }
                case _ => throw new RuntimeError("If-then-else condition expr: ${condExp} is non-boolean -- evaluates to ${valCond}")
            }
        }

    }


    /*--
    TODO: Implement a function evalVarDefine that given a identifier x,
    expression e and environment env,
       a) evaluates e under env: let result be v
       b) yields new environment that updates env with {x -> v }
     For your convenience the RuntimeError exception has been handled for you.
     */
    def evalVarDefine(x: String, e: Expr, env: Environment): Environment = {
        try {
            val v= evalExpr(e, env)
            env+(x->v)
        } catch {
            case _:RuntimeError =>  env
        }
    }

    /*-- TODO: Complete the evalCommand Function Below --*/
    // Function evalCommand
    // Evaluate a command under an environment.
    //  Returns the new environment as a result of executing the command.
    //  If the command is of the form Define(x, e), the environment is updated by evaluating
    //  e under the "old" environment and updating the old environment to now bind x to the result.
    // If the command is of the form Display(e), the environment returned is just the
    // same as the environment that was passed in as an argument.
    //
    def evalCommand( env: Environment, cmd: Cmd): Environment = cmd match {
        case Define(str, exp) => {
            try {
                val v= evalExpr(exp, env)
                env+(str->v)
            } catch {
                case _:RuntimeError =>  env
            }
        }
        case Display(exp) => {
            try {
                val v= evalExpr(exp, env)
                env
            } catch {
                case _:RuntimeError =>  env
            }
        }
    }

    /*-- TODO: Implement evalProgram function below.
       Careful: Do not use for/while loops. Instead you should be using
       pattern matching on `prog` and then using lst foldLeft function.
       A tail recursive solution is also acceptable but please try to use pattern matching.
     */
    def evalProgram(prog: CalcProgram, env0: Environment = Map.empty): Environment = prog match {
        case TopLevel(listOfCmds) => listOfCmds.foldLeft(env0) ( (env0, elt) => (evalCommand(env0, elt)) )
        case _ => throw new RuntimeError("No program to evaluate")
    }
}

//Project 1: Julia Troni
// CSCI 3155 Spring 2022