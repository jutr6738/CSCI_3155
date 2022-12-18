package edu.colorado.csci3155.project1

/*--
  TODO : Implement the inductive definition for the abstract syntax of our
    programming language here.
    Important: follow the grammar in the notes precisely or else, things will not
    work.
 ---*/

sealed trait CalcProgram
case class TopLevel(listOfCmds: List[Cmd]) extends CalcProgram
sealed trait Cmd
sealed trait Expr
// .. The rest of the definitions below ...
case class Define(s: String, e: Expr) extends Cmd
case class Display(e: Expr) extends Cmd
case class Const(d: Double) extends Expr
case class Ident(s: String) extends Expr
case class Plus( e1: Expr, e2: Expr) extends Expr
case class Minus(e1: Expr, e2: Expr) extends Expr
case class Mult(e1: Expr, e2: Expr) extends Expr
case class Div(e1: Expr, e2: Expr) extends Expr
case class Geq(e1: Expr, e2: Expr) extends Expr
case class Gt(e1: Expr, e2: Expr) extends Expr
case class Eq(e1: Expr, e2: Expr) extends Expr
case class And(e1: Expr, e2: Expr) extends Expr
case class Or(e1: Expr, e2: Expr) extends Expr
case class Not(e: Expr) extends Expr
case class IfThenElse(e1: Expr, e2: Expr, e3: Expr) extends Expr

//Project 1: Julia Troni
// CSCI 3155 Spring 2022