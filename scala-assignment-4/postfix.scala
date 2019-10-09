// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

import scala.annotation.tailrec

// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList


// (6) Implement below the shunting yard algorithm. The most
// convenient way to this in Scala is to implement a recursive 
// function and to heavily use pattern matching. The function syard 
// takes some input tokens as first argument. The second and third 
// arguments represent the stack and the output of the shunting yard 
// algorithm.
//
// In the marking, you can assume the function is called only with 
// an empty stack and an empty output list. You can also assume the
// input os  only properly formatted (infix) arithmetic expressions
// (all parentheses will be well-nested, the input only contains 
// operators and numbers).

// You can implement any additional helper function you need. I found 
// it helpful to implement two auxiliary functions for the pattern matching:  

def is_op(op: String) : Boolean = ops.contains(op)

def prec(op1: String, op2: String) : Boolean = {
	val p1 = precs.get(op1)
	val p2 = precs.get(op2)
	if(p1.isDefined && p2.isDefined) precs(op1) >= precs(op2) else false
}

@tailrec
def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match {
	case tok::tokens if tok == "(" => syard(tokens, tok::st, out)
	case tok::tokens if tok == ")" => {
		val index = st.indexOf("(")
		syard(tokens, st.drop(index+1), out:::st.take(index))
	}
	case tok::tokens if is_op(tok) => {
			if (st != Nil && prec(st.head,tok)){
				val lower_precedence = st.filter(prec(_,tok))
				val higer_precedence = st diff lower_precedence
				syard(tokens, tok::higer_precedence, out:::lower_precedence)
			} else syard(tokens, tok::st, out)
	}
	case tok::tokens => syard(tokens, st, out :+ tok)
	case _ => out:::st
}

// test cases
//syard(split("3 + 4 * ( 2 - 1 )")) 	== List("3", "4", "2", "1", "-", "*", "+")
//syard(split("10 + 12 * 33"))				== List("10", "12", "33", "*", "+")
//syard(split("( 5 + 7 ) * 2"))				== List("5", "7", "+", "2", "*")
//syard(split("5 + 7 / 2"))						== List("5", "7", "2", "/", "+")
//syard(split("5 * 7 / 2"))						== List("5", "7", "*", "2", "/")
//syard(split("9 + 24 / ( 7 - 3 )"))	== List("9", "24", "7", "3", "-", "/", "+")
//syard(split("3 + 4 + 5"))						== List("3", "4", "+", "5", "+")
//syard(split("( ( 3 + 4 ) + 5 )"))		== List("3", "4", "+", "5", "+")
//syard(split("( 3 + ( 4 + 5 ) )"))		== List("3", "4", "5", "+", "+")
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) ==  List("3", "4", "5", "+", "+")

 
// (7) Implement a compute function that evaluates an input list
// in postfix notation. This function takes a list of tokens
// and a stack as argumenta. The function should produce the 
// result as an integer using the stack. You can assume 
// this function will be only called with proper postfix 
// expressions.    

@tailrec
def recComp(toks: Toks, st: List[Int]) : Int = toks match {
	case tok::_ if is_op(tok) => tok match {
		case "+" => recComp(toks.drop(1), (st.head + st(1)) :: st.drop(2))
		case "-" => recComp(toks.drop(1), (st(1) - st.head) :: st.drop(2))
		case "*" => recComp(toks.drop(1), (st.head * st(1)) :: st.drop(2))
		case "/" => recComp(toks.drop(1), (st(1) / st.head) :: st.drop(2))
	}
	case tok::tokens => recComp(tokens,tok.toInt :: st)
	case Nil => st.head
}

def compute(toks: Toks, st: List[Int] = Nil) : Int = recComp(toks,st)


// test cases
//compute(syard(split("3 + 4 * ( 2 - 1 )")))	== 7
//compute(syard(split("10 + 12 * 33")))				== 406
//compute(syard(split("( 5 + 7 ) * 2"))) 			== 24
//compute(syard(split("5 + 7 / 2")))					== 8
//compute(syard(split("5 * 7 / 2")))					== 17
//compute(syard(split("9 + 24 / ( 7 - 3 )")))	== 15




