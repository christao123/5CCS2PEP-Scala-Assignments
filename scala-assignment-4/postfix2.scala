import scala.annotation.tailrec
// Shunting Yard Algorithm
// including Associativity for Operators 
// =====================================

// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// let- and right-associativity
abstract class Assoc
case object RA extends Assoc
case object LA extends Assoc

// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

// (8) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.

def is_op(op: String) : Boolean = ops.contains(op)

def precL(op1: String, op2: String) : Boolean = {
  val p1 = precs.get(op1)
  val p2 = precs.get(op2)
  if(p1.isDefined && p2.isDefined) precs(op1) >= precs(op2) else false
}

def precR(op1: String, op2: String) : Boolean = {
  val p1 = precs.get(op1)
  val p2 = precs.get(op2)
  if(p1.isDefined && p2.isDefined) precs(op1) > precs(op2) else false
}

@tailrec
def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match {
  case tok::tokens if tok == "(" => syard(tokens, tok :: st, out)
  case tok::tokens if tok == ")" => {
    val index = st.indexOf("(")
    syard(tokens, st.drop(index + 1), out:::st.take(index))
  }
  case tok::tokens if is_op(tok) => {
    if (assoc(tok) == RA) {
      if (st != Nil && precR(st.head, tok)) {
        val lower_precedence = st.filter(precR(_,tok))
        syard(tokens, tok::(st diff lower_precedence), out:::lower_precedence)
      }
      else syard(tokens, tok::st, out)
    }
    else {
      if (st != Nil && precL(st.head, tok)) {
        val lower_precedence = st.filter(precL(_,tok))
        syard(tokens, tok::(st diff lower_precedence), out:::lower_precedence)
      }
      else syard(tokens, tok::st, out)
    }
  }
  case tok::tokens => syard(tokens, st, out:+tok)
  case _ => out:::st
}


// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (9) Implement a compute function that produces a Long(!) for an
// input list of tokens in postfix notation.

@tailrec
def compute(toks: Toks, st: List[Long] = Nil) : Long = toks match {
  case tok::tokens if is_op(tok) => tok match {
    case "+" => compute(tokens, (st.head + st(1))::st.drop(2))
    case "-" => compute(tokens, (st(1) - st.head)::st.drop(2))
    case "*" => compute(tokens, (st.head * st(1))::st.drop(2))
    case "/" => compute(tokens, (st(1) / st.head)::st.drop(2))
    case "^" => compute(tokens, math.pow(st(1),st.head).toLong :: st.drop(2))
  }
  case tok::tokens => compute(tokens,tok.toLong::st)
  case _ => st.head
}


// test cases
//compute(syard(split("3 + 4 * ( 2 - 1 )")))  == 7
//compute(syard(split("10 + 12 * 33")))       == 406
//compute(syard(split("( 5 + 7 ) * 2")))      == 24
//compute(syard(split("5 + 7 / 2")))          == 8
//compute(syard(split("5 * 7 / 2")))          == 17
//compute(syard(split("9 + 24 / ( 7 - 3 )"))) == 15
//compute(syard(split("4 ^ 3 ^ 2")))          == 262144
//compute(syard(split("4 ^ ( 3 ^ 2 )")))      == 262144
//compute(syard(split("( 4 ^ 3 ) ^ 2")))      == 4096
//compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))  == 65536

