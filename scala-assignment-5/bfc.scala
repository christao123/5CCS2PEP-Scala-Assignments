// Part 2 about a "Compiler" for the Brainf*** language
//======================================================

// !!! Copy any function you need from file bf.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}

type Mem = Map[Int, Int]

import io.Source
import scala.annotation.tailrec
import scala.util._

// !! COPY from your bf.scala !!

def load_bff(name: String) : String = {
  try Source.fromFile(name).mkString
  catch {
    case _: Exception => ""
  }
}
  
def sread(mem: Mem, mp: Int) : Int = {
  mem.getOrElse(mp,0)
}

def write(mem: Mem, mp: Int, v: Int) : Mem = {
  mem + (mp -> v)
}

@tailrec
def jumpRight(prog: String, pc: Int, level: Int) : Int = pc match {
  case i if i < prog.length => prog.charAt(pc) match {
    case '[' if level == 0 => jumpRight(prog, pc+1, level+1)
    case '[' if level >= 1 => jumpRight(prog, pc+1, level+1)
    case ']' if level == 0 => pc+1
    case ']' if level >= 1 => jumpRight(prog, pc+1, level-1)
    case _ => jumpRight(prog, pc+1, level)
  }
  case _ => pc
}

@tailrec
def jumpLeft(prog: String, pc: Int, level: Int) : Int = pc match {
  case i if i >= 0 => prog.charAt(pc) match {
    case ']' if level == 0 => jumpLeft(prog, pc-1, level+1)
    case ']' if level >= 1 => jumpLeft(prog, pc-1, level+1)
    case '[' if level == 0 => pc+1
    case '[' if level >= 1 => jumpLeft(prog, pc-1, level-1)
    case _ => jumpLeft(prog, pc - 1, level)
  }
  case _ => pc
}

@tailrec
def compute(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = pc < prog.length && pc >= 0 match {
  case true => prog.charAt(pc) match {
    case '>' => compute(prog, pc+1, mp+1, mem)
    case '<' => compute(prog, pc+1, mp-1, mem)
    case '+' => compute(prog, pc+1, mp, write(mem, mp, sread(mem, mp)+1))
    case '-' => compute(prog, pc+1, mp, write(mem, mp, sread(mem, mp)-1))
    case '.' =>
      print(sread(mem,mp).toChar)
      compute(prog, pc+1, mp, mem)
    case ',' => compute(prog, pc+1, mp, write(mem, mp, Console.in.read().toByte))
    case '[' if sread(mem, mp) == 0 => compute(prog, jumpRight(prog, pc+1, 0), mp, mem)
    case ']' if sread(mem, mp) != 0 => compute(prog, jumpLeft(prog, pc-1, 0), mp, mem)
    case _ => compute(prog, pc+1, mp, mem)
  }
  case false => mem
}

def run(prog: String, m: Mem = Map()) = {
  compute(prog, 0, 0, m)
}

// The baseline to what we can compare our "compiler"
// implemented below. It should require something like 
// 60 seconds for the calculation on my laptop
//
//time_needed(1, run(load_bff("benchmark.bf")))


// DEBUGGING INFORMATION!!!
//
// Compiler, even real ones, are fiendishly difficult to get
// to produce correct code. The point is that for example for
// the Sierpinski program, they need to still generate code
// that displays such a triangle. If yes, then one usually
// can take comfort that all is well. If not, then something
// went wrong during the optimisations.


// ADVANCED TASKS
//================

// (5) Write a function jtable that precomputes the "jump
//     table" for a bf-program. This function takes a bf-program 
//     as an argument and Returns a Map[Int, Int]. The 
//     purpose of this map is to record the information
//     that given on the position pc is a '[' or a ']',
//     then to which pc-position do we need to jump next?
// 
//     For example for the program
//    
//       "+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]"
//
//     we obtain the map
//
//       Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)
//  
//     This states that for the '[' on position 5, we need to
//     jump to position 20, which is just after the corresponding ']'.
//     Similarly, for the ']' on position 19, we need to jump to
//     position 6, which is just after the '[' on position 5, and so
//     on. The idea is to not calculate this information each time
//     we hit a bracket, but just look up this information in the 
//     jtable. You can use the jumpLeft and jumpRight functions
//     from Part 1 for calculating the jtable.
//
//     Then adapt the compute and run functions from Part 1 in order 
//     to take advantage of the information stored in the jtable. 
//     This means whenever jumpLeft and jumpRight was called previously,
//     you should look up the jump address in the jtable.


def jtable(pg: String) : Map[Int, Int] = {
  (0 until pg.length).toList.filter(i => pg.charAt(i) == '[').map(i => i -> jumpRight(pg, i+1,0)).flatMap(e => List(e, (e._2-1)->(e._1+1) )).toMap
}

// testcase
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)

@tailrec
def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if(pc < pg.length && pc >= 0) pg.charAt(pc) match {
    case '>' => compute2(pg, tb, pc+1, mp+1, mem)
    case '<' => compute2(pg, tb, pc+1, mp-1, mem)
    case '+' => compute2(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp)+1))
    case '-' => compute2(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp)-1))
    case '.' =>
      print(sread(mem,mp).toChar)
      compute2(pg, tb, pc+1, mp, mem)
    case ',' => compute2(pg, tb, pc+1, mp, write(mem, mp, Console.in.read().toByte))
    case '[' if sread(mem, mp) == 0 => compute2(pg, tb, tb(pc), mp, mem)
    case ']' if sread(mem, mp) != 0 => compute2(pg, tb, tb(pc), mp, mem)
    case _ => compute2(pg, tb, pc+1, mp, mem)
  }
  else mem
}

def run2(pg: String, m: Mem = Map()) = {
  compute2(pg, jtable(pg), 0, 0, m)
}


//testcase
//time_needed(1, run2(load_bff("benchmark.bf")))



// (6) Write a function optimise which deletes "dead code" (everything
// that is not a bf-command) and also replaces substrings of the form
// [-] by a new command 0. The idea is that the loop [-] just resets the
// memory at the current location to 0. In the compute3 and run3 functions
// below you implement this command by writing the number 0 to mem(mp), 
// that is write(mem, mp, 0). 
//
// The easiest way to modify a string in this way is to use the regular
// expression """[^<>+-.,\[\]]""", which recognises everything that is 
// not a bf-command and replace it by the empty string. Similarly the
// regular expression """\[-\]""" finds all occurrences of [-] and 
// by using the Scala method .replaceAll you can replace it with the 
// string "0" standing for the new bf-command.

def optimise(s: String) : String = {
  s.replaceAll("""[^<>+-.,\[\]]""","").replaceAll("""\[-\]""","0")
}

@tailrec
def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if(pc < pg.length && pc >= 0) pg.charAt(pc) match {
    case '>' => compute3(pg, tb, pc+1, mp+1, mem)
    case '<' => compute3(pg, tb, pc+1, mp-1, mem)
    case '+' => compute3(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp)+1))
    case '-' => compute3(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp)-1))
    case '.' =>
      print(sread(mem,mp).toChar)
      compute3(pg, tb, pc+1, mp, mem)
    case ',' => compute3(pg, tb, pc+1, mp, write(mem, mp, Console.in.read().toByte))
    case '[' if sread(mem, mp) == 0 => compute3(pg, tb, tb(pc), mp, mem)
    case ']' if sread(mem, mp) != 0 => compute3(pg, tb, tb(pc), mp, mem)
    case '0' => compute3(pg, tb, pc+1, mp, write(mem, mp, 0))
    case _ => compute3(pg, tb, pc+1, mp, mem)
  }
  else mem
}

def run3(pg: String, m: Mem = Map()) = {
  compute3(optimise(pg), jtable(optimise(pg)), 0, 0, m)
}

// testcases

//optimise(load_bff("benchmark.bf"))          // should have inserted 0's
//optimise(load_bff("mandelbrot.bf")).length  // => 11203
 
//time_needed(1, run3(load_bff("benchmark.bf")))



// (7)  Write a function combine which replaces sequences
// of repeated increment and decrement commands by appropriate
// two-character commands. For example for sequences of +
//
//              orig bf-cmds  | replacement
//            ------------------------------
//              +             | +A 
//              ++            | +B
//              +++           | +C
//                            |
//              ...           |
//                            | 
//              +++....+++    | +Z
//                (where length = 26)
//
//  Similar for the bf-command -, > and <. All other commands should
//  be unaffected by this change.
//
//  Adapt the compute4 and run4 functions such that they can deal
//  appropriately with such two-character commands.

def is_operator(c: Char) : Boolean = List('+','-','>','<').contains(c)
def alphabetIndexToChar(n: Int) : String = (n + 64).toChar.toString
def charToAlphabetIndex(c: Char) : Int = c.toInt - 64

@tailrec
def combine_auxiliary(output: String, queue: String, char: Char, acc: Int) : String = (queue,is_operator(char)) match {
  case ("", true) => output + char + alphabetIndexToChar(acc)
  case ("", false) => output + char
  case (_, true) => queue.charAt(0) match {
    case head if head == char => acc match {
      case 26 => combine_auxiliary(output + char + alphabetIndexToChar(acc), Try(queue.substring(1)).getOrElse(""), char, 1)
      case _ => combine_auxiliary(output, Try(queue.substring(1)).getOrElse(""), char, acc + 1)
    }
    case head => combine_auxiliary(output + char + alphabetIndexToChar(acc), Try(queue.substring(1)).getOrElse(""), head, 1)
  }
  case _ => combine_auxiliary(output + char, Try(queue.substring(1)).getOrElse(""), queue(0), 1)
}

def combine(s: String) : String = combine_auxiliary("", s, s.charAt(0), 0)


// testcase
//combine(load_bff("benchmark.bf"))

@tailrec
def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if(pc < pg.length && pc >= 0) pg.charAt(pc) match {
    case '>' => compute4(pg, tb, pc + 2, mp + charToAlphabetIndex(pg.charAt(pc + 1)), mem)
    case '<' => compute4(pg, tb, pc + 2, mp - charToAlphabetIndex(pg.charAt(pc + 1)), mem)
    case '+' => compute4(pg, tb, pc + 2, mp, write(mem, mp, sread(mem, mp) + charToAlphabetIndex(pg.charAt(pc + 1))))
    case '-' => compute4(pg, tb, pc + 2, mp, write(mem, mp, sread(mem, mp) - charToAlphabetIndex(pg.charAt(pc + 1))))
    case '.' =>
      print(sread(mem,mp).toChar)
      compute4(pg, tb, pc+1, mp, mem)
    case ',' => compute4(pg, tb, pc+1, mp, write(mem, mp, Console.in.read().toByte))
    case '[' if sread(mem, mp) == 0 => compute4(pg, tb, tb(pc), mp, mem)
    case ']' if sread(mem, mp) != 0 => compute4(pg, tb, tb(pc), mp, mem)
    case '0' => compute4(pg, tb, pc+1, mp, write(mem, mp, 0))
    case _ => compute4(pg, tb, pc+1, mp, mem)
  }
  else mem
}

// should call first optimise and then combine on the input string
//def run4(pg: String, m: Mem = Map()) = ...
def run4(pg: String, m: Mem = Map()) = {
  val program = combine(optimise(pg))
  compute4(program, jtable(program), 0, 0, m)
}


// testcases
//combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

//time_needed(1, run4(load_bff("benchmark.bf")))

//time_needed(1, run(load_bff("sierpinski.bf"))) 
//time_needed(1, run4(load_bff("sierpinski.bf"))) 

//time_needed(1, run4(load_bff("mandelbrot.bf")))


