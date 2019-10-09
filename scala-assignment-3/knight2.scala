// Part 2 about finding a single tour for a board using the Warnsdorf Rule
//=========================================================================

// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

import scala.annotation.tailrec

//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = (0 until dim contains x._1) && (0 until dim contains x._2) && !(path contains x)

def get_moves(x: Pos) : List[Pos] = List((1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2)).map(o => (o._1+x._1,o._2+x._2))

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = get_moves(x).filter(p => is_legal(dim, path, p))

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = legal_moves(dim,path,x).sortBy(y => legal_moves(dim,path,y).size)

@tailrec
def get_tour_auxiliary(dim: Int, basepaths: List[Path], closed: Boolean) : Option[Path] = {
  if(basepaths.nonEmpty){
    val path = basepaths.head
    if (path.length == dim*dim && (!closed || get_moves(path.head).contains(path.last))) Some(path)
    else get_tour_auxiliary(dim, ordered_moves(dim, path, path.head).map(move => move::path):::basepaths.drop(1), closed)
  } else None
}

//(7) Complete the function that searches for a single *closed*
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board.
def first_closed_tour_heuristic(dim: Int, path: Path) : Option[Path] = get_tour_auxiliary(dim, path::Nil, true)

//(8) Same as (7) but searches for *non-closed* tours. This
//    version of the function will be called with dimensions of
//    up to 30 * 30.
def first_tour_heuristic(dim: Int, path: Path) : Option[Path] = get_tour_auxiliary(dim, path::Nil, false)

//first_closed_tour_heuristic(6, List((3,3)))
//first_tour_heuristic(8, List((0,0)))
//first_tour_heuristic(30, List((0,0)))
//first_tour_heuristic(70, List((0,0)))

