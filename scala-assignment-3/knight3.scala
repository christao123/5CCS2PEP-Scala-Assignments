// Finding a single tour on a "mega" board
//=========================================

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

import scala.annotation.tailrec

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

//(9) Implement a function that searches for a 
//    you have to be careful to write a tail-recursive version as this 
//    function will be called with dimensions of up to 70 * 70
//    and starting field (0, 0). It has to produce a solution within
//    30 seconds.

def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = get_tour_auxiliary(dim, path::Nil, false)