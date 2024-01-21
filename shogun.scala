// Main Part 4 about the Shogun Board Game
//=========================================

// Task 1 - 5 see below

object M4 {   

type Pos = (Int, Int)    // a position on a chessboard 

// Colours: Red or White
abstract class Colour
case object Red extends Colour
case object Wht extends Colour

// Pieces: Either Pawns or Kings
//===============================
abstract class Piece {
  def pos : Pos       
  def col : Colour    
  def en : Int      // energy for Pawns 1 - 4, for Kings 1 - 2
}
case class Pawn(en: Int, col: Colour, pos: Pos) extends Piece
case class King(en: Int, col: Colour, pos: Pos) extends Piece

// how to extract components from pieces
//val p = Pawn(4, Wht, (3,2))
//assert(p.pos == (3,2))
//assert(p.col == Wht)
//assert(p.en == 4)  

// checks if a piece is a king
def is_king(pc: Piece) : Boolean = pc match {
  case King(_, _, _) => true
  case _ => false
}

// incrementing and decrementing the position of a piece
def incx(pc: Piece) : Piece = pc match {
  case Pawn(en, c, (x,y)) => Pawn(en, c, (x+1,y))
  case King(en, c, (x,y)) => King(en, c, (x+1,y))
}

def incy(pc: Piece) : Piece = pc match {
  case Pawn(en, c, (x,y)) => Pawn(en, c, (x,y+1))
  case King(en, c, (x,y)) => King(en, c, (x,y+1))
}

def decx(pc: Piece) : Piece = pc match {
  case Pawn(en, c, (x,y)) => Pawn(en, c, (x-1,y))
  case King(en, c, (x,y)) => King(en, c, (x-1,y))
}

def decy(pc: Piece) : Piece = pc match {
  case Pawn(en, c, (x,y)) => Pawn(en, c, (x,y-1))
  case King(en, c, (x,y)) => King(en, c, (x,y-1))
}

//pretty printing colours and pieces
def pp_color(c: Colour) : String = c match {
  case Red => "R"
  case Wht => "W"
}

def pp(pc: Piece) : String = pc match {
  case Pawn(n, c, _) => s"P${pp_color(c)}$n"
  case King(n, c, _) => s"K${pp_color(c)}$n"
}

// Boards are sets of pieces
//===========================
case class Board(pces: Set[Piece]) {
  def +(pc: Piece) : Board = Board(pces + pc)
  def -(pc: Piece) : Board = Board(pces - pc)
}

// checking whether a position is occupied in a board
def occupied(p: Pos, b: Board) : Option[Piece] =  
  b.pces.find(p == _.pos)
  
def occupied_by(p: Pos, b: Board) : Option[Colour] =
  occupied(p, b).map(_.col)

def is_occupied(p: Pos, b: Board) : Boolean =
  occupied(p, b).isDefined

// is a position inside a board
def inside(p: Pos, b: Board): Boolean = 
  1 <= p._1 && 1 <= p._2 && p._1 <= 8 && p._2 <= 8 

// pretty printing a board
def print_board(b: Board): Unit = {
  println()
  for (i <- 8 to 1 by -1) {
    println("----" * 8)
    for (j <- 1 to 8) {
      val opc = occupied((j,i), b)
      if (opc.isDefined) print(s"|${pp(opc.get)}") 
      else print("|   ")
    }
    println("|")
  } 
  println("----" * 8)
}

// example board: initial board
val b_init = Board(Set(King(2,Wht,(4,1)), King(1,Red,(5,8)),
                  		 Pawn(4,Wht,(1,1)), Pawn(4,Red,(1,8)),
                  		 Pawn(3,Wht,(2,1)), Pawn(2,Red,(2,8)),
                  		 Pawn(2,Wht,(3,1)), Pawn(3,Red,(3,8)),
                  		 Pawn(1,Wht,(5,1)), Pawn(1,Red,(4,8)),
                  		 Pawn(4,Wht,(6,1)), Pawn(3,Red,(6,8)),
                  		 Pawn(3,Wht,(7,1)), Pawn(1,Red,(7,8)),
                  		 Pawn(2,Wht,(8,1)), Pawn(3,Red,(8,8))))

//print_board(b_init)
// --------------------------------
// |PR4|PR2|PR3|PR1|KR1|PR3|PR1|PR3|
// --------------------------------
// |   |   |   |   |   |   |   |   |
// --------------------------------
// |   |   |   |   |   |   |   |   |
// --------------------------------
// |   |   |   |   |   |   |   |   |
// --------------------------------
// |   |   |   |   |   |   |   |   |
// --------------------------------
// |   |   |   |   |   |   |   |   |
// --------------------------------
// |   |   |   |   |   |   |   |   |
// --------------------------------
// |PW4|PW3|PW2|KW2|PW1|PW4|PW3|PW2|
// --------------------------------




// Moves
//=======
abstract class Move
case object U extends Move    // up
case object D extends Move    // down
case object R extends Move    // right
case object L extends Move    // left
case object RU extends Move   // first right, then possibly up
case object LU extends Move   // first left, then possibly up
case object RD extends Move   // right then down
case object LD extends Move   // left then down
case object UR extends Move   // up then down
case object UL extends Move   // up then left
case object DR extends Move   // down then right
case object DL extends Move   //down then left

//======================
// ADD YOUR CODE BELOW
//======================


// Task 1: 
def eval(pc: Piece, m: Move, en: Int, b: Board) : Set[Piece] = m match {
  case _ if !inside(pc.pos, b) => Set()
  case _ if en == 0 && !is_occupied(pc.pos, b) => Set(pc)
  case _ if en == 0 && is_occupied(pc.pos, b) && occupied_by(pc.pos, b) != Some(pc.col) => Set(pc)
  case _ if en == 0 && is_occupied(pc.pos, b) && occupied_by(pc.pos, b) == Some(pc.col) => Set()
  case _ if en > 0 && is_occupied(pc.pos, b) => Set()
  case U => eval(incy(pc), U, en - 1, b)
  case D => eval(decy(pc), D, en - 1, b)
  case R => eval(incx(pc), R, en - 1, b)
  case L => eval(decx(pc), L, en - 1, b)
  case RU => {
    eval(incx(pc), RU, en - 1, b) ++ eval(pc, U, en, b)
  }
   case LU => {
    eval(decx(pc), LU, en - 1, b) ++ eval(pc, U, en, b)
  }
  case RD => {
    eval(incx(pc), RD, en - 1, b) ++ eval(pc, D, en, b)
  }
   case LD => {
    eval(decx(pc), LD, en - 1, b) ++ eval(pc, D, en, b)
  }
  case UR => {
    eval(incy(pc), UR, en - 1, b) ++ eval(pc, R, en, b)
  }
   case UL => {
    eval(incy(pc), UL, en - 1, b) ++ eval(pc, L, en, b)
  }
  case DR => {
    eval(decy(pc), DR, en - 1, b) ++ eval(pc, R, en, b)
  }
   case DL => {
    eval(decy(pc), DL, en - 1, b) ++ eval(pc, L, en, b)
  }
}


// Task 2: Take off the piece so it doesnt check its own position or check in eval its occupied 
def all_moves(pc: Piece, b: Board) : Set[Piece] = {
  val new_b = b - pc
  eval(pc, U, pc.en, new_b) ++ eval(pc, D, pc.en, new_b) ++ eval(pc, R, pc.en, new_b) ++ 
  eval(pc, L, pc.en, new_b) ++ eval(pc, RU, pc.en, new_b) ++ eval(pc, LU, pc.en, new_b) ++ 
  eval(pc, RD, pc.en, new_b) ++ eval(pc, LD, pc.en, new_b) ++ eval(pc, UR, pc.en, new_b) ++ 
  eval(pc, UL, pc.en, new_b) ++ eval(pc, DR, pc.en, new_b) ++ eval(pc, DL, pc.en, new_b)
}


// Task 3: All the pieces of the opposite colour that are attacked
def attacked(c: Colour, b: Board) : Set[Piece] = {
  val (player, enemy) = b.pces.partition(_.col == c)
  val allPossibleMoves = (player.map(pc => all_moves(pc,b))).flatten
  for (pc <- allPossibleMoves;
    n <- enemy;
    if pc.pos == n.pos) yield n
}


// Task 4: Number of times this specific piece is attacked by pieces of opposite colour
//attacked returns a set - it can only count 1 of them - so we must count them all
def attackedN(pc: Piece, b: Board) : Int = {
  val enemy = b.pces.filter(_.col != pc.col)
  val allPossibleMoves = (enemy.map(pc => all_moves(pc,b)).toList).flatten
  allPossibleMoves.count(_.pos == pc.pos)
}


// Task 5: Protected by Pieces of the same colour:
def protectedN(pc: Piece, b: Board) : Int = {
  val player = b.pces.filter(_.col == pc.col)
  val allPossibleMoves = (player.map(p => all_moves(p,b - pc)).toList).flatten
  allPossibleMoves.count(_.pos == pc.pos)
}


// Task 6: Check if Attacked and Protected
//If they are not under attack AND not protected then they are allowed
def legal_moves(pc: Piece, b: Board): Set[Piece] = pc match {
  case Pawn(en, col, pos) => all_moves(pc, b)
  case King(en, col, pos) =>
    for {
      n <- all_moves(pc, b)
      not_attack = attackedN(n, b) == 0
      not_protect = if (is_occupied(n.pos, b)) protectedN(occupied(n.pos, b).get, b) == 0 else true
      if not_attack && not_protect
    } yield n
}

// val b3 = Board(Set(King(2,Wht,(7,1)), King(2,Red,(4,2)),
//                   		 Pawn(2,Wht,(4,1)), Pawn(3,Red,(6,1)),
//                   		 Pawn(1,Wht,(8,4)), Pawn(4,Red,(5,3)),
//                   		 Pawn(3,Wht,(8,7)), Pawn(4,Red,(4,4)),
//                   		 Pawn(2,Wht,(6,7)), Pawn(3,Red,(6,5))))

// val b4 = Board(Set(King(1,Wht,(7,1)), King(2,Red,(4,2)),
//                   		 Pawn(2,Wht,(4,1)), Pawn(3,Red,(6,1)),
//                   		 Pawn(1,Wht,(8,4)), Pawn(3,Red,(5,3)),
//                   		 Pawn(3,Wht,(8,7)), Pawn(4,Red,(4,4)),
//                   		 Pawn(2,Wht,(6,7)), Pawn(3,Red,(6,5))))

// legal_moves(King(2,Wht,(7,1)), b3)
// legal_moves(King(1,Wht,(7,1)), b4)
// val n = King(1,Wht,(7,2))
// val n = King(1,Wht,(8,1))
// val n = King(1,Wht,(6,1))


/*
// more test cases
//=================
val pw1 = Pawn(4, Wht, (4,6))
val pw2 = Pawn(4, Wht, (2,4))
val pw3 = Pawn(3, Red, (6,8))
val pw4 = Pawn(2, Red, (2,8))
val bt = b_init + pw1 + pw2

print_board(bt)
println(s"Capture Red: ${attacked(Wht, bt)}")
  // => Set(Pawn(2,Red,(2,8)), Pawn(3,Red,(6,8)))

println(s"Capture Wht: ${attacked(Red, bt)}")
  // => Set(Pawn(4,Wht,(4,6)))

println(s"ProtectedN:  ${protectedN(pw3, bt)}")
  // => 2

println(s"AttackedN:   ${attackedN(pw4, bt)}")
  // => 2

println(s"all moves:   ${all_moves(pw2, bt)}")
  // => Set(Pawn(4,Wht,(4,2)), Pawn(4,Wht,(1,7)), Pawn(4,Wht,(5,3)), Pawn(4,Wht,(5,5)), 
  //        Pawn(4,Wht,(2,8)), Pawn(4,Wht,(3,7)), Pawn(4,Wht,(6,4)))
*/


}


