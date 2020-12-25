object Main {
    type Board = Array[Array[Int]]

    val board_side = 9
    val square_side = 3

    def solve(board: Board, cell: Int): Option[Board] = {
        (cell%board_side, cell/board_side) match {
            case (0, 9) => Some(board)
            case (r, c) if board(r)(c) > 0 => solve(board, cell+1)
            case (r, c) =>
                def toCheckCells(i: Int) = {
                    Seq(
                        board(i)(c),
                        board(r)(i),
                        board(i/square_side + r/square_side * square_side)(i%square_side + c/square_side * square_side)
                    )
                }
                def guess(x: Int) = solve(board.updated(r, board(r).updated(c, x)), cell+1)
                val used = board.indices.flatMap(toCheckCells)
                (1 to board_side).diff(used)
                    .flatMap(n => guess(n))  // Try once for each possible digit
                    .headOption  // Get the first non-null option. 
        }
    }

    def main(args: Array[String]) = {
        val board = Array(   // 0 represents the empty cell
            Array(1, 0, 0, 0, 0, 7, 0, 9, 0),
            Array(0, 3, 0, 0, 2, 0, 0, 0, 8),
            Array(0, 0, 9, 6, 0, 0, 5, 0, 0),
            Array(0, 0, 5, 3, 0, 0, 9, 0, 0),
            Array(0, 1, 0, 0, 8, 0, 0, 0, 2),
            Array(6, 0, 0, 0, 0, 4, 0, 0, 0),
            Array(3, 0, 0, 0, 0, 0, 0, 1, 0),
            Array(0, 4, 0, 0, 0, 0, 0, 0, 7),
            Array(0, 0, 7, 0, 0, 0, 3, 0, 0)
        )

        // Print the formated matrix
        println(solve(board, 0).get.map(_.mkString(" ")).mkString("\n"))
    }
}