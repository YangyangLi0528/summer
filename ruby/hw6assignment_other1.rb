# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  NEW_Pieces = [
      [[[0, 0], [-1, 0], [1, 0], [2, 0], [-2, 0]], # long (only needs two)
       [[0, 0], [0, -1], [0, 1], [0, 2], [0, -2]]],
      rotations([[0, 0], [1, 0], [0, 1], [1, 1], [-1, 0]]), # Fist
      rotations([[0, 0], [1, 0], [0, 1]]) #small L
  ]
  CHEAT_Piece = [[[0, 0]]] #single piece


  All_My_Pieces = All_Pieces.concat(NEW_Pieces)

  # your enhancements here
  def initialize (point_array, board)
    super
  end

  # class method to choose the next piece
  def self.next_piece (board)
    if board.cheat_enable
      MyPiece.new(CHEAT_Piece, board)
    else
      MyPiece.new(All_My_Pieces.sample, board)
    end
  end

end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    super
    @current_block = MyPiece.next_piece(self)
    @cheat_enable = false
  end

  def cheat_enable
    @cheat_enable
  end

  def rotate_180
    rotate_clockwise
    rotate_clockwise
  end

  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
    @cheat_enable = false
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    locations.each_index{
      |index|current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
          @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def enable_cheat
    if  !(@cheat_enable || @score < 100 )
      @score -= 100
      @cheat_enable = true
    end
  end

end

class MyTetris < Tetris
  # your enhancements here
  def initialize
    super
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.enable_cheat})
  end

end