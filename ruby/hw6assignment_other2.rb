# Programming Languages C - Assignment 1
# Alex. C
# 14/07/2018 - Initial submission
# 15/07/2018 - Formatting


class MyPiece < Piece

  All_My_Pieces = [rotations([[0,0],[1,0],[0,1],[1,1],[0,2]]),
				   [[[0,0],[1,0],[-1,0],[2,0],[-2,0]],
				   [[0,0],[0,-1],[0,1],[0,2],[0,-2]]],
				   rotations([[0,0],[0,1],[1,0]])] + All_Pieces
				   
  Cheat_Piece = [[[0,0]]]				   
 
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
  
  def self.cheat_piece (board)
	MyPiece.new(Cheat_Piece, board)
  end
end

class MyBoard < Board

  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
	@cheat = false
  end


  def next_piece  
	if @cheat then 
	  @current_block = MyPiece.cheat_piece(self)
	  @cheat = false
    else
	    @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end
  
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
	no_points = locations.length - 1
    (0..no_points).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
  
  def cheat
    if !@cheat && @score >= 100 then
	  @score -= 100
	  @cheat = true
	end     
  end  

end

class MyTetris < Tetris

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  
  def key_bindings
	super() # Call superclass key_bindings to setup originals
	@root.bind('u', proc {2.times {@board.rotate_clockwise}}) # And add our own for 'u'
	@root.bind('c', proc {@board.cheat}) # And 'c' another for cheat
  end	
end