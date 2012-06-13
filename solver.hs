import Data.List

square_dim = 3
board_dim = 3
row_size = 9
col_size = 9
square_size = 9

test_puzzle = [0,0,4,0,0,0,2,0,0,0,1,0,7,2,6,0,3,0,0,6,0,0,0,0,7,0,0,0,9,6,4,0,0,0,8,0,0,0,0,0,0,0,0,4,0,0,2,0,9,3,5,6,0,7,2,0,7,8,0,0,0,6,0,6,0,0,2,9,0,0,0,3,0,0,5,0,1,0,8,0,4]

get_my_square ind board = map (board !!) (indices_in_square square_x square_y)
	 where
	 row = ind `quot` row_size
	 col = ind `mod`  col_size
	 square_x = col `quot` board_dim
	 square_y = row `quot` board_dim

indices_in_square x y = foldr (++) [] (map (\i -> [i..i+square_dim-1]) [start,start+row_size..(start+row_size*(square_dim-1))])
	where
	start = y*square_dim*row_size + square_dim*x				
	       	    
potential_numbers l = [x | x <- [1..square_size], not (x `elem` l)]

get_my_row ind board = take row_size (drop (row_size*row_coordinate) board)
	 where
	 row_size = square_dim*square_dim
	 row_coordinate = head (board_coordinate ind)

get_my_column ind board = map (board !!) [col,col+row_size..end]
	 where
	 col = ind `mod` row_size
	 end = row_size*(col_size-1) + col

board_coordinate ind = [row, col]
	where
	row = ind `quot` row_size
	col = ind `mod` row_size

potential_solutions ind board = (potential_numbers (get_my_row ind board)) `intersect` (potential_numbers (get_my_column ind board)) `intersect` (potential_numbers (get_my_square ind board))

try_to_solve board ind = if (board !! ind) `elem` [1..square_size] --check if already solved
	     	       	 then (board !! ind)
			 else
			     if length (potential_solutions ind board) == 1 --check if there's only one possible soln
			     then head (potential_solutions ind board)
			     else 0

is_solved puzzle = foldr (&&) True (map (\x -> (if x `elem` [1..9] then True else False)) puzzle)

solve puzzle = if (is_solved puzzle)
      	       then puzzle
	       else solve (map (try_to_solve puzzle) [0..(length puzzle)-1])