import Data.List
import Data.List.Split

myinternet x [] = x
myinternet x (h:t) = if h <= x then x-h + myinternet x t else myinternet x t - (h-x)

------------------------------------------------------------------------------------------

row s n = first_n_minus_1 ++ [(last first_n_minus_1)+(n-1)*(n-2)/2]
          where first_n_minus_1 = [s..s+n-2]

matrix s n line = if line < n-1 then [current_row] ++ (matrix next_start n (line+1))  else [current_row]
                  where current_row = row s n
                        offset = (n-1)*(n-2)/2
                        next_start = if line<n-2 then last current_row+1 else (n-1+offset)^2-offset-(n-2)

mymatrix n = matrix 1 n 0
-------------------------------------------------------------------------------------------


shot_balloons height [] = []
shot_balloons height (h:t) = if h==height then shot_balloons (height-1) t else (h:(shot_balloons height t))

pinkballoons [] = 0
pinkballoons (h:t) = if null(new_list) then 1 else pinkballoons new_list + 1
					where new_list = shot_balloons h (h:t)


--------------------------------------------------------------------------------------------

is_same l1 l2 
        | (null l1) && (not (null l2) ) = False
        | (null l2) && (not (null l1) ) = False
        | (null l2) && (null l1) = True
        | otherwise = ( sort(l1) == sort(l2) )

check_for_n_root n l = if (length l) `mod` n == 0 then length(filter (is_same (head chunks)) chunks) == length chunks else False
                       where chunks = chunksOf n l

check_string_root n l = if check_for_n_root n l then take n l else check_string_root (n+1) l

stringroot l = check_string_root 1 l

----------------------------------------------------------------------------------------------


seat_rank [x1,x2,x3] | x2=='o' = -1
                     | x1=='o' && x3=='o' = 0
                     | x1=='o' || x3=='o' = 1
                     | otherwise = 2


last_max_seat l index | length l < 3 = (-1,-1)
                      | otherwise = if rank > (fst next_tuple) then (rank, index) else next_tuple
                        where rank = seat_rank (take 3 l)
                              next_tuple = last_max_seat (drop 1 l) (index+1)

first_max_seat l index | length l < 3 = (-1,-1)
                       | otherwise = if rank >= (fst next_tuple) then (rank, index) else next_tuple
                        where rank = seat_rank (take 3 l)
                              next_tuple = first_max_seat (drop 1 l) (index+1)


clear_seats_with_rank_2 l | length l < 3 = l
                          | (first_3!!0)=='e' && (first_3!!1)=='e' && (first_3!!2)=='e' = [first_3!!0] ++ clear_seats_with_rank_2 ( ['o'] ++ (drop 2 l) )
                          | otherwise = [first_3!!0] ++ clear_seats_with_rank_2 (drop 1 l)
                          where first_3 = take 3 l

myseat l | (last l)=='e' = length l-1
         | (head l)=='e' = 0
         | (length l)`mod`2==0 = snd (last_max_seat reversed_cleared_reversed 1)
         | (fst first_max) > 0 = snd (first_max)
         | otherwise = snd (last_max)
         where first_max = first_max_seat l 1
               last_max = last_max_seat l 1
               reversed = reverse l
               cleared_reversed = clear_seats_with_rank_2 reversed
               reversed_cleared_reversed = reverse cleared_reversed
