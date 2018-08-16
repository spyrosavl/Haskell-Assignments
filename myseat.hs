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