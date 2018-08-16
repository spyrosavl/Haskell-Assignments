import Data.List
import Data.List.Split

is_same l1 l2 
        | (null l1) && (not (null l2) ) = False
        | (null l2) && (not (null l1) ) = False
        | (null l2) && (null l1) = True
        | otherwise = ( sort(l1) == sort(l2) )

check_for_n_root n l = if (length l) `mod` n == 0 then length(filter (is_same (head chunks)) chunks) == length chunks else False
                       where chunks = chunksOf n l

check_string_root n l = if check_for_n_root n l then take n l else check_string_root (n+1) l

stringroot l = check_string_root 1 l