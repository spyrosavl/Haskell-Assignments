row s n = first_n_minus_1 ++ [(last first_n_minus_1)+(n-1)*(n-2)/2]
          where first_n_minus_1 = [s..s+n-2]

matrix s n line = if line < n-1 then [current_row] ++ (matrix next_start n (line+1))  else [current_row]
                  where current_row = row s n
                        offset = (n-1)*(n-2)/2
                        next_start = if line<n-2 then last current_row+1 else (n-1+offset)^2-offset-(n-2)