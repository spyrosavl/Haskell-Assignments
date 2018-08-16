shot_balloons height [] = []
shot_balloons height (h:t) = if h==height then shot_balloons (height-1) t else (h:(shot_balloons height t))

pinkballoons [] = 0
pinkballoons (h:t) = if null(new_list) then 1 else pinkballoons new_list + 1
					where new_list = shot_balloons h (h:t)