myinternet x [] = x
myinternet x (h:t) = if h <= x then x-h + myinternet x t else myinternet x t - (h-x)