a :: String
a = "a!"

b :: String
b = "b!"

c :: String
c = "c!"

d :: String
d = "d!"


caseSelector c = case c of 
                   "a" ->  a
                   "b" ->  b
                   "c" ->  c
                   "d" ->  d
                   _ -> []