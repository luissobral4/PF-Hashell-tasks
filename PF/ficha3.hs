---Ficha 3
data Hora = H Int Int
            deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

--a)
etapavalida :: Etapa -> Bool
etapavalida (H a b,H x y) | x < a || a > 24 || x > 24 || y > 60 || b > 60 = False
                          | x == a && y < b = False
                          | otherwise = True

--b)
viagemvalida :: Viagem -> Bool
viagemvalida [h] = etapavalida h
viagemvalida (h@(H a b,H x y):h1@(H a1 b1,H x1 y1):t) | etapavalida h && etapavalida (H x y,H a1 b1) = viagemvalida (h1:t)
                                                      | otherwise = False

--c)
partidachegada :: Viagem -> Etapa
partidachegada v@((a,b):t) = (a,f (last v))
                              where f (a,b) = b

--d)
tempoviagem :: Viagem -> Int
tempoviagem [] = 0
tempoviagem (h@(H a b,H x y):t) = ((x-a)*60+y-b) + tempoviagem t

--e)
tempoespera :: Viagem -> Int
tempoespera [] = 0
tempoespera [a] = 0
tempoespera (h@(H a b,H x y):h1@(H a1 b1,H x1 y1):t) = ((a1-x)*60+b1-y) + tempoespera t

--f)
tempototal :: Viagem -> Int
tempototal v = tempoviagem v + tempoespera v
