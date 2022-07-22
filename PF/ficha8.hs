 -- 1)

data Frac = F Integer Integer
           --deriving Show

normaliza :: Frac -> Frac
normaliza (F x y) = F x1 y1
                  where m = mdc (abs x) (abs y)
                        x1 = (s*(div (abs x) m))
                        y1 = (div (abs y) m)
                        s = if (x * y > 0) then 1 else (-1)

mdc :: Integer -> Integer -> Integer
mdc x y | x == y = x
        | x > y = mdc (x-y) y
        | x < y = mdc x (y-x)

instance Eq Frac where
  --eqf :: Frac -> Frac -> Bool
  a == b = x == x2 && y == y2
           where (F x y) = normaliza a
                 (F x2 y2) = normaliza b

instance Ord Frac where
  compare (F x y) (F a b) = compare (fromIntegral x /fromIntegral y) (fromIntegral a /fromIntegral b)

instance Show Frac where
  show (F x y) = show x ++ "/" ++ show y

instance Num Frac where
  (F x y) + (F x2 y2) = normaliza (F (x*y2+x2*y) (y * y2))
  (F x y) * (F x2 y2) = normaliza (F (x * x2) (y*y2))
  (F x y) - (F x2 y2) = normaliza (F (x*y2-x2*y) (y * y2))
  negate (F x y) = F (-x) y
  abs (F x y) = F (abs x) (abs y)
  signum (F x y) = F ( signum x * signum y) 1
  fromInteger n = F n 1

funcaomaiordobro a l = filter (\x -> x > 2*a) l


 -- 2)

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

-- a)
instance Show a => Show (Exp a) where
  show (Const a) = show a
  show (Simetrico a) = "-" ++ show a
  show (Mais a b) = show a ++ "+" ++ show b
  show (Menos a b) = show a ++ "-" ++ show b
  show (Mult a b) = show a ++ "*" ++ show b

instance (Num a,Eq a) => Eq (Exp a) where
  a == b = calcula a == calcula b

calcula :: Num a => Exp a -> a
calcula (Const x) = x
calcula (Simetrico x) = (-1)*calcula x
calcula (Mais x y) = calcula x + calcula y
calcula (Menos x y) = calcula x - calcula y
calcula (Mult x y) = calcula x * calcula y

instance Num a => Num (Exp a) where
  a + b = Mais a b
  a - b = Menos a b
  a * b = Mult a b
  negate a = Simetrico a
  abs a = signum a
  signum x = Const (signum (calcula x))
  fromInteger x = Const (fromInteger x)

-- 3)

data Movimento = Credito Float | Debito Float
               --deriving Show
data Data = D Int Int Int
           deriving Eq
data Extracto = Ext Float [(Data, String, Movimento)]
              --deriving Show

instance Ord Data where
  compare (D x y z) (D x2 y2 z2) = compare (z,y,x) (z2,y2,x2)

instance Show Data where
  show (D x y z) = (show x) ++ "/" ++ (show y) ++ "/" ++ (show z)

ordena :: Extracto -> Extracto
ordena (Ext x l) = (Ext x (ordenalista l))

ordenalista :: [(Data,String,Movimento)] -> [(Data,String,Movimento)]
ordenalista [] = []
ordenalista (h:t) = inseredata h (ordenalista t)
inseredata :: (Data,String,Movimento) -> [(Data,String,Movimento)] -> [(Data,String,Movimento)]
inseredata d [] = [d]
inseredata a@(x,y,z)
           (b@(x2,y2,z2):t) | x < x2 = a : b : t
                            | otherwise = b : inseredata a t

instance Show Extracto where
  show (Ext a (b:t)) = "Saldo anterior: " ++ show a ++ "                                                          "
                             ++ "-------------------------------------------------                               "
                             ++ "Data          Descricao       Credito      Debito                               "
                             ++ "-------------------------------------------------                               "
                             ++ concat(map f (b:t))
                             ++ "-------------------------------------------------                               "
                             ++ "Saldo actual: " ++ show (a + s (b:t))

f (a,b,Debito x) = show a ++ "      "  ++ show b ++ "                     " ++ show x ++ "                                   "
f (x,y,Credito a) = show x ++ "    "  ++ show y ++ "     " ++ show a ++ "                                                  "

--(Data,String,Movimento) where
s [] = 0
s ((x,y,Debito a):t) = -a + s t
s ((x,y,Credito a):t) = a + s t
