max2 :: Int -> Int -> Int
max2 x y =if(x>y)then x else y

max3 :: Int->Int->Int->Int
max3 x y z= max2 x (max2 y z)

nraizes :: Double->Double->Double->Int
nraizes a b c | (b^2 - 4*a*c) > 0 = 2
              | (b^2 - 4*a*c) == 0 = 1
              | otherwise = 0

raizes :: (Double,Double,Double)->[Double]
raizes (a,b,c) | nraizes a b c == 2 = [((-b+sqrt(b^2-4*a*c))/(2*a)),((-b-sqrt(b^2-4*a*c))/(2*a))]
               | nraizes a b c == 1 = [(-b)]
               | otherwise = []

type Hora =(Int,Int)
hvalida :: Hora->Bool
hvalida (h,m) = (h >= 0 && h <24) && (m >=0 && m<60)

hseguida :: Hora->Hora->Bool
hseguida (h,m) (h1,m1) = (h>h1) || (h==h1 && m>m1)

convhoras :: Hora->Int
convhoras (h,m) = h*60 +m

convmin :: Int->Hora
convmin m =(div m 60,mod m 60)  

dif :: Hora->Hora->Int
dif (h,m) (h1,m1) = ((h-h1)*60+(m-m1))

somarmin :: Hora->Int->Hora
somarmin (h,m) m1= if ((m1+m)>60) then (h+div (m+m1) 60,mod (m+m1) 60) else (h,m+m1)

data Semaforo =  Verde | Amarelo | Vermelho deriving (Show,Eq)
next :: Semaforo->Semaforo
next cor | cor == Verde =Amarelo
         | cor == Amarelo=Vermelho
         | cor == Vermelho=Verde

stop :: Semaforo->Bool
stop cor = cor==Vermelho

safe :: Semaforo->Semaforo->Bool
safe a1 a2 | stop a1 && not (stop a2)=True
            | stop a2 && not (stop a1)=True
            | otherwise =False

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)
posx :: Ponto->Double
posx (Cartesiano x y) = x
posx (Polar r a) = abs(r*cos (a))

posy :: Ponto->Double
posy (Cartesiano x y) = y
posy (Polar r a) = abs(r*sin (a))

dist :: Ponto->Ponto->Double
dist (Cartesiano x y) (Cartesiano x1 y1)=abs(sqrt((x-x1)^2+(y-y1)^2))

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto| Triangulo Ponto Ponto Ponto deriving (Show,Eq)

poligono :: Figura->Bool
poligono (Triangulo a b c)=True
poligono (Rectangulo d e)=True
poligono (Circulo f g)=False

vertices :: Figura->[Ponto]
vertices (Triangulo a b c)=[a,b,c]
vertices (Rectangulo d e)=[d,e]
vertices (Circulo f g)=[]

area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2 -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Rectangulo p4 p5)=
    let k = dist p4 p5
    in ((sqrt(2)/2)*k)*((sqrt(2)/2)*k)