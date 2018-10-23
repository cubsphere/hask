true = True
false = False

fr :: Eq a => [a] -> [a]
fr (x:xs) |xs == [] = []
          |head xs == x = x:(fr xs)
          |otherwise = fr xs

flcl :: Eq a => [a] -> [a]
flcl li = [x | (x,y) <- zip (init li) (tail li), x==y]

g :: [Int] -> Bool
g = foldr (\x y -> x && y) true . map (\x -> (mod) x 2 == 0) . filter (\x -> x>=10 && x<=100)

type Fabricante = String
type Potencia = Integer

data Lampada = Compacta Fabricante Potencia | Incandescente Fabricante Potencia

instance Show Lampada where
    show (Compacta f p) = "Compacta, " ++ f ++ ", " ++ (show p) ++ "W"
    show (Incandescente f p) = "Incandescente, " ++ f ++ ", " ++ (show p) ++ "W"

instance Eq Lampada where
    (Compacta f1 p1) == (Compacta f2 p2) = f2 == f1 && p1 == p2
    (Incandescente f1 p1) == (Incandescente f2 p2) = f2 == f1 && p1 == p2
    _ == _ = false

data Lustre = LustreLampada Lampada | LustreBarra (Lustre) (Lustre)

pot :: Lustre -> Integer
pot (LustreLampada (Compacta f p)) = p
pot (LustreLampada (Incandescente f p)) = p
pot (LustreBarra l1 l2) = pot l1 + pot l2

bal :: Lustre -> Bool
bal x = snd (potAndBal x)

potAndBal :: Lustre -> (Integer, Bool)
potAndBal (LustreLampada (Compacta f p)) = (p,true)
potAndBal (LustreLampada (Incandescente f p)) = (p,true)
potAndBal (LustreBarra l1 l2) = (p1+p2, b1 && b2 && p1==p2)
    where (p1,b1) = potAndBal l1
          (p2,b2) = potAndBal l2