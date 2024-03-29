------------------------------------------------------------------
-- Functioneel programmeren
-- Opdracht 1: Functionele figuren (versie 1.4)
--
-- Student: Joshua Van Synghel
-- Nummer: 852078066
--
------------------------------------------------------------------
module Figuur where

import Data.Char
import System.IO

type Figuur a = Pos -> a

type Pos = (Double, Double) -- (x, y)

schaakbord :: Figuur Bool
schaakbord (x, y) = even (round x) == even (round y)

------------------------------------------------------------------
-- 1. ASCII rendering

type Dimensie = (Int, Int) -- (breedte, hoogte)

coordinaten :: Dimensie -> [[(Int, Int)]]
coordinaten (b, h) = [[ (x, y) | x <- [0..b-1] ] | y <- [0..h-1] ]

render :: Dimensie -> Figuur a -> [[a]] 
render (x, y) f = map (map (f . naarPositie(x, y))) (coordinaten(x, y))

naarPositie :: Dimensie -> (Int, Int) -> Pos
naarPositie d (x, y) = (fromIntegral x*2/b-1, 1-fromIntegral y*2/h)
  where
    b  = fromIntegral (fst d-1)  -- bereik x van 0 - (x-1)
    h  = fromIntegral (snd d-1)  -- bereik y van 0 - (y-1)

boolChar :: Bool -> Char
boolChar True  = '@'
boolChar False = '.'

-- functie verwacht nog een positie
-- vb boolChar (schaakbord (32, 32))
verander :: (a -> b) -> Figuur a -> Figuur b
verander v f = (v . f)

toon :: Figuur Bool -> IO ()
toon = putStrLn . unlines . render (32, 20) . verander boolChar
------------------------------------------------------------------
-- 2. Basisvormen

cirkel :: Double -> Figuur Bool
cirkel r (x, y) | x^2 + y^2 <= r^2 = True  -- vergelijking van een cirkel
                | otherwise        = False

vierkant :: Double -> Figuur Bool
vierkant b (x, y) | b/2 <= abs x || b/2 <= abs y = False  -- breedte wordt gedeeld door 2 om vierkant
                  | otherwise                    = True   -- in breedte als hoogte gelijk te verdelen over axis

driehoek :: Figuur Bool
driehoek (x, y) | y > 2*x + 1  = False  -- vergelijking rechte van linksonder naar bovenmidden
                | y > -2*x + 1 = False  -- vergelijking rechte van rechtsonder naar bovenmidden
                | y < -1       = False  -- vergelijking om onderkant driehoek af te bakenen
                | otherwise    = True

------------------------------------------------------------------
-- 3. Transformaties

transform :: (Pos -> Pos) -> Figuur a -> Figuur a
transform t f = \(x, y) -> (f . t) (x, y)  

verschuif :: (Double, Double) -> Figuur a -> Figuur a
verschuif (b, h) f = \(x, y) -> f (x-b, y-h)  

schaal :: Double -> Double -> Figuur a -> Figuur a
schaal b h f = \(x, y) -> f (x/b, y/h)

infixr 7 #
infixr 7 //

(#) :: (Double, Double) -> Figuur a -> Figuur a -- verschuiven
(#) (b, h) f = verschuif (b, h) f

(//) :: Double -> Figuur a -> Figuur a -- schalen
(//) s f = schaal s s f 

------------------------------------------------------------------
-- 4. Transformaties met poolcoordinaten

type Polar = (Double, Double) -- (afstand, hoek)

toPolar :: Pos -> Polar
toPolar (x, y) = (sqrt (x^2 + y^2), hoek) 
  where hoek  | x == 0     = pi/2 * signum y
              | x < 0      = atan (y/x) + pi
              | otherwise  = atan (y/x)

fromPolar :: Polar -> Pos
fromPolar (r, h) = (cos h*r, sin h*r)

transPolar :: (Polar -> Polar) -> Figuur a -> Figuur a
transPolar t f (x, y) = transform t f (toPolar(x, y))

roteer :: Double -> Figuur a -> Figuur a
roteer d f = transPolar (\(r, a) -> (r, a-d)) (f . fromPolar)

krul :: Double -> Figuur a -> Figuur a
krul d f = transPolar (\(r, a) -> (r, a-d*r)) (f . fromPolar)

------------------------------------------------------------------
-- 5. Composities

compositie :: (a -> b -> c) -> Figuur a -> Figuur b -> Figuur c
compositie c fa fb = \(x, y) -> c (fa (x, y)) (fb (x, y))

(<+>) :: Figuur Bool -> Figuur Bool -> Figuur Bool
(<+>) fa fb = compositie (||) fa fb

(<&>) :: Figuur Bool -> Figuur Bool -> Figuur Bool
(<&>) fa fb = compositie (&&) fa fb

(<->) :: Figuur Bool -> Figuur Bool -> Figuur Bool
(<->) fa fb = compositie (xor) fa fb 
   where
    xor = \a b -> a /= b

ring :: Double -> Double -> Figuur Bool
ring da db = (cirkel da) <-> (cirkel db)

box :: Double -> Double -> Figuur Bool
box da db = (vierkant da) <-> (vierkant db)

------------------------------------------------------------------
-- 6. Kleuren

type Kleur = (Double, Double, Double, Double) -- (rood, groen, blauw, alpha)

alpha :: Kleur -> Double
alpha (_, _, _, a) = a

rood, groen, blauw, zwart, wit, leeg :: Kleur
rood   = (1,0,0,1)
groen  = (0,1,0,1)
blauw  = (0,0,1,1)
zwart  = (0,0,0,1)
wit    = (1,1,1,1)
leeg   = (0,0,0,0) -- volledig doorzichtig

veranderKleur :: (Double -> Double) -> Kleur -> Kleur
veranderKleur f (r, g, b, a) = (f r, f g, f b, f a)

transparant :: Double -> Kleur -> Kleur
transparant d = veranderKleur (\x -> x * d)

zipKleur :: (Double -> Double -> Double) -> Kleur -> Kleur -> Kleur
zipKleur f (r, g, b, a) (w, x, y, z) = (f r w, f g x, f b y, f a z)

mixKleur :: Double -> Kleur -> Kleur -> Kleur
mixKleur d k1 k2 = zipKleur (+) (veranderKleur (* d) k1) (veranderKleur (* (1-d)) k2)

------------------------------------------------------------------
-- 7. PPM rendering

headerPPM :: Dimensie -> String
headerPPM (x, y) = "P6 " ++ (show x) ++ " " ++ (show y) ++ " 255\n"

kleurPPM :: Kleur -> String
kleurPPM (r, g, b, _) = intToChr r : intToChr g : intToChr b : []
   where
    intToChr = chr . abs . round . (*255)

maakPPM :: Dimensie -> Figuur Kleur -> String
maakPPM (x, y) f = headerPPM (x, y) ++ 
                   concat (map (kleurPPM . f . naarPositie (x, y)) (concat(coordinaten(x, y))))

schrijf :: FilePath -> Figuur Kleur -> IO ()
schrijf file = writeBinaryFile file . maakPPM (300, 300)

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile file s = do
  h <- openBinaryFile file WriteMode
  hPutStr h s
  hClose h
  
kleur :: Kleur -> Figuur Bool -> Figuur Kleur
kleur = kleurMet . const

kleurMet :: Figuur Kleur -> Figuur Bool -> Figuur Kleur
kleurMet = compositie (\k b -> if b then k else leeg)

------------------------------------------------------------------
-- 8. Kleuren composities

over :: Kleur -> Kleur -> Kleur
over (r, g, b, a) (w, x, y, z) = (r + w * (1 - a), 
                                  g + x * (1 - a),
                                  b + y * (1 - a),
                                  a + z * (1 - a))

(<#>) :: Figuur Kleur -> Figuur Kleur -> Figuur Kleur
(<#>) fa fb = compositie (over) fa fb

stapel :: [Figuur Kleur] -> Figuur Kleur
stapel fks = foldr (<#>) (\(x, y) -> leeg) fks

------------------------------------------------------------------
-- 9. Gradienten

gradient :: Kleur -> Kleur -> Figuur Kleur
gradient k1 k2 = \(x, _) -> mixKleur ((1 - x)/2) k1 k2

gradientCirkel :: Kleur -> Kleur -> Figuur Kleur
gradientCirkel k1 k2 = \(x, y) -> if fst (toPolar (x, y)) <= 1 
                                     then mixKleur (fst (toPolar (x, y))) k1 k2
                                  else
                                     k1                                     

------------------------------------------------------------------
-- 10. Voorbeelden

fig1, fig2, fig3, fig4 :: Figuur Kleur
fig1 = kleur (transparant 0.6 rood) (cirkel 0.9) 
       <#> kleur wit (0.4 // schaakbord)
fig2 = kleur (transparant 0.6 (mixKleur 0.5 blauw wit)) ((0.4,0.4) # vierkant 1) 
       <#> kleur rood (cirkel 0.5)
fig3 = stapel [ kleur (transparant 0.8 rood) ((x/2,-0.3) # ring 0.3 0.45) 
              | x <- [-1..1] ]
       <#> kleur wit ((0.4 // schaakbord) <&> driehoek)
fig4 = kleurMet ((0.2,0.2) # 1.2 // gradientCirkel zwart wit) (cirkel 0.8)

type Serie = Double -> Figuur Kleur

serie :: Double -> Serie -> Figuur Kleur
serie n f = stapel (map f [0..n])

spiraal :: Serie -> Serie
spiraal g t = verschuif (fromPolar (t/10, t*0.8)) (g t)

eindvb :: Figuur Kleur
eindvb = figs <#> bollen <#> grid
  where
    paars   =  transparant 0.9 (mixKleur 0.5 rood blauw)
    lgrijs  =  mixKleur 0.3 zwart wit
    bol     =  kleurMet ((0.3,0.3) # 1.2 // gradientCirkel paars rood) (cirkel 1)
    bollen  =  (-0.1, -0.1) # serie 9 (spiraal (\t -> (0.05 + t/40) // bol))
    grid    =  kleurMet achter (0.3 // krul 0.1 schaakbord)
    achter  =  roteer (pi/6) (gradient zwart lgrijs)
    figs    =  (-0.5,0.5) # kleurMet grad (0.15 // foldr1 (<+>) lijst)
    lijst   =  [  (3,-0.5)  # schaal 1.1 0.9 (ring 0.6 1)
               ,  (2,2)     # ring 0.4 1
               ,  (-2,-2)   # roteer (pi/4) (box 0.6 1.5)
               ,  (0,2)     # cirkel 1 <-> ((0.5,0) # cirkel 1)
               ,  roteer (-pi/4) (driehoek <&> cirkel 1) 
               ]
    grad    =  gradient (transparant 0.85 groen) (transparant 0.85 blauw)


-- breakdown van eindvb om fout te analyseren)

figsFull = (-0.5,0.5) # kleurMet (gradient (transparant 0.85 groen) 
   (transparant 0.85 blauw)) (0.15 // foldr1 (<+>) 
   ([  (3,-0.5)  # schaal 1.1 0.9 (ring 0.6 1)
               ,  (2,2)     # ring 0.4 1
               ,  (-2,-2)   # roteer (pi/4) (box 0.6 1.5)
               ,  (0,2)     # cirkel 1 <-> ((0.5,0) # cirkel 1)
               ,  roteer (-pi/4) (driehoek <&> cirkel 1) 
               ]))

bollenFull = (-0.1, -0.1) # serie 9 (spiraal (\t -> (0.05 + t/40) // (
  kleurMet ((0.3,0.3) # 1.2 // gradientCirkel (transparant 0.9 (mixKleur 0.5 rood blauw)) rood) (cirkel 1))))

-- fout zit in volgende functie

gridFull = kleurMet (roteer (pi/6) (gradient zwart (mixKleur 0.3 zwart wit))) (0.3 // krul 0.1 schaakbord)