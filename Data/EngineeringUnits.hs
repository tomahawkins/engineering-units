{- |

A numeric type to manage engineering units.

Provides automatic unit conversion:

> > print $ value (1 * h + 1 * min') s        -- Time in seconds of 1 hour + 1 minute.
> 3660.0

Automatic unit reduction:

> > print $ value (20 * gpm * 10 * min') gal  -- Note the minutes cancel each other out.
> 200.0

And consistency checking:

> > print $ value (22 * mph + 3 gal) mph      -- Note that speed (m/s) is inconsistent with volume (m^3).
> *** Exception: Incompatible units: [M]/[S] /= [M,M,M]/[]

And defining new units is easy:

> -- | Millimeters, a measure of distance, is 1/1000 of a meter.
> mm :: Value
> mm = 0.001 * m

> -- | Joules, a measure of energy, is one newton meter.
> j :: Value
> j = n * m

-}
module Data.EngineeringUnits
  ( Value
  , value
  -- * Distance
  , m    
  , cm   
  , mm
  , km
  , in'  
  , ft   
  , mi   
  -- * Area
  , cm2  
  , in2  
  -- * Volume
  , cm3  
  , ml
  , l
  , in3  
  , gal  
  -- * Mass
  , kg   
  , g
  , mg
  -- * Force
  , n    
  , lbs  
  -- * Rotation
  , rev  
  -- * Speed
  , mph  
  , kph
  -- * Rotational Rate
  , rpm  
  -- * Time
  , s    
  , ns
  , us
  , ms
  , min' 
  , h
  -- * Energy
  , j
  , btu
  -- * Power
  , hp   
  , w    
  , kw   
  -- * Pressure
  , psi  
  , bar
  -- * Flow
  , gpm  
  , lpm
  -- * Misc
  , s2   
  , radsPerRev
  ) where

import Data.List

-- | The base units for distance, time, mass, and revolutions.
data Unit = M | S | Kg | Rev deriving (Eq, Ord, Show)

-- | A value is a number and its associated units.
data Value = Value Double [Unit] [Unit] deriving (Show, Eq, Ord)

instance Num Value where
  a@(Value aV _ _) + b@(Value bV _ _) = same a b $ aV + bV
  a@(Value aV _ _) - b@(Value bV _ _) = same a b $ aV - bV
  Value aV aN aD * Value bV bN bD = normalize $ Value (aV * bV) (aN ++ bN) (aD ++ bD)
  fromInteger a = Value (fromIntegral a) [] []
  negate (Value v n d) = Value (negate v) n d
  abs    (Value v n d) = Value (abs    v) n d
  signum (Value v n d) = Value (signum v) n d

instance Fractional Value where
  Value aV aN aD / Value bV bN bD = normalize $ Value (aV / bV) (aN ++ bD) (aD ++ bN)
  recip (Value v n d) = Value (recip v) d n
  fromRational a = Value (fromRational a) [] []

instance Floating Value where
  pi    = Value pi [] []
  (Value a n d) ** b = case b of
    Value 2 [] [] -> normalize $ Value (a ** 2) (n ++ n) (d ++ d)
    _ -> error "Not supported (**) where power is not a unitless value of 2."
  sqrt v@(Value a n d) = Value (sqrt a) (sqrt' n) (sqrt' d)
    where
    sqrt' a = case a of
      []  -> []
      [_] ->  error $ "Sqrt failed on unit reduction: " ++ show v
      a : b : c
        | a == b -> a : sqrt' c
        | otherwise -> error $ "Sqrt failed on unit reduction: " ++ show v
  exp   = error "Not supported yet for Value: exp  "
  log   = error "Not supported yet for Value: log  "
  sin   = unitless "sin" sin
  cos   = unitless "cos" cos
  tan   = unitless "tan" tan
  asin  = unitless "asin" asin
  acos  = unitless "acos" acos
  atan  = unitless "atan" atan
  sinh  = error "Not supported yet for Value: sinh "
  cosh  = error "Not supported yet for Value: cosh "
  asinh = error "Not supported yet for Value: asinh"
  acosh = error "Not supported yet for Value: acosh"
  atanh = error "Not supported yet for Value: atanh"

unitless :: String -> (Double -> Double) -> Value -> Value
unitless msg f (Value a n d)
  | null n && null d = Value (f a) [] []
  | otherwise        = error $ msg ++ " requires unitless value."

-- | Normalize a value, i.e. simplify and sort units.
normalize :: Value -> Value
normalize a@(Value _ n d) = order $ reduce (n ++ d) a
  where
  reduce :: [Unit] -> Value -> Value
  reduce [] a = a
  reduce (a : rest) (Value v n d)
    | elem a n && elem a d = reduce rest $ Value v (delete a n) (delete a d)
    | otherwise            = reduce rest $ Value v n d
  order :: Value -> Value
  order (Value v n d) = Value v (sort n) (sort d)

-- | Create a value if two units are compatible.
same :: Value -> Value -> Double -> Value
same (Value _ aN aD) (Value _ bN bD) v
  | aN == bN && aD == bD = Value v aN aD
  | otherwise = error $ "Incompatible units: " ++ show aN ++ "/" ++ show aD ++ " /= " ++ show bN ++ "/" ++ show bD

-- | Extract a value in the given units.
--
--   > value val units
--   > value (2.54 * cm) in'
value :: Value -> Value -> Double
value val@(Value v _ _) units@(Value k _ _) = result
  where
  Value result _ _ = same val units $ v / k

-- | Meters.
m    = Value 1 [M]   []
-- | Seconds.
s    = Value 1 [S]   []
-- | Kilograms.
kg   = Value 1 [Kg]  []
-- | Revolutions.
rev  = Value 1 [Rev] []
-- | Seconds ^ 2.
s2   = s * s
-- | Centimeters.
cm   = 0.01 * m
-- | Centimeters ^ 2.
cm2  = cm * cm
-- | Centimeters ^ 3.
cm3  = cm * cm * cm
-- | Millimeters.
mm   = 0.1 * cm
-- | Kilometers.
km   = 1000 * m
-- | Milliliters.
ml   = cm3
-- | Liters.
l    = 1000 * ml
-- | Grams.
g    = 0.001 * kg
-- | Milligrams.
mg   = 0.001 * g
-- | Inches.
in'  = 2.54 * cm
-- | Inches ^ 2.
in2  = in' * in'
-- | Inches ^ 3.
in3  = in' * in' * in'
-- | Feet.
ft   = 12 * in'
-- | Nanoseconds.
ns = 0.000000001 *s
-- | Microseconds.
us = 0.000001 *s
-- | Milliseconds.
ms = 0.001 * s
-- | Minutes.
min' = 60 * s
-- | Hours.
h    = 60 * min'
-- | Newtons.
n    = kg * m / s2
-- | Pounds.
lbs  = 4.4482216152605 * n
-- | Miles.
mi   = 5280 * ft
-- | Gallons.
gal  = 231 * in3
-- | Horsepower.
hp   = 33000 * ft * lbs / min'
-- | Kilowatts.
kw   = 1.3410220888 * hp
-- | Watts.
w    = 0.001 * kw
-- | Pounds per inch ^ 2.
psi  = lbs / in2
-- | Bar.
bar  = 14.5037738 * psi
-- | Miles per hour.
mph  = mi / h
-- | Kilometers per hour.
kph  = km / h
-- | Revolutions per minute.
rpm  = rev / min'
-- | Gallons per minute.
gpm  = gal / min'
-- | Liters per minute.
lpm  = l / min'
-- | Radians per revolution: 2 * pi / rev
radsPerRev = 2 * pi / rev
-- | Joules.
j    = n * m
-- | BTUs.
btu  = 1055.05585 * j


