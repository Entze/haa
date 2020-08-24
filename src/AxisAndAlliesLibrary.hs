module AxisAndAlliesLibrary where

import Data.List
import Data.Ratio

data Unit = Infantry | Artillery | Tank | AntiAirArtillery | Fighter | Bomber | Submarine | Transport | Destroyer | Cruiser | AircraftCarrier | Battleship | DamagedBattleship deriving (Show, Eq, Ord, Enum, Read, Bounded)

approx :: (Fractional a) => Rational -> a
approx rat = num / den
  where
    num = fromInteger $ numerator rat
    den = fromInteger $ denominator rat

binomial :: (Integral int) => int -> int -> Integer
binomial _ 0 = 1
binomial 0 _ = 0
binomial n 1 = fromIntegral n
binomial n k
  | n == k = 1
  | n < k = 0
  | otherwise = binomial' (fromIntegral n) (fromIntegral k)

binomial' :: Integer -> Integer -> Integer
binomial' n k = product num'' `quot` (product (den1' ++ den2'))
  where
    num'' = num' \\ common'
    den2' = den2 \\ common'
    common' = intersect num' den2
    num' = num \\ common
    den1' = den1 \\ common
    common = intersect num den1
    num = [2..n]
    den1 = [2..k]
    den2 = [2..(n-k)]

binomialDistributionOfDiceThrows :: (Integral h, Integral n, Integral f) => h -> n -> f -> Rational
binomialDistributionOfDiceThrows hits throws face = binomialDistributionOfDiceThrows' (fromIntegral hits) (fromIntegral throws) (fromIntegral face)
  where
    binomialDistributionOfDiceThrows' :: Integer -> Integer -> Integer -> Rational
    binomialDistributionOfDiceThrows' _ 0 _ = 0
    binomialDistributionOfDiceThrows' 0 n f = (1 - (f % 6))^n
    binomialDistributionOfDiceThrows' 1 n f = (n % 1) * (f % 6) * ((1 - (f % 6))^(n-1))
    binomialDistributionOfDiceThrows' 2 n f = (n^2 % 2) * p^2 * (1 - p)^(n-2) - (n % 2) * p^2 * (1 - p)^(n-2)
      where
        p = (f % 6)
    binomialDistributionOfDiceThrows' h n 1 = (1 % (6^n)) * 5^(n-h) * ((binomial n h) % 1)
    binomialDistributionOfDiceThrows' h n 2 = (1 % (3^n)) * 2^(n-h) * ((binomial n h) % 1)
    binomialDistributionOfDiceThrows' h n 3 = (1 % (2^n)) * ((binomial n h) % 1)
    binomialDistributionOfDiceThrows' h n 4 = (1 % (3^n)) * 2^h * ((binomial n h) % 1)




landUnits :: [Unit]
landUnits = [Infantry, Artillery, Tank, AntiAirArtillery]


airUnits :: [Unit]
airUnits = [Fighter, Bomber]


seaUnits :: [Unit]
seaUnits = [Submarine, Transport, Destroyer, Cruiser, AircraftCarrier, Battleship, DamagedBattleship]


attackValue :: Unit -> Int
attackValue Infantry = 1
attackValue Artillery = 2
attackValue Tank = 3
attackValue AntiAirArtillery = 0
attackValue Fighter = 3
attackValue Bomber = 4
attackValue Submarine = 2
attackValue Transport = 0
attackValue Destroyer = 2
attackValue Cruiser = 3
attackValue AircraftCarrier = 1
attackValue Battleship = 4
attackValue DamagedBattleship = attackValue Battleship


attacks :: Unit -> [Unit]
attacks Infantry = landUnits ++ airUnits
attacks Artillery = landUnits ++ airUnits
attacks Tank = landUnits ++ airUnits
attacks AntiAirArtillery = airUnits
attacks Fighter = landUnits ++ airUnits ++ seaUnits
attacks Bomber = landUnits ++ airUnits ++ seaUnits
attacks Submarine = seaUnits
attacks Transport = []
attacks Destroyer = airUnits ++ seaUnits
attacks Cruiser = landUnits ++ airUnits ++ seaUnits
attacks AircraftCarrier = airUnits ++ seaUnits
attacks Battleship = landUnits ++ airUnits ++ seaUnits
attacks DamagedBattleship = attacks Battleship


defenseValue :: Unit -> Int
defenseValue Infantry = 2
defenseValue Artillery = 2
defenseValue Tank = 2
defenseValue AntiAirArtillery = 1
defenseValue Fighter = 4
defenseValue Bomber = 1
defenseValue Submarine = 1
defenseValue Transport = 0
defenseValue Destroyer = 2
defenseValue Cruiser = 3
defenseValue AircraftCarrier = 2
defenseValue Battleship = 4
defenseValue DamagedBattleship = attackValue Battleship


onDestruction :: Unit -> [Unit]
onDestruction Battleship = [DamagedBattleship]
onDestruction _ = []


numberOfShots :: Unit -> Int
numberOfShots AntiAirArtillery = 3
numberOfShots _ = 1


unitCost :: Unit -> Int
unitCost Infantry = 3
unitCost Artillery = 4
unitCost Tank = 6
unitCost AntiAirArtillery = 5
unitCost Fighter = 10
unitCost Bomber = 12
unitCost Submarine = 6
unitCost Transport = 7
unitCost Destroyer = 8
unitCost Cruiser = 12
unitCost AircraftCarrier = 14
unitCost Battleship = 20
unitCost DamagedBattleship = unitCost Battleship


lowestIPCFirstLandUnits = sortOn unitCost landUnits
lowestIPCFirstAirUnits = sortOn unitCost airUnits
lowestIPCFirstSeaUnits = sortOn unitCost seaUnits
lowestIPCFirstSeaUnitsOpt = nub (Battleship:lowestIPCFirstSeaUnits)
lowestIPCFirst = sortOn unitCost (lowestIPCFirstLandUnits ++ lowestIPCFirstAirUnits ++ lowestIPCFirstSeaUnits)
lowestIPCFirstOpt = nub (Battleship:lowestIPCFirst)


generalCombatAttackValues :: [Unit] -> [Int]
generalCombatAttackValues attackingArmy = sort (generalCombatAttackValues' (sort attackingArmy))

generalCombatAttackValues' :: [Unit] -> [Int]
generalCombatAttackValues' (Infantry:army)
  | elem Artillery army = 2:2:(generalCombatAttackValues' (delete Artillery army))
  | otherwise = 1:(generalCombatAttackValues' army)
generalCombatAttackValues' (unit:army) = (attackValue unit):(generalCombatAttackValues' army)
generalCombatAttackValues' [] = []

generalCombatDefenseValues :: [Unit] -> [Int]
generalCombatDefenseValues = (map defenseValue) . sort

generalCombatAppliedLosses :: [Unit] -> [Unit] -> Int -> [Unit]
generalCombatAppliedLosses army _ hits
  | hits <= 0 = army
generalCombatAppliedLosses [] _ _ = []
generalCombatAppliedLosses army lossProfile hits = generalCombatAppliedLosses' army lossProfile hits

generalCombatAppliedLosses' :: [Unit] -> [Unit] -> Int -> [Unit]
generalCombatAppliedLosses' army _ 0 = army
generalCombatAppliedLosses' [] _ _ = []
generalCombatAppliedLosses' army lossProfile@(next:lp) hits
  | next `elem` army = generalCombatAppliedLosses' ((delete next army) ++ onDestruction next) lossProfile (hits-1)
  | otherwise = generalCombatAppliedLosses' army lp hits


airDefenseValues :: [Unit] -> [Unit] -> [Int]
airDefenseValues attackingArmy defendingArmy = replicate nrOfShots (defenseValue AntiAirArtillery)
  where
    nrOfShots = min (nrOfAAA * 3) nrOfAirUnits
    nrOfAAA = count AntiAirArtillery defendingArmy
    nrOfAirUnits = countIf (`elem` airUnits) attackingArmy

airDefenseAppliedLosses :: [Unit] -> [Unit] -> Int -> [Unit]
airDefenseAppliedLosses army lossProfile hits = generalCombatAppliedLosses army (lossProfile \\ airUnits) hits


offshoreBombardmentValues :: [Unit] -> [Int]
offshoreBombardmentValues = (map attackValue) . sort

offshoreBombardmentAppliedLosses :: [Unit] -> [Unit] -> Int -> [Unit]
offshoreBombardmentAppliedLosses = generalCombatAppliedLosses


partitionHits :: Int -> [[Int]]
partitionHits hits = [[ones, twos, threes, fours] |
                      ones <- [0..hits],
                      twos <- [0..hits], ones + twos <= hits,
                      threes <- [0..hits], ones + twos + threes <= hits,
                      fours <- [hits - (ones + twos + threes)]]


probabilityOfHits :: [Int] -> Int -> Ratio Integer
probabilityOfHits values hits = 1%1
  where
    validPartitions = filter ((<= fours) . (!! 3)) $ filter ((<= threes) . (!! 2)) $ filter ((<= twos) . (!! 1)) $ filter ((<= ones) . (!! 0)) (partitionHits hits)
    ones = count 1 values
    twos = count 2 values
    threes = count 3 values
    fours = count 4 values



countIf :: (a -> Bool) -> [a] -> Int
countIf predicate = length . (filter predicate)

count :: Eq a => a -> [a] -> Int
count el = countIf (== el)
