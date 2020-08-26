module AxisAndAlliesLibrary where

import Data.List
import Data.Ratio
import Data.Tuple.Extra

data Unit = Infantry | Artillery | Tank | AntiAirArtillery | Fighter | Bomber | Submarine | Transport | Destroyer | Cruiser | AircraftCarrier | Battleship | DamagedBattleship deriving (Show, Eq, Ord, Enum, Read, Bounded)

data GameTree = Win [Unit] Rational | Loss [Unit] Rational | Draw Rational | Inconclusive [Unit] [Unit] Rational [GameTree] deriving (Show, Eq)

binomials :: [[Integer]]
binomials = [[1],[1],[1,2],[1,3],[1,4,6],[1,5,10],[1,6,15,20],[1,7,21,35]] ++ [1:[l + r | k <- [1..(n `quot` 2)], l <- [binomial (n - 1) (k - 1)], r <- [binomial (n-1) (k)]] | n <- [8..]]

binomial :: (Integral int) => int -> int -> Integer
binomial _ 0 = 1
binomial 0 _ = 0
binomial n 1 = fromIntegral n
binomial n k
  | n == k = 1
  | n < k = 0
  | k > (n `quot` 2) = binomial n (n-k)
  | n > (fromIntegral (maxBound :: Int)) = binomial' (fromIntegral n) (fromIntegral k)
  | otherwise = (binomials !! n') !! k'
  where
    n' = fromIntegral n
    k' = fromIntegral k

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
    binomialDistributionOfDiceThrows' 0 0 _ = 1
    binomialDistributionOfDiceThrows' _ 0 _ = 0
    binomialDistributionOfDiceThrows' 0 1 f = (1 - (f % 6))
    binomialDistributionOfDiceThrows' h n f = (1 % (6^n)) * ((f^h) % 1) * ((binomial n h) % 1) * (((6 - f)^(n-h)) % 1)


cumulativeDensity :: Num a => [a] -> [a]
cumulativeDensity probabilities = cumulativeDensity' probabilities 0
  where
    cumulativeDensity' [] _ = []
    cumulativeDensity' (p:ps) c = p':(cumulativeDensity' ps p')
      where
        p' = c+p

groupByCumulativeDensity :: (Num a, Ord a) => [a] -> a -> [(Int, a)]
groupByCumulativeDensity probabilities epsilon = groupByCumulativeDensity' probabilities 0 0
  where
    groupByCumulativeDensity' [] _ _ = []
    groupByCumulativeDensity' (p:ps) c n
      | p' < epsilon = groupByCumulativeDensity' ps p' (n+1)
      | otherwise = (n, p'):(groupByCumulativeDensity' ps 0 (n+1))
        where
          p' = p + c

drawDensityGraphWithDescriptionLine :: (Show a, Show b, RealFrac b) => a -> b -> b -> String
drawDensityGraphWithDescriptionLine description density total = (replicate cells '#') ++ (' ':((show description)) ++ ": " ++ (show value))
  where
    value = (density / total)
    cells = round (value * 40)


drawDensityGraphWithDescriptionLines :: (Show a, Show b, RealFrac b) => [a] -> [b] -> [String]
drawDensityGraphWithDescriptionLines [] _ = []
drawDensityGraphWithDescriptionLines _ [] = []
drawDensityGraphWithDescriptionLines descriptions densities = map (uncurry3 drawDensityGraphWithDescriptionLine) (zip3 descriptions densities (repeat s))
  where
    s = sum densities

drawDensityGraphWithDescription :: (Show a, Show b, RealFrac b) => [a] -> [b] -> String
drawDensityGraphWithDescription descriptions densities = unlines (drawDensityGraphWithDescriptionLines descriptions densities)


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


probabilityOfHits :: [Int] -> [Rational]
probabilityOfHits values = probabilityOfHitsWith ones twos threes fours
  where
    ones = count 1 values
    twos = count 2 values
    threes = count 3 values
    fours = count 4 values

probabilityOfHitsWith :: Int -> Int -> Int -> Int -> [Rational]
probabilityOfHitsWith ones twos threes fours = map snd reducedProbabilities
  where
    singleProbabilities = sortOn fst (probabilityOfHitsIndexed ones twos threes fours)
    reducedProbabilities = reduce' singleProbabilities
    reduce' :: [(Int, Rational)] -> [(Int,Rational)]
    reduce' ((a,r1):(b,r2):rs)
      | a == b = reduce' ((a, r1 + r2):rs)
      | otherwise = (a,r1):(reduce' ((b,r2):rs))
    reduce' x = x

probabilityOfHitsIndexed :: Int -> Int -> Int -> Int -> [(Int, Rational)]
probabilityOfHitsIndexed ones twos threes fours = reduce' (probabilityOfHitsSingleIndexed ones twos threes fours)
  where
    reduce' :: [(Int, [Rational])] -> [(Int, Rational)]
    reduce' ((n, p):ls) = (n, product p):(reduce' ls)
    reduce' [] = []

probabilityOfHitsSingleIndexed :: Int -> Int -> Int -> Int -> [(Int, [Rational])]
probabilityOfHitsSingleIndexed ones twos threes fours = singleProbabilities
  where
    singleProbabilities = [(one + two + three + four,
                            [(onesProbability !! one),
                             (twosProbability !! two),
                             (threesProbability !! three),
                             (foursProbability !! four)]) |
                            one <- [0..ones],
                            two <- [0..twos],
                            three <- [0..threes],
                            four <- [0..fours]]
    onesProbability = map ((flip ((flip binomialDistributionOfDiceThrows) ones)) 1) [0..ones]
    twosProbability = map ((flip ((flip binomialDistributionOfDiceThrows) twos)) 2) [0..twos]
    threesProbability = map ((flip ((flip binomialDistributionOfDiceThrows) threes)) 3) [0..threes]
    foursProbability = map ((flip ((flip binomialDistributionOfDiceThrows) fours)) 4) [0..fours]



constructGameTreeFromArmies :: [Unit] -> [Unit] -> [Unit] -> [Unit] -> GameTree
constructGameTreeFromArmies [] _ [] _ = Draw (1%1)
constructGameTreeFromArmies [] _ defendingArmies _ = Loss defendingArmies (1%1)
constructGameTreeFromArmies attackingArmies _ [] _ = Win attackingArmies (1%1)
constructGameTreeFromArmies attackingArmies attackingLossProfile defendingArmies defendingLossProfile = (Inconclusive attackingArmies defendingArmies (1 % 1) children)
  where
    children = []

  {--
battleRound :: [Unit] -> [Unit] -> [Unit] -> [Unit] -> [(BattleOutcome, Int, Rational, [Unit], [Unit])]
battleRound = undefined

battleOutcomeFromArmies :: [Unit] -> [Unit] -> BattleOutcome
battleOutcomeFromArmies [] [] = Draw
battleOutcomeFromArmies [] _ = Loss
battleOutcomeFromArmies _ [] = Win
battleOutcomeFromArmies _ _ = Inconclusive
--}

countIf :: (a -> Bool) -> [a] -> Int
countIf predicate = length . (filter predicate)

count :: Eq a => a -> [a] -> Int
count el = countIf (== el)
