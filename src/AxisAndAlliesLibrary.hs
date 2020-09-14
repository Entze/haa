module AxisAndAlliesLibrary where

import Data.List
import Data.Ratio
import Data.Tuple.Extra

data Unit = Infantry | Artillery | Tank | AntiAirArtillery | Fighter | Bomber | Submarine | Transport | Destroyer | Cruiser | AircraftCarrier | Battleship | DamagedBattleship deriving (Show, Eq, Ord, Enum, Read, Bounded)

-- Type (SubmarineStrikes = Strikes that cannot hit AirUnits) (AirUnitStrikes = Strikes that cannot hit Submarines) (GeneralStrikes)
data RoundType = AirDefense | Offshorebombardment | SupriseStrikeAttacker | SupriseStrikeDefender Int | SubmarineAttacker | AirUnitsAttacker Int | GeneralCombatAttacker Int Int Int | SubmarineDefender Int Int Int | AirUnitsDefender Int Int Int | GeneralCombatDefender Int Int Int deriving (Show, Eq)

data Outcome = Attacker | Defender | Draw | Inconclusive deriving (Show, Eq, Enum, Bounded)

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
attacks Fighter = Submarine `delete` (landUnits ++ airUnits ++ seaUnits)
attacks Bomber = Submarine  `delete` (landUnits ++ airUnits ++ seaUnits)
attacks Submarine = seaUnits
attacks Transport = []
attacks Destroyer = airUnits ++ seaUnits
attacks Cruiser = landUnits ++ airUnits ++ seaUnits
attacks AircraftCarrier = airUnits ++ seaUnits
attacks Battleship = landUnits ++ airUnits ++ seaUnits
attacks DamagedBattleship = attacks Battleship

armyCanAttack :: [Unit] -> [Unit]
armyCanAttack army = ((nub . concat . (map attacks) . nub) army) ++ additional
  where
    additional = ((map changeTo) . nub . (filter (== Destroyer))) army
    changeTo Destroyer = Submarine
    changeTo x = x

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
lowestIPCFirstSeaUnits = (reverse .  nub . reverse) ((sortOn unitCost seaUnits) ++ [Transport])
lowestIPCFirstSeaUnitsOpt = nub (Battleship:lowestIPCFirstSeaUnits)
lowestIPCFirst = (reverse . nub . reverse) ((sortOn unitCost (lowestIPCFirstLandUnits ++ lowestIPCFirstAirUnits ++ lowestIPCFirstSeaUnits)) ++ [Transport])
lowestIPCFirstOpt = nub (Battleship:lowestIPCFirst)


generalCombatAttackValues :: [Unit] -> [Int]
generalCombatAttackValues = generalCombatAttackValues' . sort

generalCombatAttackValues' :: [Unit] -> [Int]
generalCombatAttackValues' (Infantry:army)
  | elem Artillery army = 2:2:(generalCombatAttackValues' (delete Artillery army))
  | otherwise = 1:(generalCombatAttackValues' army)
generalCombatAttackValues' (unit:army) = (attackValue unit):(generalCombatAttackValues' army)
generalCombatAttackValues' [] = []

generalCombatDefenseValues :: [Unit] -> [Int]
generalCombatDefenseValues = map defenseValue

generalCombatAppliedLosses :: [Unit] -> [Unit] -> Int -> [Unit]
generalCombatAppliedLosses army _ hits
  | hits <= 0 = army
generalCombatAppliedLosses [] _ _ = []
generalCombatAppliedLosses army lossProfile hits = generalCombatAppliedLosses' army lossProfile hits

generalCombatAppliedLosses' :: [Unit] -> [Unit] -> Int -> [Unit]
generalCombatAppliedLosses' army _ 0 = army
generalCombatAppliedLosses' army [] _ = army
generalCombatAppliedLosses' [] _ _ = []
generalCombatAppliedLosses' army lossProfile@(next:lp) hits
  | next `elem` army = generalCombatAppliedLosses' ((delete next army) ++ onDestruction next) lossProfile (hits-1)
  | otherwise = generalCombatAppliedLosses' army lp hits


airBattleAppliedLosses :: Bool -> [Unit] -> [Unit] -> Int -> [Unit]
airBattleAppliedLosses withDestroyer army lossProfile hits = generalCombatAppliedLosses army lp hits
  where
    lp = if withDestroyer then lossProfile else (Submarine `delete` lossProfile)


airDefenseValues :: [Unit] -> [Unit] -> [Int]
airDefenseValues attackingArmy defendingArmy = replicate nrOfShots (defenseValue AntiAirArtillery)
  where
    nrOfShots = min (nrOfAAA * 3) nrOfAirUnits
    nrOfAAA = count AntiAirArtillery defendingArmy
    nrOfAirUnits = countIf (`elem` airUnits) attackingArmy

airDefenseAppliedLosses :: [Unit] -> [Unit] -> Int -> [Unit]
airDefenseAppliedLosses army lossProfile hits = generalCombatAppliedLosses army (lossProfile `intersect` airUnits) hits

offshoreBombardmentValues :: [Unit] -> [Int]
offshoreBombardmentValues attackingArmy = replicate nrOfCruisers (attackValue Cruiser) ++ replicate nrOfBattleships (attackValue Battleship)
  where
    nrOfCruisers = count Cruiser attackingArmy
    nrOfBattleships = count Battleship attackingArmy


submarineBattleAttackValues :: [Unit] -> [Int]
submarineBattleAttackValues = generalCombatAttackValues . (filter (== Submarine))

submarineBattleDefenseValues :: [Unit] -> [Int]
submarineBattleDefenseValues = generalCombatDefenseValues . (filter (== Submarine))

submarineBattleAppliedLosses :: [Unit] -> [Unit] -> Int -> [Unit]
submarineBattleAppliedLosses army lossProfile hits = generalCombatAppliedLosses army (lossProfile `intersect` (attacks Submarine)) hits



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
    singleProbabilities = sortOn fst (probabilityOfHitsWithIndexed ones twos threes fours)
    reducedProbabilities = reduce' singleProbabilities
    reduce' :: [(Int, Rational)] -> [(Int,Rational)]
    reduce' ((a,r1):(b,r2):rs)
      | a == b = reduce' ((a, r1 + r2):rs)
      | otherwise = (a,r1):(reduce' ((b,r2):rs))
    reduce' x = x


probabilityOfHitsIndexed :: [Int] -> [(Int, Rational)]
probabilityOfHitsIndexed values = probabilityOfHitsWithIndexed ones twos threes fours
  where
    ones = count 1 values
    twos = count 2 values
    threes = count 3 values
    fours = count 4 values

probabilityOfHitsWithIndexed :: Int -> Int -> Int -> Int -> [(Int, Rational)]
probabilityOfHitsWithIndexed ones twos threes fours = (reduce'' . reduce') (probabilityOfHitsWithSingleIndexed ones twos threes fours)
  where
    reduce'' :: [(Int, Rational)] -> [(Int, Rational)]
    reduce'' (a@(n1, p1):b@(n2, p2):ls)
      | n1 == n2 = reduce'' ((n1, p1 + p2):ls)
      | otherwise = a:(reduce'' (b:ls))
    reduce'' x = x
    reduce' :: [(Int, [Rational])] -> [(Int, Rational)]
    reduce' ((n, p):ls) = (n, product p):(reduce' ls)
    reduce' [] = []

probabilityOfHitsWithSingleIndexed :: Int -> Int -> Int -> Int -> [(Int, [Rational])]
probabilityOfHitsWithSingleIndexed ones twos threes fours = singleProbabilities
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

airDefense :: [Unit] -> (RoundType, Int, Rational, Rational, [Unit], [Unit]) -> [(RoundType, Int, Rational, Rational, [Unit], [Unit])]
airDefense attackingLossProfile (AirDefense, depth, _, continuedProbability, attackingArmies, defendingArmies) = outcomes
  where
    outcomes :: [(RoundType, Int, Rational, Rational, [Unit], [Unit])]
    outcomes = map (\(hits, prob) -> (GeneralCombatAttacker 0 0 0, depth + 1, prob, continuedProbability * prob, airDefenseAppliedLosses attackingArmies attackingLossProfile hits, defendingArmies)) probabilitiesOfHits
    probabilitiesOfHits = sortOn ((1 -) . snd) (probabilityOfHitsIndexed strength)
    strength = airDefenseValues attackingArmies defendingArmies
airDefense _ _ = []

offshoreBombardment :: (RoundType, Int, Rational, Rational, [Unit], [Unit]) -> [(RoundType, Int, Rational, Rational, [Unit], [Unit])]
offshoreBombardment (Offshorebombardment, depth, _, continuedProbability, attackingArmies, defendingArmies) = outcomes
  where
    outcomes = map (\(hits, prob) -> (GeneralCombatAttacker 0 0 hits, depth + 1, prob, continuedProbability * prob, ((filter (/= Battleship)) . (filter (/= Cruiser))) attackingArmies, defendingArmies)) probabilitiesOfHits
    probabilitiesOfHits = sortOn ((1 -) . snd) (probabilityOfHitsIndexed strength)
    strength = offshoreBombardmentValues attackingArmies
offshoreBombardment _ = []

supriseStrike :: [Unit] -> [Unit] -> (RoundType, Int, Rational, Rational, [Unit], [Unit]) -> [(RoundType, Int, Rational, Rational, [Unit], [Unit])]
supriseStrike attackingLossProfile defendingLossProfile (SupriseStrikeAttacker, depth, _, continuedProbability, attackingArmies, defendingArmies)
  | Destroyer `notElem` attackingArmies && Submarine `elem` defendingArmies = map (\(hits, prob) -> (SupriseStrikeDefender hits, depth + 1, prob, continuedProbability * prob, attackingArmies, defendingArmies)) probabilitiesOfHits
  | any (`elem` attackingArmies) airUnits = map (\(hits, prob) -> (AirUnitsAttacker 0, depth + 1, prob, continuedProbability * prob, attackingArmies, submarineBattleAppliedLosses defendingArmies defendingLossProfile hits)) probabilitiesOfHits
  | otherwise = map (\(hits, prob) -> (GeneralCombatAttacker 0 0 0, depth + 1, prob, continuedProbability * prob, attackingArmies, submarineBattleAppliedLosses defendingArmies defendingLossProfile hits)) probabilitiesOfHits
  where
    probabilitiesOfHits = sortOn ((1 -) . snd) (probabilityOfHitsIndexed strength)
    strength = submarineBattleAttackValues attackingArmies
supriseStrike attackingLossProfile defendingLossProfile (SupriseStrikeDefender attackHits, depth, _, continuedProbability, attackingArmies, defendingArmies)
  | Submarine `elem` attackingArmies && Destroyer `elem` defendingArmies = map (\(hits, prob) -> (SubmarineAttacker, depth + 1, prob, continuedProbability * prob, submarineBattleAppliedLosses attackingLossProfile attackingArmies hits, defendingArmies)) probabilitiesOfHits
  | any (`elem` attackingArmies) airUnits = map (\(hits, prob) -> (AirUnitsAttacker 0, depth + 1, prob, continuedProbability * prob, submarineBattleAppliedLosses attackingArmies attackingLossProfile hits, submarineBattleAppliedLosses defendingLossProfile defendingArmies attackHits)) probabilitiesOfHits
  | otherwise = map (\(hits, prob) -> (GeneralCombatAttacker 0 0 0, depth + 1, prob, continuedProbability * prob, submarineBattleAppliedLosses attackingArmies attackingLossProfile hits, submarineBattleAppliedLosses defendingArmies defendingLossProfile attackHits)) probabilitiesOfHits
  where
    probabilitiesOfHits = sortOn ((1 -) . snd) (probabilityOfHitsIndexed strength)
    strength = submarineBattleDefenseValues defendingArmies
supriseStrike _ _ _ = []

submarineCombat :: [Unit] -> (RoundType, Int, Rational, Rational, [Unit], [Unit]) -> [(RoundType, Int, Rational, Rational, [Unit], [Unit])]
submarineCombat _ (SubmarineAttacker, depth, _, continuedProbability, attackingArmies, defendingArmies)
  | any (`elem` attackingArmies) airUnits = map (\(hits, prob) -> (AirUnitsAttacker hits, depth + 1, prob, continuedProbability * prob, attackingArmies, defendingArmies)) probabilitiesOfHits
  | otherwise = map (\(hits, prob) -> (GeneralCombatAttacker hits 0 0, depth + 1, prob, continuedProbability * prob, attackingArmies, defendingArmies)) probabilitiesOfHits
  where
    probabilitiesOfHits = sortOn ((1 -) . snd) (probabilityOfHitsIndexed strength)
    strength = submarineBattleAttackValues attackingArmies
submarineCombat attackerLossProfile (SubmarineDefender subHits airHits genHits, depth, _, continuedProbability, attackingArmies, defendingArmies)
  | any (`elem` defendingArmies) airUnits = map (\(hits, prob) -> (AirUnitsDefender subHits airHits genHits, depth + 1, prob, continuedProbability * prob, submarineBattleAppliedLosses attackingArmies attackerLossProfile hits, defendingArmies)) probabilitiesOfHits
  | otherwise = map (\(hits, prob) -> (GeneralCombatAttacker subHits airHits genHits, depth + 1, prob, continuedProbability * prob, submarineBattleAppliedLosses attackingArmies attackerLossProfile hits, defendingArmies)) probabilitiesOfHits
  where
    probabilitiesOfHits = sortOn ((1 -) . snd) (probabilityOfHitsIndexed strength)
    strength = submarineBattleAttackValues attackingArmies
submarineCombat _ _ = []


airUnitsCombat :: [Unit] -> (RoundType, Int, Rational, Rational, [Unit], [Unit]) -> [(RoundType, Int, Rational, Rational, [Unit], [Unit])]
airUnitsCombat _ (AirUnitsAttacker subHits, depth, _, continuedProbability, attackingArmies, defendingArmies)
  | any (`elem` attackingArmies) (Submarine `delete` seaUnits) || any (`elem` attackingArmies) landUnits = map (\(hits, prob) -> (GeneralCombatAttacker subHits (if withDestroyer then hits else 0) (if (not withDestroyer) then hits else 0), depth + 1, prob, continuedProbability * prob, attackingArmies, defendingArmies)) probabilitiesOfHits
  | Submarine `elem` defendingArmies && Destroyer `elem` attackingArmies = map (\(hits, prob) -> (SubmarineDefender subHits (if withDestroyer then hits else 0) (if (not withDestroyer) then hits else 0), depth + 1, prob, continuedProbability * prob, attackingArmies, defendingArmies)) probabilitiesOfHits
  | any (`elem` defendingArmies) airUnits = map (\(hits, prob) -> (AirUnitsDefender subHits (if withDestroyer then hits else 0) (if (not withDestroyer) then hits else 0), depth + 1, prob, continuedProbability * prob, attackingArmies, defendingArmies)) probabilitiesOfHits
  | otherwise = map (\(hits, prob) -> (GeneralCombatDefender subHits (if withDestroyer then hits else 0) (if (not withDestroyer) then hits else 0), depth + 1, prob, continuedProbability * prob, attackingArmies, defendingArmies)) probabilitiesOfHits
  where
    probabilitiesOfHits = sortOn ((1 -) . snd) (probabilityOfHitsIndexed strength)
    strength = generalCombatAttackValues attackingArmies
    withDestroyer = Destroyer `elem` attackingArmies
airUnitsCombat attackerLossProfile (AirUnitsDefender subHits airHits generalHits, depth, _, continuedProbability, attackingArmies, defendingArmies) = outcomes
  where
    outcomes = map (\(hits, prob) -> (GeneralCombatDefender subHits airHits generalHits, depth+1, prob, continuedProbability * prob, airBattleAppliedLosses withDestroyer attackingArmies attackerLossProfile hits, defendingArmies)) probabilitiesOfHits
    probabilitiesOfHits = sortOn ((1 -) . snd) (probabilityOfHitsIndexed strength)
    strength = generalCombatDefenseValues defendingArmies
    withDestroyer = Destroyer `elem` defendingArmies
airUnitsCombat _ _ = []

generalCombat :: [Unit] -> [Unit] -> (RoundType, Int, Rational, Rational, [Unit], [Unit]) -> [(RoundType, Int, Rational, Rational, [Unit], [Unit])]
generalCombat _ _ (GeneralCombatAttacker subHits airHits generalHits, depth, _, continuedProbability, attackingArmies, defendingArmies)
  | Submarine `elem` defendingArmies && Destroyer `elem` attackingArmies = map (\(hits, prob) -> (SubmarineDefender subHits airHits (generalHits + hits), depth + 1, prob, continuedProbability * prob, attackingArmies, defendingArmies)) probabilitiesOfHits
  | any (`elem` defendingArmies) airUnits = map (\(hits, prob) -> (AirUnitsDefender subHits airHits (generalHits + hits), depth + 1, prob, continuedProbability * prob, attackingArmies, defendingArmies)) probabilitiesOfHits
  | otherwise = map (\(hits, prob) -> (GeneralCombatDefender subHits airHits (generalHits + hits), depth + 1, prob, continuedProbability * prob, attackingArmies, defendingArmies)) probabilitiesOfHits
  where
    probabilitiesOfHits = sortOn ((1 -) . snd) (probabilityOfHitsIndexed strength)
    strength = generalCombatAttackValues (((filter (`notElem` airUnits)) . (filter (/= Submarine))) attackingArmies)
generalCombat attackingLossProfile defendingLossProfile (GeneralCombatDefender subHits airHits generalHits, depth, _, continuedProbability, attackingArmies, defendingArmies) = outcomes
  where
    remapRoundType (_, depth, p, cp, a, d)
      | Submarine `elem` a && Destroyer `notElem` d = (SupriseStrikeAttacker, depth, p, cp, a, d)
      | Submarine `elem` d && Destroyer `notElem` a = (SupriseStrikeDefender 0, depth, p, cp, a, d)
      | Submarine `elem` a = (SubmarineAttacker, depth, p, cp, a, d)
      | any (`elem` a) airUnits = (AirUnitsAttacker 0, depth, p, cp, a, d)
      | otherwise = (GeneralCombatAttacker 0 0 0, depth, p, cp, a, d)
    outcomes = map (remapRoundType . (\(hits, prob) -> (SupriseStrikeAttacker, depth + 1, prob, continuedProbability * prob, generalCombatAppliedLosses attackingArmies attackingLossProfile hits, afterDefense))) probabilitiesOfHits
    afterAttacks = map ((generalCombatAppliedLosses attackingArmies attackingLossProfile) . fst) probabilitiesOfHits
    afterDefense = generalCombatAppliedLosses afterAir defendingLossProfile generalHits
    afterAir = airBattleAppliedLosses False afterSubmarine defendingLossProfile airHits
    afterSubmarine = submarineBattleAppliedLosses defendingArmies defendingLossProfile subHits
    probabilitiesOfHits = sortOn ((1 -) . snd) (probabilityOfHitsIndexed strength)
    strength = generalCombatDefenseValues (((filter (`notElem` airUnits)) . (filter (/= Submarine))) defendingArmies)
generalCombat _ _ _ = []


constructGameTreeFromArmies :: [Unit] -> [Unit] -> [Unit] -> [Unit] -> [(RoundType, Int, Rational, Rational, [Unit], [Unit])]
constructGameTreeFromArmies attackingArmies attackingLossProfile defendingArmies defendingLossProfile
  | any (`elem` attackingArmies) airUnits && AntiAirArtillery `elem` defendingArmies = constructGameTreeFromArmies' [(AirDefense, 0, 1 % 1, 1 % 1, attackingArmies, defendingArmies)]
  | all (`notElem` defendingArmies) seaUnits && (Cruiser `elem` attackingArmies || Battleship `elem` attackingArmies) = constructGameTreeFromArmies' [(Offshorebombardment, 0, 1 % 1, 1 % 1, attackingArmies, defendingArmies)]
  | Submarine `elem` attackingArmies && Destroyer `notElem` defendingArmies = constructGameTreeFromArmies' [(SupriseStrikeAttacker, 0, 1 % 1, 1 % 1, attackingArmies, defendingArmies)]
  | Submarine `elem` defendingArmies && Destroyer `notElem` attackingArmies = constructGameTreeFromArmies' [(SupriseStrikeDefender 0, 0, 1 % 1, 1 % 1, attackingArmies, defendingArmies)]
  | Submarine `elem` attackingArmies && Destroyer `elem` defendingArmies = constructGameTreeFromArmies' [(SubmarineAttacker, 0, 1 % 1, 1 % 1, attackingArmies, defendingArmies)]
  | any (`elem` attackingArmies) airUnits = constructGameTreeFromArmies' [(AirUnitsAttacker 0, 0, 1 % 1, 1 % 1, attackingArmies, defendingArmies)]
  | otherwise = constructGameTreeFromArmies' [(GeneralCombatAttacker 0 0 0, 0, 1 % 1, 1 % 1, attackingArmies, defendingArmies)]
  where
    constructGameTreeFromArmies' :: [(RoundType, Int, Rational, Rational, [Unit], [Unit])] -> [(RoundType, Int, Rational, Rational, [Unit], [Unit])]
    constructGameTreeFromArmies' (b@(_, _, _, _, [], _):bs) = b:(constructGameTreeFromArmies' bs)
    constructGameTreeFromArmies' (b@(_, _, _, _, _, []):bs) = b:(constructGameTreeFromArmies' bs)
    constructGameTreeFromArmies' (b@(_, depth, p, continuedProbability, a, d):bs)
      | attackerCanAttack && not defenderCanAttack = (GeneralCombatAttacker 0 0 0, depth, p, continuedProbability, a, []):(constructGameTreeFromArmies' bs)
      | not attackerCanAttack && defenderCanAttack = (GeneralCombatAttacker 0 0 0, depth, p, continuedProbability, [], d):(constructGameTreeFromArmies' bs)
      | not attackerCanAttack && not defenderCanAttack = (GeneralCombatAttacker 0 0 0, depth, p, continuedProbability, a, d):(constructGameTreeFromArmies' bs)
        where
          attackerCanAttack = any (`elem` attacksUnits) d
          defenderCanAttack = any (`elem` defendsUnits) a
          attacksUnits = armyCanAttack attackingArmies
          defendsUnits = armyCanAttack defendingArmies
    constructGameTreeFromArmies' (b:bs) = b:(constructGameTreeFromArmies' (bs ++ children))
      where
        children = concat [airDefense attackingLossProfile b, offshoreBombardment b, supriseStrike attackingLossProfile defendingLossProfile b, submarineCombat attackingLossProfile b, airUnitsCombat attackingLossProfile b, generalCombat attackingLossProfile defendingLossProfile b]
    constructGameTreeFromArmies' x = x

outcomeFromArmies :: [Unit] -> [Unit] -> Outcome
outcomeFromArmies [] [] = Draw
outcomeFromArmies [] d = Defender
outcomeFromArmies a [] = Attacker
outcomeFromArmies a d
  | any (`elem` a') d && any (`elem` d') a = Inconclusive
  | otherwise = Draw
  where
    a' = armyCanAttack a
    d' = armyCanAttack d

compressLeftOverUnits :: [([Unit], Rational)] -> [([Unit], Rational)]
compressLeftOverUnits = compressLeftOverUnits' . (sortOn fst) . (map (\(units, rational) -> (sort units, rational)))
  where
    compressLeftOverUnits' :: [([Unit], Rational)] -> [([Unit], Rational)]
    compressLeftOverUnits' (a@(u1, p1):b@(u2, p2):ls)
      | u1 == u2 = compressLeftOverUnits' ((u1, p1 + p2):ls)
      | otherwise = a:(compressLeftOverUnits' (b:ls))
    compressLeftOverUnits' x = x

compressLeftOverUnitsDraw :: [(([Unit], [Unit]), Rational)] -> [(([Unit], [Unit]), Rational)]
compressLeftOverUnitsDraw = compressLeftOverUnits' . (sortOn fst) . (map (\((unitsA, unitsD), rational) -> ((sort unitsA, sort unitsD), rational)))
  where
    compressLeftOverUnits' :: [(([Unit], [Unit]), Rational)] -> [(([Unit], [Unit]), Rational)]
    compressLeftOverUnits' (a@(u1@(u1a, u1b), p1):b@((u2a, u2b), p2):ls)
      | u1a == u2a && u1b == u2b = compressLeftOverUnits' ((u1, p1 + p2):ls)
      | otherwise = a:(compressLeftOverUnits' (b:ls))
    compressLeftOverUnits' x = x


compressInformation :: Information -> Information
compressInformation ((a, pa, ula), (d, pd, uld), (dr, pdr, uldr)) = ((a, pa, compressLeftOverUnits ula), (d, pd, compressLeftOverUnits uld), (dr, pdr, compressLeftOverUnitsDraw uldr))


type Information = ((Outcome, Rational, [([Unit], Rational)]), (Outcome, Rational, [([Unit], Rational)]), (Outcome, Rational, [(([Unit], [Unit]), Rational)]))

nullInformation :: Information
nullInformation = ((Attacker, 0 % 1, []), (Defender, 0 % 1, []), (Draw, 0 % 1, []))

addInformationFromNode :: Information -> (RoundType, Int, Rational, Rational, [Unit], [Unit]) -> Information
addInformationFromNode (attacker@(Attacker, overallProbabilityAttacker, unitsLeftAttacker), defender@(Defender, overallProbabilityDefender, unitsLeftDefender), draw@(Draw, overallProbabilityDraw, unitsLeftDraw)) (_, _, _, p, a, d)
  | outcome == Attacker = ((Attacker, overallProbabilityAttacker + p, ((a,p):unitsLeftAttacker)), defender, draw)
  | outcome == Defender = (attacker, (Defender, overallProbabilityDefender + p, ((d,p):unitsLeftDefender)), draw)
  | outcome == Draw = (attacker, defender, (Draw, overallProbabilityDraw + p, ((a,d),p):unitsLeftDraw))
    where
      outcome = outcomeFromArmies a d
addInformationFromNode x _ = x

extractInformation :: [(RoundType, Int, Rational, Rational, [Unit], [Unit])] -> [Information]
extractInformation ns = extractInformation' nullInformation ns
  where
    extractInformation' info (n:ns) = info':(extractInformation' (addInformationFromNode info' n) ns)
      where
        info' = compressInformation info


noLeftover :: Information -> ((Outcome, Rational), (Outcome, Rational), (Outcome, Rational), (Outcome, Rational))
noLeftover ((a, pa, _), (d, pd, _), (dr, pdr, _)) = ((a, pa), (d, pd), (dr, pdr), (Inconclusive, inconclusiveP))
  where
    inconclusiveP = 1 - (pa + pd + pdr)


convertToData :: Information -> [(Outcome, Rational)]
convertToData ((a, pa, _), (d, pd, _), (dr, pdr, _)) = zip [a, d, dr, Inconclusive] [pa, pd, pdr, 1-(pa +pd + pdr)]

convertToDataLeftoverAttacker :: Information -> [([Unit], Rational)]
convertToDataLeftoverAttacker ((_, _, a), _, _) = a

convertToDataLeftoverDefender :: Information -> [([Unit], Rational)]
convertToDataLeftoverDefender (_, (_, _, d), _) = d

convertToDataLeftoverDraw :: Information -> [(([Unit], [Unit]), Rational)]
convertToDataLeftoverDraw (_, _, (_, _, d)) = d

convertToPercentage :: Fractional b => [(a, Rational)] -> [(a, b)]
convertToPercentage i = zip a (map fromRational r)
  where
    (a, r) = unzip i


takeUntilInconclusiveIsSmallerThan :: (Ord a, Fractional a) => a -> [Information] -> [Information]
takeUntilInconclusiveIsSmallerThan eps = takeWhile (((>= eps) . fromRational . (!! 3) . snd . unzip . convertToData))


countIf :: (a -> Bool) -> [a] -> Int
countIf predicate = length . (filter predicate)

count :: Eq a => a -> [a] -> Int
count el = countIf (== el)

deleteAll :: Eq a => [a] -> a -> [a]
deleteAll (a:as) el
  | a == el = deleteAll as el
  | otherwise = a:(deleteAll as el)
deleteAll as _ = as
