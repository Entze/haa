module AxisAndAlliesLibrary where

import Data.List

data Unit = Infantry | Artillery | Tank | AntiAirArtillery | Fighter | Bomber | Submarine | Transport | Destroyer | Cruiser | AircraftCarrier | Battleship | DamagedBattleship deriving (Show, Eq, Ord, Enum, Read, Bounded)

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
generalCombatAppliedLosses army _ 0 = army
generalCombatAppliedLosses [] _ _ = []
generalCombatAppliedLosses army lossProfile@(next:lp) n
  | army `elem` next = generalCombatAppliedLosses (delete next army) lossProfile (n-1)
  | otherwise = generalCombatAppliedLosses army lp n

airDefenseValues :: [Unit] -> [Unit] -> [Int]
airDefenseValues attackingArmy defendingArmy = replicate nrOfShots (defenseValue AntiAirArtillery)
  where
    nrOfShots = min (nrOfAAA * 3) nrOfAirUnits
    nrOfAAA = count AntiAirArtillery defendingArmy
    nrOfAirUnits = countIf (`elem` airUnits) attackingArmy

airDefenseAppliedLosses :: [Unit] -> [Unit] -> Int -> [Int]
airDefenseAppliedLosses army lossProfile n = generalCombatAppliedLosses army (lossProfile \\ airUnits) n

offshoreBombardmentValues :: [Unit] -> [Int]
offshoreBombardmentValues = (map attackValue) . sort

offshoreBombardmentAppliedLosses :: [Unit] -> [Unit] -> Int -> [Unit]
offshoreBombardmentAppliedLosses = generalCombatAppliedLosses

countIf :: (a -> Bool) -> [a] -> Int
countIf predicate = length . (filter predicate)

count :: Eq a => a -> [a] -> Int
count el = countIf (== el)
