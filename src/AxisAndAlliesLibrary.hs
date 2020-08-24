module AxisAndAlliesLibrary where

import Data.List

data Unit = Infantry | Artillery | Tank | AntiAirArtillery | Fighter | Bomber | Submarine | Transport | Destroyer | Cruiser | AircraftCarrier | Battleship | DamagedBattleship deriving (Show, Eq, Ord, Enum, Read)

baseAttackValue :: Unit -> Int
baseAttackValue Infantry = 1
baseAttackValue Artillery = 2
baseAttackValue Tank = 3
baseAttackValue AntiAirArtillery = 0
baseAttackValue Fighter = 3
baseAttackValue Bomber = 4
baseAttackValue Submarine = 2
baseAttackValue Transport = 0
baseAttackValue Destroyer = 2
baseAttackValue Cruiser = 3
baseAttackValue AircraftCarrier = 1
baseAttackValue Battleship = 4

baseDefenseValue :: Unit -> Int
baseDefenseValue Infantry = 2
baseDefenseValue Artillery = 2
baseDefenseValue Tank = 2
baseDefenseValue AntiAirArtillery = 1
baseDefenseValue Fighter = 4
baseDefenseValue Bomber = 1
baseDefenseValue Submarine = 1
baseDefenseValue Transport = 0
baseDefenseValue Destroyer = 2
baseDefenseValue Cruiser = 3
baseDefenseValue AircraftCarrier = 2
baseDefenseValue Battleship = 4

baseHitpoints :: Unit -> Int
baseHitpoints Infantry = 1
baseHitpoints Artillery = 1
baseHitpoints Tank = 1
baseHitpoints AntiAirArtillery = 1
baseHitpoints Fighter = 1
baseHitpoints Bomber = 1
baseHitpoints Submarine = 1
baseHitpoints Transport = 1
baseHitpoints Destroyer = 1
baseHitpoints Cruiser = 1
baseHitpoints AircraftCarrier = 1
baseHitpoints Battleship = 2

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

compressArmy :: Army -> Army
compressArmy ((FiUn (a1, u1)):(FiUn (a2, u2)):ls)
  | a1 <= 0 = compressArmy ((FiUn (a2, u2)):ls)
  | u1 == u2 = compressArmy ((FiUn (a1 + a2, u1)):ls)
  | otherwise = (FiUn (a1, u1)):(compressArmy ((FiUn (a2, u2)):ls))
compressArmy [x] = [x]
compressArmy [] = []

reduceArmy :: Army -> Army
reduceArmy = compressArmy . sort

addToArmy :: FightUnit -> Army -> Army
addToArmy fu a = reduceArmy (fu:a)

mergeArmies :: Army -> Army -> Army
mergeArmies army0 army1 = reduceArmy (army0 ++ army1)

removeFromArmy :: FightUnit -> Army -> Army
removeFromArmy fu army = reduceArmy (removeFromArmy' fu army)
  where
    removeFromArmy' _ [] = []
    removeFromArmy' (FiUn (0, _)) army = army
    removeFromArmy' (FiUn (a0, u0)) ((FiUn (a1, u1)):army)
      | u0 == u1 = (FiUn (a1', u1)):(removeFromArmy' (FiUn (a0', u0)) army)
      | otherwise = (FiUn (a1, u1)):(removeFromArmy' (FiUn (a0, u0)) army)
      where
        removed = min a0 a1
        a0' = max 0 (a0 - removed)
        a1' = max 0 (a1 - removed)


lossesOfArmy :: [FightUnit] -> Army -> Army
lossesOfArmy [] army = army
lossesOfArmy _ [] = []
lossesOfArmy losses army = foldl (flip removeFromArmy) army losses

nrOfUnits :: Unit -> Army -> Int
nrOfUnits _ [] = 0
nrOfUnits unit ((FiUn (a, u)):army)
  | unit == u = a + (nrOfUnits unit army)
  | otherwise = nrOfUnits unit army

--TODO
fightUnitToAttackValue :: FightUnit -> FightValue
fightUnitToAttackValue (FiUn (a, u)) = FiVa (a, baseAttackValue u)

attackFirePower :: Army -> FirePower
attackFirePower a = firePower
  where
    firePower = map fightUnitToFightValue army'
    army' = removeFromArmy (FiUn (matchedInfantry, Infantry)) (addToArmy (FiUn (matchedInfantry, Artillery)) army)
    matchedInfantry = min nrOfInfantries nrOfArtilleries
    nrOfInfantries = nrOfUnits Infantry army
    nrOfArtilleries = nrOfUnits Artillery army
    army = reduceArmy a

defendFirePower :: Army -> FirePower
defendFirePower a = firePower
  where
    firePower = map fightUnitToFightValue army
    army = reduceArmy a
