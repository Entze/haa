module AxisAndAlliesLibrary where

import Data.List

data Unit = Infantry | Artillery | Tank | AntiAirArtillery | Fighter | Bomber | Submarine | Destroyer | Cruiser | AircraftCarrier | Battleship deriving (Show, Eq, Ord, Enum, Read)

newtype FightUnit = FiUn (Int, Unit) deriving (Show, Eq)

type Army = [FightUnit]

data Battle = AttackerDefender Army Army

instance Ord FightUnit where
  compare (FiUn (a0,a1)) (FiUn (b0, b1))
    | cmp == EQ = compare a0 b0
    | otherwise = cmp
    where
      cmp = compare a1 b1


baseAttackValue :: Unit -> Int
baseAttackValue Infantry = 1
baseAttackValue Artillery = 2
baseAttackValue Tank = 3
baseAttackValue AntiAirArtillery = 0
baseAttackValue Fighter = 3
baseAttackValue Bomber = 4
baseAttackValue Submarine = 2
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
baseHitpoints Destroyer = 1
baseHitpoints Cruiser = 1
baseHitpoints AircraftCarrier = 1
baseHitpoints Battleship = 2

compressArmy :: Army -> Army
compressArmy ((FiUn (a1, u1)):(FiUn (a2, u2)):ls)
  | u1 == u2 = compressArmy ((FiUn (a1 + a2, u1)):ls)
  | otherwise = (FiUn (a1, u1)):(compressArmy ((FiUn (a2, u2)):ls))
compressArmy [x] = [x]
compressArmy [] = []

reduceArmy :: Army -> Army
reduceArmy = compressArmy . sort
