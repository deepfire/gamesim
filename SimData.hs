{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module SimData where


-- * Atomic types

data IPlayer
  =  First
  |  Second
  |  Third
  |  Fourth
  deriving (Enum, Eq, Ord, Show)

data IResource
  =  Water
  |  Drugs
  |  Gas
  |  Ammo
  |  Scrap
  |  Services
  deriving (Enum, Eq, Ord, Show)

data IGang
  =  Valkiry
  |  Gents
  |  Patrol
  |  Slavers
  |  Gunners
  |  Chemists
  deriving (Enum, Eq, Ord, Show)

data IMerc
  =  Grasper
  |  Guitarist
  |  Madcap
  |  Prothrall
  |  Bonecrusher
  |  Preacher
  |  Drummer
  |  Bounder
  |  Barkeeper
  |  BlackWidow
  |  Striker
  |  Dealess
  deriving (Enum, Eq, Ord, Show)

data family IGangman (igang ∷ IGang) ∷ *

data instance IGangman Valkiry
  =  ValkLead
  |  Valk1
  |  Valk2
  |  Valk3
  deriving (Enum, Eq, Ord, Show)

data instance IGangman Gents
  =  GentLead
  |  Gent1
  |  Gent2
  |  Gent3
  deriving (Enum, Eq, Ord, Show)

data instance IGangman Patrol
  =  PatrolLead
  |  Patrol1
  |  Patrol2
  |  Patrol3
  deriving (Enum, Eq, Ord, Show)

data instance IGangman Slavers
  =  SlaverLead
  |  Slaver1
  |  Slaver2
  |  Slaver3
  deriving (Enum, Eq, Ord, Show)

data instance IGangman Gunners
  =  GunnerLead
  |  Gunner1
  |  Gunner2
  |  Gunner3
  deriving (Enum, Eq, Ord, Show)

data instance IGangman Chemists
  =  ChemistLead
  |  Chemist1
  |  Chemist2
  |  Chemist3
  deriving (Enum, Eq, Ord, Show)

data IHench
  =  Gangman
  |  Merc
  |  Fanatic
  |  Slave
  deriving (Eq, Ord, Show)

data ITerritory
  =  ForbiddenRiver
  |  CitadelOfFlayedOne
  |  ChurchRevelationOfTheProphet
  |  ClinicEternalLife
  |  OilPumpOfHottyFred
  |  RainbowLake
  |  BulletFarm
  |  ArmoryOfMagniKrugger
  |  IronTown
  |  SmelteryOfHragBakster
  |  ShiningBurialGrounds
  |  FactoryMechanicalMessiah
  |  BarTheLastDrop
  |  BrothelKittiesOfTheMaster
  |  FightingPitBloodAndConcrete
  |  CasinoStElmosFires
  deriving (Eq, Ord, Show)

newtype Attack       = Attack       Int           deriving (Eq, Num, Show)
newtype Defence      = Defence      Int           deriving (Eq, Num, Show)
newtype Swag         = Swag         Int           deriving (Eq, Num, Show)
newtype AtSizes      = AtSizes      [Int]         deriving (         Show)
newtype EnablesMercs = EnablesMercs [IMerc]       deriving (         Show)
newtype TerrsInPlay  = TerrsInPlay  [ITerritory]  deriving (         Show)
newtype TerrDeck     = TerrDeck     [ITerritory]  deriving (         Show)
newtype MercsInPlay  = MercsInPlay  [IMerc]       deriving (         Show)
newtype MercDeck     = MercDeck     [IMerc]       deriving (         Show)
newtype Sitting      = Sitting      [IGang]       deriving (         Show)


-- * Game data

game_henchmen
  = [ (Gangman,   (Attack 1,  Defence 1))
    , (Merc,      (Attack 1,  Defence 1))
    , (Fanatic,   (Attack 2,  Defence 0))
    , (Slave,     (Attack 0,  Defence 1)) ]

game_territories
  = [ (ForbiddenRiver,                 (Water,     Swag 5,   AtSizes [2, 3, 4]))
    , (CitadelOfFlayedOne,             (Water,     Swag 5,   AtSizes [   3, 4]))
    , (ChurchRevelationOfTheProphet,   (Drugs,     Swag 5,   AtSizes [2,    4]))
    , (ClinicEternalLife,              (Drugs,     Swag 5,   AtSizes [      4]))
    , (OilPumpOfHottyFred,             (Gas,       Swag 5,   AtSizes [2, 3, 4]))
    , (RainbowLake,                    (Gas,       Swag 5,   AtSizes [   3, 4]))
    , (BulletFarm,                     (Ammo,      Swag 5,   AtSizes [2, 3, 4]))
    , (ArmoryOfMagniKrugger,           (Ammo,      Swag 5,   AtSizes [   3, 4]))
    , (IronTown,                       (Scrap,     Swag 4,   AtSizes [2, 3, 4]))
    , (SmelteryOfHragBakster,          (Scrap,     Swag 4,   AtSizes [2, 3, 4]))
    , (ShiningBurialGrounds,           (Scrap,     Swag 4,   AtSizes [   3, 4]))
    , (FactoryMechanicalMessiah,       (Scrap,     Swag 4,   AtSizes [      4]))
    , (BarTheLastDrop,                 (Services,  Swag 4,   AtSizes [2, 3, 4]))
    , (BrothelKittiesOfTheMaster,      (Services,  Swag 4,   AtSizes [2, 3, 4]))
    , (FightingPitBloodAndConcrete,    (Services,  Swag 4,   AtSizes [   3, 4]))
    , (CasinoStElmosFires,             (Services,  Swag 4,   AtSizes [      4])) ]
