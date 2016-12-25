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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module SimRules where

import           Control.Lens.TH
import           Data.List
import           Data.Maybe
import qualified Data.Map.Lazy      as M
import           Data.Tuple.Extra
import           Prelude.Unicode

import Utils
import SimData


-- * Composites

data DGangman where
  DGangman ∷ {
    _dga_ix         ∷ IGangman
  , _dga_deck_count ∷ Int
  , _dga_battlecry  ∷ BattleCry
  , _dga_capture    ∷ Capture
  } → DGangman
deriving instance Show DGangman

data BattleCry
  =  BattleCry [Action ()] | NoBattleCry
  deriving Show

data Capture
  =  Capture [Action ()] | NoCapture
  deriving Show

data IHenchman where
  IMerc     ∷ IMerc → IHenchman
  IGangman  ∷ IGangman → IHenchman
  IFanatic  ∷ IHenchman
  ISlave    ∷ IHenchman
deriving instance Show IHenchman

newtype GangDeck     = GangDeck     [IGangman]    deriving (Show)
newtype PlayerDeck   = PlayerDeck   [IGangman]    deriving (Show)
newtype PlayerBase   = PlayerBase   [Henchman]    deriving (Show)
newtype PlayerHand   = PlayerHand   [IHenchman]   deriving (Show)

game_gangs
  = [ (Valkiry,   (EnablesMercs [Grasper,    Guitarist],
                   [ DGangman ValkLead    1
                      NoBattleCry
                      NoCapture
                   , DGangman Valk1       3
                      NoBattleCry
                      NoCapture
                   , DGangman Valk2       3
                      NoBattleCry
                      NoCapture
                   , DGangman Valk3       3
                      NoBattleCry
                      NoCapture ]))
    , (Gents,     (EnablesMercs [Madcap,   Prothrall],
                   [ DGangman GentLead    1
                      NoBattleCry
                      NoCapture
                   , DGangman Gent1       3
                      NoBattleCry
                      NoCapture
                   , DGangman Gent2       3
                      NoBattleCry
                      NoCapture
                   , DGangman Gent3       3
                      NoBattleCry
                      NoCapture ]))
    , (Patrol,    (EnablesMercs [Bonecrusher,    Preacher],
                   [ DGangman PatrolLead  1
                      NoBattleCry
                      NoCapture
                   , DGangman Patrol1     3
                      NoBattleCry
                      NoCapture
                   , DGangman Patrol2     3
                      NoBattleCry
                      NoCapture
                   , DGangman Patrol3     3
                      NoBattleCry
                      NoCapture ]))
    , (Slavers,   (EnablesMercs [Drummer, Bounder],
                   [ DGangman SlaverLead  1
                      NoBattleCry
                      NoCapture
                   , DGangman Slaver1     3
                      NoBattleCry
                      NoCapture
                   , DGangman Slaver2     3
                      NoBattleCry
                      NoCapture
                   , DGangman Slaver3     3
                      NoBattleCry
                      NoCapture ]))
    , (Gunners,   (EnablesMercs [Barkeeper,     BlackWidow],
                   [ DGangman GunnerLead  1
                      NoBattleCry
                      NoCapture
                   , DGangman Gunner1     3
                      NoBattleCry
                      NoCapture
                   , DGangman Gunner2     3
                      NoBattleCry
                      NoCapture
                   , DGangman Gunner3     3
                      NoBattleCry
                      NoCapture ]))
    , (Chemists,  (EnablesMercs [Striker,  Dealess],
                   [ DGangman ChemistLead 1
                      NoBattleCry
                      NoCapture
                   , DGangman Chemist1    3
                      NoBattleCry
                      NoCapture
                   , DGangman Chemist2    3
                      NoBattleCry
                      NoCapture
                   , DGangman Chemist3    3
                      NoBattleCry
                      NoCapture ])) ]

game_gangmen ∷ M.Map IGangman DGangman
game_gangmen = M.fromList ∘ fmap ((first _dga_ix) ∘ dupe) ∘ concat ∘ fmap (snd ∘ snd) $ game_gangs


-- * Game data queries

ihench_attack    ∷ IHenchmanType → Attack
ihench_attack    = fst ∘ (M.fromList game_henchmen M.!)

ihench_defence   ∷ IHenchmanType → Defence
ihench_defence   = snd ∘ (M.fromList game_henchmen M.!)

iterr_resource   ∷ ITerritory → IResource
iterr_resource   = (\(x, _, _) → x) ∘ (M.fromList game_territories M.!)

iterr_init_swag  ∷ ITerritory → Swag
iterr_init_swag  = (\(_, x, _) → x) ∘ (M.fromList game_territories M.!)

igang_deck       ∷ IGang → GangDeck
igang_deck       = GangDeck ∘ fmap _dga_ix ∘ snd ∘ (M.fromList game_gangs M.!)

igang_enables_mercs ∷ IGang → EnablesMercs
igang_enables_mercs = fst ∘ (M.fromList game_gangs M.!)

gangman_desc ∷ IGangman → DGangman
gangman_desc ix = flip (fromMaybe) (M.lookup ix game_gangmen)
  $ error "Gangman type %s lacks a description!" $ show ix


-- * Active objects

type family Index a where
  Index Player    = IPlayer
  Index Territory = ITerritory
  Index Henchman     = IHenchman
class (Show (Index a)) ⇒ Indexed a where index ∷ a → Index a
instance Indexed Henchman          where index = _he_ix
instance Indexed Territory         where index = _te_ix
instance Indexed Player            where index = _pl_ix

data Henchman
  =  Henchman {
      _he_ix           ∷ IHenchman
    , _he_untapped     ∷ Bool
    }
  deriving (Show)

data Territory
  =  Territory {
      _te_ix           ∷ ITerritory
    , _te_swag         ∷ Swag
    , _te_owner        ∷ Maybe IGang
    , _te_henchmen     ∷ [Henchman]
    }
  deriving (Show)

data Player
  =  Player {
      _pl_ix           ∷ IPlayer
    , _pl_gang         ∷ IGang
    , _pl_deck         ∷ [IGangman]     -- Visibility: noone;    only mercs and gangmen
    , _pl_hand         ∷ [IHenchman]    -- Visibility: player;   only mercs and gangmen
    , _pl_base         ∷ [Henchman]     -- Visibility: everyone; everyone
    , _pl_terrs        ∷ [ITerritory]
    , _pl_swag         ∷ Swag
    }
  deriving (Show)

data Field
  =  Field {
      _fi_terrsinplay  ∷ TerrsInPlay  -- 1 to 4
    , _fi_mercsinplay  ∷ MercsInPlay  -- mercs only
    , _fi_terrdeck     ∷ TerrDeck
    , _fi_mercdeck     ∷ MercDeck     -- mercs only
    }
  deriving (Show)

data GameState
  =  GameState {
      _gs_field        ∷ Field
    , _gs_players      ∷ [Player]
    , _gs_current      ∷ IPlayer
    }
  deriving (Show)

data Squad
  =  Squad {
      _sq_targets      ∷ [(Territory, IHenchman)]
    }
  deriving (Show)


-- * Action

data Action s where
  DumpState ∷
      Action GameState
  EnterPlayers ∷
    { player_gangs        ∷ [IGang] }
    → Action [Player]
  InitialTerrDeck ∷
      Action TerrDeck
  InitialMercDeck ∷
    { merc_multiplier     ∷ Int }
    → Action MercDeck
  MoveMercsIntoPlay ∷
    { merc_count          ∷ Int }
    → Action (MercDeck, MercsInPlay)
  PlayerSitting ∷
    { sitting             ∷ Sitting }
    → Action ()
  InitialDeckHandBase ∷
    { player              ∷ IPlayer
    , base_size           ∷ Int }
    → Action (PlayerDeck, PlayerHand, PlayerBase)
deriving instance Show (Action a)


-- * The impassable TH barrier: nothing crosses from above or below..

makeLenses ''Field
makeLenses ''GameState
makeLenses ''Henchman
makeLenses ''Player
makeLenses ''Territory
makeLenses ''Squad
makeLenses ''DGangman
