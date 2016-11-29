
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- module Sim
--   ()
-- where


import           Data.Maybe
import           Data.List
import qualified Data.Set              as S
import qualified Data.Map.Lazy         as M
import           Prelude.Unicode
import           System.Random.Shuffle as Sys
import           System.IO.Unsafe      as Sys


-- * Generic utils

remove_duplicates ∷ Ord a ⇒ [a] → [a]
remove_duplicates = S.toList . S.fromList

shuffle_list ∷ [a] → [a]
shuffle_list = Sys.unsafePerformIO ∘ Sys.shuffleM

move_cards ∷ Int → [a] → [a] → ([a], [a])
move_cards n from to
  = let new_from = drop n from
        new_to   = take n from ++ to
    in (from, to)

split_by ∷ Int → (a → Bool) → [a] → ([a], [a])
split_by n f xs = _split_by n f xs ([], [])
  where _split_by 0 f xs     (acct, accf) = (acct, accf ++ xs)
        _split_by n f []     (acct, accf) = (acct, accf)
        _split_by n f (x:xs) (acct, accf) = _split_by (n - 1) f xs
                                            $ if f x
                                              then (x:acct,   accf)
                                              else (  acct, x:accf)


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

data WGangman where
  WGangman ∷ (Show (IGangman gang)) ⇒ {
    wga_ix         ∷ IGangman gang
  , wga_deck_count ∷ Int
  , wga_battelcry  ∷ BattleCry
  , wga_capture    ∷ Capture
  } → WGangman
deriving instance Show WGangman

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
  =  TerrW1
  |  TerrW2
  |  TerrD1
  |  TerrD2
  |  TerrG1
  |  TerrG2
  |  TerrA1
  |  TerrA2
  |  TerrP1
  |  TerrP2
  |  TerrP3
  |  TerrP4
  |  TerrV1
  |  TerrV2
  |  TerrV3
  |  TerrV4
  deriving (Eq, Ord, Show)

newtype Attack       = Attack       Int           deriving (Eq, Num, Show)
newtype Defence      = Defence      Int           deriving (Eq, Num, Show)
newtype Swag         = Swag         Int           deriving (Eq, Num, Show)
newtype AtSizes      = AtSizes      [Int]         deriving (         Show)
newtype EnablesMercs = EnablesMercs [IMerc]       deriving (         Show)
newtype GangDeck     = GangDeck     [WGangman]    deriving (         Show)
newtype PlayerDeck   = PlayerDeck   [WGangman]    deriving (         Show)
newtype PlayerBase   = PlayerBase   [WHench]      deriving (         Show)
newtype PlayerHand   = PlayerHand   [WHench]      deriving (         Show)
newtype TerrInPlay   = TerrInPlay   [ITerritory]  deriving (         Show)
newtype TerrDeck     = TerrDeck     [ITerritory]  deriving (         Show)
newtype MercsInPlay  = MercsInPlay  [IMerc]       deriving (         Show)
newtype MercsDeck    = MercsDeck    [IMerc]       deriving (         Show)
newtype Sitting      = Sitting      [IGang]       deriving (         Show)

type family Index a where
  Index Player    = IPlayer
  Index Territory = ITerritory
  Index Hench     = IHench

class (Show (Index a)) ⇒ Indexed a where
  index          ∷ a → Index a


-- * Composite types

data WHench where
  CMerc     ∷ IMerc → WHench
  CGangman  ∷ WGangman → WHench
  CFanatic  ∷ WHench
  CSlave    ∷ WHench
deriving instance Show WHench

proof = WGangman Chemist1

data BattleCry
  =  BattleCry [Action] | NoBattleCry
  deriving Show

data Capture
  =  Capture [Action] | NoCapture
  deriving Show


-- * Game data

game_henchmen
  = [ (Gangman,   (Attack 1,  Defence 1))
    , (Merc,      (Attack 1,  Defence 1))
    , (Fanatic,   (Attack 2,  Defence 0))
    , (Slave,     (Attack 0,  Defence 1)) ]

game_territories
  = [ (TerrW1_ForbiddenRiver,    (Water,     Swag 5,   AtSizes [2, 3, 4]))
    , (TerrW2_CitadelOfFlayedOne,    (Water,     Swag 5,   AtSizes [   3, 4]))
    , (TerrD1_ChurchRevelationOfTheProphet,    (Drugs,     Swag 5,   AtSizes [2,    4]))
    , (TerrD2_ClinicEternalLife,    (Drugs,     Swag 5,   AtSizes [      4]))
    , (TerrG1_OilPumpOfHottyFred,    (Gas,       Swag 5,   AtSizes [2, 3, 4]))
    , (TerrG2_RainbowLake,    (Gas,       Swag 5,   AtSizes [   3, 4]))
    , (TerrA1_BulletFarm,    (Ammo,      Swag 5,   AtSizes [2, 3, 4]))
    , (TerrA2_ArmoryOfMagniKrugger,    (Ammo,      Swag 5,   AtSizes [   3, 4]))
    , (TerrP1_IronTown,    (Scrap,     Swag 4,   AtSizes [2, 3, 4]))
    , (TerrP2_SmelteryOfHragBakster,    (Scrap,     Swag 4,   AtSizes [2, 3, 4]))
    , (TerrP3_ShiningBurialGrounds,    (Scrap,     Swag 4,   AtSizes [   3, 4]))
    , (TerrP4_FactoryMechanicalMessiah,    (Scrap,     Swag 4,   AtSizes [      4]))
    , (TerrV1_BarTheLastDrop,    (Services,  Swag 4,   AtSizes [2, 3, 4]))
    , (TerrV2_BrothelKittiesOfTheMaster,    (Services,  Swag 4,   AtSizes [2, 3, 4]))
    , (TerrV3_FightingPitBloodAndConcrete,    (Services,  Swag 4,   AtSizes [   3, 4]))
    , (TerrV4_CasinoStElmosFires,    (Services,  Swag 4,   AtSizes [      4])) ]

game_gangs
  = [ (Valkiry,   (EnablesMercs [Grasper,    Guitarist],
                   GangDeck [ WGangman ValkLead    1
                               NoBattleCry
                               NoCapture
                            , WGangman Valk1       3
                               NoBattleCry
                               NoCapture
                            , WGangman Valk2       3
                               NoBattleCry
                               NoCapture
                            , WGangman Valk3       3
                               NoBattleCry
                               NoCapture ]))
    , (Gents,     (EnablesMercs [Madcap,   Prothrall],
                   GangDeck [ WGangman GentLead    1
                               NoBattleCry
                               NoCapture
                            , WGangman Gent1       3
                               NoBattleCry
                               NoCapture
                            , WGangman Gent2       3
                               NoBattleCry
                               NoCapture
                            , WGangman Gent3       3
                               NoBattleCry
                               NoCapture ]))
    , (Patrol,    (EnablesMercs [Bonecrusher,    Preacher],
                   GangDeck [ WGangman PatrolLead  1
                               NoBattleCry
                               NoCapture
                            , WGangman Patrol1     3
                               NoBattleCry
                               NoCapture
                            , WGangman Patrol2     3
                               NoBattleCry
                               NoCapture
                            , WGangman Patrol3     3
                               NoBattleCry
                               NoCapture ]))
    , (Slavers,   (EnablesMercs [Drummer, Bounder],
                   GangDeck [ WGangman SlaverLead  1
                               NoBattleCry
                               NoCapture
                            , WGangman Slaver1     3
                               NoBattleCry
                               NoCapture
                            , WGangman Slaver2     3
                               NoBattleCry
                               NoCapture
                            , WGangman Slaver3     3
                               NoBattleCry
                               NoCapture ]))
    , (Gunners,   (EnablesMercs [Barkeeper,     BlackWidow],
                   GangDeck [ WGangman GunnerLead  1
                               NoBattleCry
                               NoCapture
                            , WGangman Gunner1     3
                               NoBattleCry
                               NoCapture
                            , WGangman Gunner2     3
                               NoBattleCry
                               NoCapture
                            , WGangman Gunner3     3
                               NoBattleCry
                               NoCapture ]))
    , (Chemists,  (EnablesMercs [Striker,  Dealess],
                   GangDeck [ WGangman ChemistLead 1
                               NoBattleCry
                               NoCapture
                            , WGangman Chemist1    3
                               NoBattleCry
                               NoCapture
                            , WGangman Chemist2    3
                               NoBattleCry
                               NoCapture
                            , WGangman Chemist3    3
                               NoBattleCry
                               NoCapture ])) ]


-- * Game data queries

ihench_attack    ∷ IHench → Attack
ihench_attack    = fst ∘ (M.fromList game_henchmen M.!)

ihench_defence   ∷ IHench → Defence
ihench_defence   = snd ∘ (M.fromList game_henchmen M.!)

iterr_resource   ∷ ITerritory → IResource
iterr_resource   = (\(x, _, _) → x) ∘ (M.fromList game_territories M.!)

iterr_init_swag  ∷ ITerritory → Swag
iterr_init_swag  = (\(_, x, _) → x) ∘ (M.fromList game_territories M.!)

igang_deck       ∷ IGang → GangDeck
igang_deck       = snd ∘ (M.fromList game_gangs M.!)

igang_enables_mercs ∷ IGang → EnablesMercs
igang_enables_mercs = fst ∘ (M.fromList game_gangs M.!)


-- * Active objects

instance Indexed Hench     where
  index                    = he_ix

instance Indexed Territory where
  index                    = te_ix

instance Indexed Player    where
  index                    = pl_ix

data Hench
  =  Hench {
      he_ix           ∷ IHench
    , he_whench       ∷ WHench
    , he_untapped     ∷ Bool
    }
  deriving (Show)

data Territory
  =  Territory {
      te_ix           ∷ ITerritory
    , te_swag         ∷ Swag
    , te_owner        ∷ Maybe IGang
    , te_henchmen     ∷ [Hench]
    }
  deriving (Show)

data Player
  =  Player {
      pl_ix           ∷ IPlayer
    , pl_gang         ∷ IGang
    , pl_deck         ∷ [IHench]    -- Visibility: noone;    only mercs and gangmen
    , pl_hand         ∷ [IHench]    -- Visibility: player;   only mercs and gangmen
    , pl_base         ∷ [Hench]     -- Visibility: everyone; everyone
    , pl_terrs        ∷ [ITerritory]
    , pl_swag         ∷ Swag
    }
  deriving (Show)

data Field
  =  Field {
      fi_terr_current ∷ [Territory]  -- 1 to 4
    , fi_merc_current ∷ [IHench]     -- mercs only
    , fi_terr_closed  ∷ [ITerritory]
    , fi_merc_closed  ∷ [IHench]     -- mercs only
    }
  deriving (Show)

data GameState
  =  GameState {
      gs_field        ∷ Field
    , gs_players      ∷ [Player]
    , gs_current      ∷ IPlayer
    }
  deriving (Show)

data Squad
  =  Squad {
      sq_targets      ∷ [(Territory, IHench)]
    }
  deriving (Show)


-- * Actions

-- game_state_next_action ∷ GameState

data IAction
  = IInitialTerrDeck
  | IInitialMercsDeck
  | IMoveMercsIntoPlay
  | IInitialDeckBaseHand
  | IPlayerSitting
  deriving (Enum, Eq, Ord, Show)

data Action
  = InitialTerrDeck         { player_gangs        ∷ [IGang]
                            , new_global_terrs    ∷ TerrDeck }
  | InitialMercsDeck        { player_gangs        ∷ [IGang]
                            , merc_multiplier     ∷ Int
                            , new_closed_mercs    ∷ MercsDeck }
  | MoveMercsIntoPlay       { merc_count          ∷ Int
                            , closed_mercs        ∷ MercsDeck
                            , open_mercs          ∷ MercsInPlay
                            , new_closed_open     ∷ (MercsDeck, MercsInPlay) }
  | InitialDeckBaseHand     { player_gang         ∷ IGang
                            , base_size           ∷ Int
                            , deck_base_hand      ∷ (PlayerDeck, PlayerBase, PlayerHand) }
  | PlayerSitting           { player_gangs        ∷ [IGang]
                            , sitting             ∷ Sitting }
  deriving Show

next_phase ∷ GameState → Phase → Phase
next_phase (GameState (Field cur_terr cur_merc clo_terr clo_merc) players icur)
           p
  | CheckDayEnd  ← p = if length cur_terr > 0
                       then DayPlayerBegin
                       else succ p
  | CheckGameEnd ← p = if length cur_terr + length clo_terr > 0
                       then DayPlayerBegin
                       else succ p
  | GameEnd      ← p = error "Asked to continue game past GameEnd."
  | otherwise        = succ p

-- run_action ∷ GameState → Action → GameState
-- run_action (GameState (Field cur_terr cur_merc clo_terr clo_merc) players icur)
--            act
--   | (InitialTerrDeck  player_gangs _) ← act
--     → 

complete_action :: Action → Action
complete_action x@(InitialTerrDeck  player_gangs _)
  = x { new_global_terrs =
        let game_size = length player_gangs
        in TerrDeck ∘ map fst ∘ (flip filter) game_territories $
            \(iterr, (_, _, AtSizes allowed_sizes)) → game_size ∈ allowed_sizes }

complete_action x@(InitialMercsDeck player_gangs merc_multiplier _)
  = x { new_closed_mercs =
        let
        in MercsDeck ∘ shuffle_list ∘ (flip foldMap) game_gangs $
            \(igang, (EnablesMercs imercs, _))
             → if igang ∈ player_gangs
               then foldl (++) [] $ take merc_multiplier $ repeat imercs
               else [] }

complete_action x@(MoveMercsIntoPlay nmercs (MercsDeck deck) (MercsInPlay inplay) _)
  = x { new_closed_open =
        let (new_deck, new_inplay) = move_cards nmercs deck inplay
        in ( MercsDeck new_deck
           , MercsInPlay new_inplay ) }

complete_action x@(InitialDeckBaseHand gang base_size _)
  = x { deck_base_hand =
        let GangDeck gangmen = igang_deck gang
            wgang_has_capture (WGangman _ _ _ capture) | NoCapture ← capture = False
                                                       | Capture _ ← capture = True
            shuffled         = shuffle_list gangmen
            (base, rest)     = split_by base_size wgang_has_capture gangmen
            (deck, hand)     = splitAt 4 rest
        in ( PlayerDeck $ deck
           , PlayerBase $ fmap CGangman base
           , PlayerHand $ fmap CGangman hand ) }

complete_action x@(PlayerSitting gangs _)
  = x { sitting = Sitting $ shuffle_list gangs }


-- * Game phases

data Phase
  =  GameBegin

  |  InitialTerrsMercs -- 1. populate terrs_closed =
                      --      case 4 → gen 2 Water ++ gen 2 Drugs ++ gen 2 Gas ++ gen 2 Ammo ++ gen 4 Scrap ++ gen 4 Services
                      --      case 3 → gen 2 Water ++ gen 0 Drugs ++ gen 2 Gas ++ gen 2 Ammo ++ gen 3 Scrap ++ gen 3 Services
                      --      case 2 → gen 1 Water ++ gen 1 Drugs ++ gen 1 Gas ++ gen 1 Ammo ++ gen 2 Scrap ++ gen 2 Services
                      -- 2. populate mercs_closed =
                      --      shuffle $
                      --      sum $ mapProduct (enumerate ∷ MercIndex) (map pl_gang players) $
                      --              \(midx, pgang) →
                      --                 if mercidx_enabling_gang midx = pgang then gen 3 midx else []
                      -- 3. move 2 mercs_closed → mercs_current

  |  PlayerDecksBasesHands -- forM players $
                      --   \player →
                      --      let cards = pl_init_cards $ pl_gang player
                      --      pl_deck player <- shuffle $ gang_init_deck cards
                      --      pl_base player <- gang_init_base $ pl_gang player
                      --      (pl_hand player, pl_deck player) <- (take 4 pl_deck, skip 4 pl_deck)

  |  SitPlayers       -- 1. elect a permanent, random, continious order of players
                      -- 2. sit players according to order

  |  NewTerritories   -- take (length players) terr_closed → terr_current
                      --   initialize with terr_init_swag

  |  ChoosePlayer     -- ...

  |  DayPlayerBegin

  |  HireHenchs       -- 1. 0 to 1             Merc    merc_current → pl_hand
                      -- 2. if took merc
                      --         1                 head merc_closed → merc_current

  |  PlayCard         -- 1.      1            Hench         pl_hand → pl_base
                      -- 2. player may activate BattleCry

  |  CaptureMove      -- 1. choose terr_set from terr_current
                      -- 2. elect a valid (possibly empty) attack squad
                      -- validity condition:
                      -- if at_owner = Just pl_gang ||
                      --    length pl_attack_squad = 0
                      -- then True
                      -- else squad_attack > terr_defence &&
                      --      some (hench_gang = pl_gang) pl_attack_squad
                      --    0 to inf untapped Hench pl_base         → pl_attack_squad
                      -- 3. all enemy Slave                         → Void
                      --    all enemy at_henchmen tapped            → pl_base (at_owner terr)
                      -- 4. all non-Fanatic         pl_attack_squad → at_hench terr
                      --    all     Fanatic         pl_attack_squad → Void
                      -- 5. player may activate Capture for any/all henchmen of pl_attack_squad

  |  HarvestSwag      -- forM (filter (at_owner = pl_gang) terr_current) $
                      --   \terr →
                      --     case at_swag terr of
                      --       0 → return
                      --       n → do
                      --         at_swag = at_swag - 1
                      --         pl_swag = pl_swag + 1
                      -- 1. pl_swag = pl_swag + length $

  |  TerritoryOwning  -- forM terr_current $
                      --   \terr → do
                      --     if at_swag = 0
                      --     then case at_owner terr
                      --            Just owner → do terr → owner
                      --                             (shuffled at_henchmen) → bottom_of (pl_deck owner)
                      --            Nothing    → do terr → Void

  |  GetHenchs        -- 1. (shuffled pl_hand) → bottom_of pl_deck
                      -- 2.     case length pl_deck
                      --          1..4   → take 1..4 pl_deck → pl_hand
                      --             0   → case player_choice of
                      --                      take 1 random merc_closed → pl_hand
                      --                      take 1 fanatic            → pl_base
                      --                      take 2 slaves             → pl_base

  |  CheckDayEnd      -- repeat while (length terr_current) > 0

  |  UntapHenchs      -- forM pl_base . players $
                      --   \henchman → do $
                      --     ah_untapped henchman → True

  |  CheckGameEnd     -- repeat while (current + closed) territories left

  |  ScoreCount       -- winner:
                      --   let max_score = max player_score players
                      --       winners   = filter (player_score = max_score) players
                      --       max_terrs = max pl_terrs winners
                      --       winners2  = filter (pl_terrs = max_terrs) winners
                      --       pl_mercs  = length (filter is_merc (pl_deck ++ pl_base ++ pl_hand))
                      --       max_mercs = max pl_mercs winners2
                      --       winners3  = filter (pl_mercs = max_mercs) winners2
                      --       winner    = max enumIndex winners3

  |  GameEnd
  deriving (Enum, Eq, Ord, Show)


main = undefined

-- ПОЯСНЕНИЯ ПО ТЕРМИНАМ
-- Permanent - Постоянно действующее свойство Подручного.
-- Holder - Удерживающий Территорию игрок, т.е. игрок, у которого есть Подручные на этой Территории.
-- ...beat Henchman from Territory... - Holder Территории возвращает на свою Базу с неё указанное число любых или конкретных 
-- Подручных, изнуренными или свежими.
-- ...Trash this Henchman... - Указанный Подручный или несколько перемешиваются и помещаются под колоду владельца.
-- 
-- Valkyrez
-- Mary «Right Executess»	Battlecry:	Choose in your hand and remove from the game 3 cards or less. Put on your Base equal number of untapped Henchmen from your hand and\or top of your Deck. They Battlecry skills don't works.
-- Saviess	Battlecry:	Choose one Henchman on your Territory. Beat it untapped. Play its Battlecry skill.
-- Robbess	Capture:	Choose one Henchman in your hand. Put it on your Base tapped. It Battlecry skill don't works.
-- Nomadess	Capture:	Choose one Henchman on your Base. Move its to this Territory. It Capture skill don't works.
-- 
-- Gentz
-- Germont «Young Baron»	Battlecry:	Choose in your hand and remove from the game 3 cards or less. Choose on your Base equal number of different Henchmen. Play they Battlecry skills.
-- Temptress	Battlecry:	Choose from OPEN_MERCS any one Merc Henchman and put it on your Base untapped. It Battlecry skill don't works. Tap Temptress.
-- Bodyguard	Capture:	Choose one Henchman in your hand. Put it on your Base untapped. It Battlecry skill don't works. Trash Bodyguard.
-- Smartass	Capture:	Choose any one Henchman on your Base. Play it Battlecry skill.
-- 
-- Patrolz
-- Carl «Bookkeeper»	Battlecry:	Choose in your hand and remove from the game 3 cards or less. Remove from the game double number of Swag tokens on any Territories in any combination.
-- Devastator	Battlecry:	Choose any Territory. Remove from the game a Swag token from it. Beat tapped Henchman from it.
-- Sapper	Capture:	Remove from the game a Swag token from this Territory.
-- Gambit	Capture:	Choose two different Territories. Move a Swag token from one to another. Beat Gambit untapped.
-- 
-- Slaverz
-- Leo «Kind Daddy»	Battlecry:	Choose in your hand and remove from the game 3 cards or less. Put on your Base double number of untapped Thrall Henchmen.
-- Teamster	Battlecry:	In this gameround power of Thrall Henchmen considered.
-- Wardess	Capture:	Trash up to two your Henchmen on this Territory. Remove from the game equal number of Swag tokens from this Territory.
-- Huntsman	Capture:	Put one Thrall Henchman on this Territory.
-- 
-- Gunnerz
-- Billy «Bullet Head»	Battlecry:	Choose in your hand and remove from the game 3 cards or less. Beat tapped from the game double number of Henchmen on any Territories in any combination.
-- Explodess	Battlecry:	Choose any one Territory. Beat one Henchman from it tapped.
-- Flamer	Capture:	Trash all your Henchmans from this Territory. Remove from the game this Territory and all Swag tokens on it.
-- Looter	Capture:	Take Swag token from this Territory immediately. Beat Looter tapped.
-- 
-- Chemz
-- Angelina «Pure Blood»	Battlecry:	Choose in your hand and remove from the game 3 cards or less. Put on your Base equal number of untapped Fanatic Henchmen.
-- Dodger	Battlecry:	Choose any one Henchman on any Base. Trash it. Put untapped Fanatic Henchman on your Base.
-- Patcher	Capture:	Choose on your Base tapped Henchman. Untap it. It Battlecry skill don't works.
-- Madman	Capture:	Beat Madman tapped. Put untapped Fanatic Henchman on your Base.
-- 
-- Mercz
-- Grasper	Battlecry:	Choose any Territory. Take a Swag token from it.
-- Guitarist	Battlecry:	Beat up to two your Henchmen untapped from one or different Territories.
-- Madcap	Battlecry:	Choose one Henchman in your hand. Put it on your Base untapped. It Battlecry skill don't works.
-- Prothrall	Battlecry:	Choose any one Henchman on any Base. Trash it. Put one Thrall Henchman on your Base untapped.
-- Bonecrusher	Battlecry:	Choose up to two Henchmen on your Base. Play Battlecry skills of them. Trash choosen Henchmen.
-- Preacher	Battlecry:	Choose two untapped Henchmans on your Base. Trash it. Put two untapped Fanatic Henchman on your Base.
-- Drummer	Battlecry:	Choose any one Henchman on your Base. If it tapped, untap it. Play its Battlecry skill.
-- Bounder	Battlecry:	Remove from the game a Swag token from each Territories.
-- Barkeeper	Battlecry:	Choose up to two tapped Henchman on your Base. Untap it. It Battlecry skill don't works.
-- BlackWidow	Battlecry:	Each opponent choose one untapped Henchman from his Base and trash it.
-- Striker	Battlecry:	Choose any Territory. Remove from the game two Swag tokens from it.
-- Dealess	Battlecry:	In this gameround you cam attack twice, two different Territories. Effect do not stack.
-- 
-- Thrall	Permanent:	Power of Slave Henchman doesn't impact. Immediately remove it from the game after it return to the Territory to Base or trash it.
-- Fanatic	Permanent:	Power of Fanatic Henchman has impact as two Henchmen. Immediately remove it from the game after capture Territory or trash it.
