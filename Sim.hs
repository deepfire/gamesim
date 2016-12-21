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
module Sim where

import           Control.Lens          hiding (Indexed(..), Index(..))
import           Control.Monad.Freer
import           Control.Monad.Freer.Internal
import           Data.Maybe
import           Data.List
import qualified Data.Set              as S
import qualified Data.Map.Lazy         as M
import           Prelude.Unicode

import Utils
import SimData


-- * Composite types

data WGangman where
  WGangman ∷ (Show (IGangman gang)) ⇒ {
    wga_ix         ∷ IGangman gang
  , wga_deck_count ∷ Int
  , wga_battlecry  ∷ BattleCry
  , wga_capture    ∷ Capture
  } → WGangman
deriving instance Show WGangman

proof = WGangman Chemist1

data BattleCry
  =  BattleCry [Action ()] | NoBattleCry
  deriving Show

data Capture
  =  Capture [Action ()] | NoCapture
  deriving Show

data WHench where
  CMerc     ∷ IMerc → WHench
  CGangman  ∷ WGangman → WHench
  CFanatic  ∷ WHench
  CSlave    ∷ WHench
deriving instance Show WHench

newtype GangDeck     = GangDeck     [WGangman]    deriving (         Show)
newtype PlayerDeck   = PlayerDeck   [WGangman]    deriving (         Show)
newtype PlayerBase   = PlayerBase   [WHench]      deriving (         Show)
newtype PlayerHand   = PlayerHand   [WHench]      deriving (         Show)

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

type family Index a where
  Index Player    = IPlayer
  Index Territory = ITerritory
  Index Hench     = IHench

class (Show (Index a)) ⇒ Indexed a where
  index          ∷ a → Index a

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
      fi_terrsinplay  ∷ TerrsInPlay  -- 1 to 4
    , fi_mercsinplay  ∷ MercsInPlay  -- mercs only
    , fi_terrdeck     ∷ TerrDeck
    , fi_mercdeck     ∷ MercDeck     -- mercs only
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

runAction ∷ GameState → Eff '[Action] w → Eff '[] w
runAction gs m = loop gs m where
  loop ∷ GameState → Eff (Action ': r) w → Eff r w
  loop gs (Val x) = return x
  loop gs@(GameState fi@(Field ti@(TerrsInPlay ticards) mi@(MercsInPlay micards)
                               td@(TerrDeck    tdcards) md@(MercDeck    mdcards))
                     players icur)
       (E u q) =
    case decomp u of
      Right act →
        case act of

          DumpState →
            k gs gs

          EnterPlayers gangs →
            let players = [ Player { pl_gang = g } | g ← gangs ]
            in k (gs { gs_players = players }) players

          InitialTerrDeck →
            let game_size = length players
                terrdeck  = TerrDeck ∘ map fst ∘ (flip filter) game_territories $
                            \(iterr, (_, _, AtSizes allowed_sizes)) → game_size ∈ allowed_sizes
            in k (gs { gs_field = fi { fi_terrdeck = terrdeck } }) terrdeck

          InitialMercDeck merc_multiplier →
            let gangs     = fmap pl_gang players
                mercdeck  = MercDeck ∘ shuffle_list ∘ (flip foldMap) game_gangs $
                            \(igang, (EnablesMercs imercs, _))
                            → if igang ∈ gangs
                              then foldl (++) [] $ take merc_multiplier $ repeat imercs
                              else []
            in k (gs { gs_field = fi { fi_mercdeck = mercdeck } }) mercdeck

          MoveMercsIntoPlay nmercs →
            let (new_mdcards, new_micards) = move_cards nmercs mdcards micards
                (nmd, nmi)                 = (MercDeck new_mdcards, MercsInPlay new_micards)
            in k (gs { gs_field = fi { fi_mercdeck    = nmd
                                     , fi_mercsinplay = nmi } })
                 (nmd, nmi)

          InitialDeckHandBase iplayer base_size →
            let idx              = fromEnum iplayer
                p@(Player _ igang _ _ _ _ _) =
                                   players !! idx -- XXX: non-total
                GangDeck gangmen = igang_deck igang
                wgang_has_capture (WGangman _ _ _ capture) | NoCapture ← capture = False
                                                           | Capture _ ← capture = True
                shuffled         = shuffle_list gangmen
                (basecs, restcs) = split_by base_size wgang_has_capture gangmen
                (deckcs, handcs) = splitAt 4 restcs
                deck             = fmap wga_ix deckcs
                hand             = fmap CGangman handcs
                base             = fmap CGangman basecs
            in k (gs { gs_players = players & element idx .~ p { pl_deck = deck, pl_base = base, pl_hand = hand } } )
                 (deck, hand, base)

-- complete_action x@(PlayerSitting gangs _)
--   = x { sitting = Sitting $ shuffle_list gangs }
--   = x { new_terrdeck =
--         let game_size = length player_gangs
--         in TerrDeck ∘ map fst ∘ (flip filter) game_territories $
--             \(iterr, (_, _, AtSizes allowed_sizes)) → game_size ∈ allowed_sizes }
          
      Left u → E u (tsingleton (k gs))
    where k s = qComp q (loop gs)

runGameState ∷ GameState → Eff '[Action] GameState → GameState
runGameState gs = run ∘ runAction gs

game ∷ GameState
game = runGameState (GameState {}) $ do
  send $ EnterPlayers [Valkiry]
  send DumpState

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

next_phase ∷ GameState → Phase → Phase
next_phase (GameState (Field (TerrsInPlay cur_terr) (MercsInPlay cur_merc)
                             (TerrDeck    clo_terr) (MercDeck    clo_merc))
                      players
                      icur)
           p
  | CheckDayEnd  ← p = if length cur_terr > 0
                       then DayPlayerBegin
                       else succ p
  | CheckGameEnd ← p = if length cur_terr + length clo_terr > 0
                       then DayPlayerBegin
                       else succ p
  | GameEnd      ← p = error "Asked to continue game past GameEnd."
  | otherwise        = succ p


-- * Game phases

data Phase
  =  GameBegin        -- 1. choose player gangs

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
