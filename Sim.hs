-- module Sim
--   ()
-- where

type Name = String

data Gang
  = Valkiry
  | Gents
  | Patrol
  | Slavers
  | Gunners
  | Chemists

data Resource
  = Water
  | Drugs
  | Gas
  | Ammo
  | Scrap
  | Services

data Territory
  = Territory {
      terr_name      :: Name
    , terr_res       :: Resource
    , terr_init_swag :: Int
    }

type Action = GameState -> GameState

data HenchAction
  = BattleCry  Action
  | Capture    Action

data MercIndex
  = MercBarkeeper
  | MercAbomination
  | MercPreacher
  | MercGrabber
  | MercFlamer
  | MercDrummer
  | MercMadcap
  | MercGuitarist
  | MercTaskmaster
  | MercDealess
  | MercWidow
  | MercConman
  deriving (Enum, Show)

mercidx_enabling_gang :: MercIndex -> Gang

data Mercenary
  = Mercenary {
    }

data Gangman
  = Gangman {
      gman_gang :: Gang
    }

data Henchman
  = MercHenchman Mercenary
  | GangHenchman Gangman
  | FanaticHenchman
  | SlaveHenchman

newtype Attack  = Attack Int
newtype Defence = Defence Int
newtype Swag    = Swag Int

data ActHenchman
  = ActHenchman {
      ah_henchman  :: Henchman
    , ah_untapped  :: Bool
    }

data ActTerritory
  = ActTerritory {
      at_terr      :: Territory
    , at_swag      :: Swag
    , at_owner     :: Maybe Gang
    , at_henchmen  :: [ActHenchman]
    }

data Field
  = Field {
      terr_current :: [ActTerritory] -- 1 to 4
    , merc_current :: [Mercenary]
    , terr_closed  :: [Territory]
    , merc_closed  :: [Mercenary]
    }

data Player
  = Player {
      pl_gang      :: Gang
    , pl_deck      :: [Henchman]    -- Visibility: noone;    only mercs and gangmen
    , pl_hand      :: [Henchman]    -- Visibility: player;   only mercs and gangmen
    , pl_base      :: [ActHenchman] -- Visibility: everyone; everyone
    , pl_terrs     :: [Territory]
    , pl_swag      :: Swag
    }

data GameState
  = GameState {
      field        :: Field
    , players      :: [Player]
    }

data Squad
  = Squad {
      sq_target_terr :: ActTerritory
    }


hench_attack  :: Henchman -> Attack
hench_attack  (MercHenchman   _) = Attack  1
hench_attack  (GangHenchman   _) = Attack  1
hench_attack  FanaticHenchman    = Attack  2
hench_attack  SlaveHenchman      = Attack  0

hench_defence :: Henchman -> Defence
hench_defence (MercHenchman   _) = Defence 1
hench_defence (GangHenchman   _) = Defence 1
hench_defence FanaticHenchman    = Defence 0
hench_defence SlaveHenchman      = Defence 1


-- Day: 1-4
-- Rounds: 1-inf
-- Day end condition: no more active territories
-- Territory deactivation: non-zero swag counter at end of round
-- Round: a standartized sequence of player actions

gang_init_cards :: Gang -> [Henchman]
gang_init_cards Valkiry  = []
gang_init_cards Gents    = []
gang_init_cards Patrol   = []
gang_init_cards Slavers  = []
gang_init_cards Gunners  = []
gang_init_cards Chemists = []

gang_init_base :: Gang -> [Henchman] -- length = 2, two different with Capture != Nothing
gang_init_base = undefined

data GameIndex
  = PhaseBegin        -- 1) populate terrs_closed =
                      --      case 4 -> gen 2 Water ++ gen 2 Drugs ++ gen 2 Gas ++ gen 2 Ammo ++ gen 4 Scrap ++ gen 4 Services
                      --      case 3 -> gen 2 Water ++ gen 0 Drugs ++ gen 2 Gas ++ gen 2 Ammo ++ gen 3 Scrap ++ gen 3 Services
                      --      case 2 -> gen 1 Water ++ gen 1 Drugs ++ gen 1 Gas ++ gen 1 Ammo ++ gen 2 Scrap ++ gen 2 Services
                      -- 2) populate mercs_closed =
                      --      shuffle $
                      --      sum $ mapProduct (enumerate :: MercIndex) (map pl_gang players) $
                      --              \(midx, pgang) ->
                      --                 if mercidx_enabling_gang midx = pgang then gen 3 midx else []
                      -- 3) move 3 mercs_closed -> mercs_current
                      -- 4) forM players $
                      --      \player ->
                      --         let cards = pl_init_cards $ pl_gang player
                      --         pl_deck player <- shuffle $ gang_init_deck cards
                      --         pl_base player <- gang_init_base $ pl_gang player
                      --         (pl_hand player, pl_deck player) <- (take 4 pl_deck, skip 4 pl_deck)
                      -- 5) elect a permanent, random, continious order of players
                      -- 6) sit players according to order

  | PhaseDay1
  | PhaseDay2
  | PhaseDay3
  | PhaseDay4
  | PhaseEnd          -- invariant: no (current + closed) territories left
                      -- winner:
                      --   let max_score = max player_score players
                      --       winners   = filter (player_score = max_score) players
                      --       max_terrs = max pl_terrs winners
                      --       winners2  = filter (pl_terrs = max_terrs) winners
                      --       pl_mercs  = length (filter is_merc (pl_deck ++ pl_base ++ pl_hand))
                      --       max_mercs = max pl_mercs winners2
                      --       winners3  = filter (pl_mercs = max_mercs) winners2
                      --       winner    = max enumIndex winners3

player_score :: Player -> Int
player_score (Player _gang _deck _hand _base terrs swag) =
  swag +
  2 * length terrs +
  2 * (remove_duplicates terrs)

data DayIndex
  = PhaseTerritories  -- take (length players) terr_closed -> terr_current
                      --   initialize with terr_init_swag

  | PhaseRounds       -- repeat while (length terr_current) > 0

  | PhaseRefresh      -- forM pl_base . players $
                      --   \henchman -> do $
                      --     ah_untapped henchman -> True
  deriving (Enum, Show)

data RoundIndex
  = PhaseHire     -- 1) 0 to 1             Merc    merc_current -> pl_hand
                  -- 2) if took merc
                  --         1                 head merc_closed -> merc_current

  | PhasePlayCard -- 1)      1            Hench         pl_hand -> pl_base
                  -- 2) player may activate BattleCry

  | PhaseCapture  -- 1) choose terr_set from terr_current
                  -- 2) elect a valid (possibly empty) attack squad
                  -- validity condition:
                  -- if at_owner = Just pl_gang ||
                  --    length pl_attack_squad = 0
                  -- then True
                  -- else squad_attack > terr_defence &&
                  --      some (hench_gang = pl_gang) pl_attack_squad
                  --    0 to inf untapped Hench pl_base         -> pl_attack_squad
                  -- 3) all enemy SlaveHenchman                 -> Void 
                  --    all enemy at_henchmen tapped            -> pl_base (at_owner terr)
                  -- 4) all non-FanaticHenchman pl_attack_squad -> at_henchmen terr
                  --    all     FanaticHenchman pl_attack_squad -> Void
                  -- 5) player may activate Capture for any/all henchmen of pl_attack_squad

  | PhaseHarvest  -- forM (filter (at_owner = pl_gang) terr_current) $
                  --   \terr ->
                  --     case at_swag terr of
                  --       0 -> return
                  --       n -> do
                  --         at_swag = at_swag - 1
                  --         pl_swag = pl_swag + 1
                  -- 1) pl_swag = pl_swag + length $ 

  | PhaseControl  -- forM terr_current $
                  --   \terr -> do
                  --     if at_swag = 0
                  --     then case at_owner terr
                  --            Just owner -> do terr -> owner
                  --                             (shuffled at_henchmen) -> bottom_of (pl_deck owner)
                  --            Nothing    -> do terr -> Void

  | PhaseTrash    -- 1) (shuffled pl_hand) -> bottom_of pl_deck
                  -- 2)     case length pl_deck
                  --          1..4   -> take 1..4 pl_deck -> pl_hand
                  --             0   -> case player_choice of
                  --                      take 1 random merc_closed -> pl_hand
                  --                      take 1 fanatic            -> pl_base
                  --                      take 2 slaves             -> pl_base
    
  deriving (Enum, Show)


main = undefined
