module UI.Components.StateBasedCRDT.GCounter where

import Prelude
import Data.Array (cons, sort, sortBy)
import Data.CRDT.GCounter as GCounter
import Data.HashMap as Map
import Data.Replication (ReplicaId)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import UI.Components.StateBasedCRDT (Editor, EditorAction(..), mkEditor)

data State
  = State ReplicaId GCounter.GCounter

editor :: forall m. Editor GCounter.GCounter m
editor =
  mkEditor
    { initialState: State
    , updateState
    , render
    , handleAction: \_ -> pure unit
    }
  where
  updateState rid counter _ = State rid counter

  sortByKey (a /\ _) (b /\ _) = compare a b

  render (State rid counter) =
    HH.div_
      [ HH.h5
          [ HP.classes $ H.ClassName <$> [ "is-size-3", "has-text-centered" ] ]
          [ HH.text <<< show <<< GCounter.query $ counter ]
      , HH.table [ HP.class_ $ H.ClassName "table" ]
          [ HH.thead_
              $ counter
              # GCounter.toMap
              # Map.keys
              # sort
              # map (HH.td_ <<< pure <<< HH.text <<< show)
              # cons (HH.td_ [ HH.text "Replica" ])
          , HH.tbody_
              [ HH.tr_
                  $ counter
                  # GCounter.toMap
                  # Map.toArrayBy Tuple
                  # sortBy sortByKey
                  # map (HH.td_ <<< pure <<< HH.text <<< show <<< snd)
                  # cons (HH.td_ [ HH.text "Value" ])
              ]
          ]
      , HH.button
          [ HE.onClick $ increment rid
          , HP.classes $ H.ClassName <$> ["button", "is-link", "is-fullwidth"]
          ]
          [ HH.text "Increment" ]
      ]

  increment rid _ = NotifyParentAboutNewState $ GCounter.increment rid
