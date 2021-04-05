module UI.Components.StateBasedCRDT.TwoPhaseSet where

import Prelude

import Data.CRDT.GSet (GSet)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import UI.Components.StateBasedCRDT (Editor, EditorAction(..), mkEditor)

data View
  = Split
  | Combined

type State
  = { currentInput :: String
    , currentState :: (GSet String)
    , currentView :: View
    }

data Action
  = SetView View

editor :: forall m. Editor (GSet String) m
editor =
  mkEditor
    { initialState
    , updateState
    , render
    , handleAction
    }
  where
  initialState gset = { currentInput: "", currentState: gset, currentView: Combined }

  updateState gset st = st { currentState = gset }

  render st =
    HH.div_
      [ HH.div [ HP.class_ $ H.ClassName "center-and-space-between" ]
          [ HH.text "View"
          , HH.div [ HP.classes $ H.ClassName <$> [ "buttons", "has-addons" ] ]
              [ HH.button
                  [ HP.classes $ H.ClassName <$> [ "button", "is-small" ]
                  , HE.onClick \_ -> EditorSpecificAction $ SetView Combined
                  ]
                  [ HH.text "Combined" ]
              , HH.button
                  [ HP.classes $ H.ClassName <$> [ "button", "is-small" ]
                  , HE.onClick \_ -> EditorSpecificAction $ SetView Split
                  ]
                  [ HH.text "Split" ]
              ]
          ]
      ]

  handleAction = case _ of
    SetView view -> H.modify_ \st -> st { currentView = view }
