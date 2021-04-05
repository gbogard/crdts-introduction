module UI.Components.StateBasedCRDT.GSet where

import Prelude
import Data.Array (sort)
import Data.CRDT.GSet as GSet
import Data.HashSet as Set
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import UI.Components.StateBasedCRDT (Editor, EditorAction(..), mkEditor)

type State
  = { currentInput :: String
    , currentState :: (GSet.GSet String)
    }

data Action
  = SetCurrentInput String

editor :: forall m. Editor (GSet.GSet String) m
editor =
  mkEditor
    { initialState
    , updateState
    , render
    , handleAction
    }
  where
  initialState gset = { currentInput: "", currentState: gset }

  updateState gset st = st { currentState = gset, currentInput = "" }

  render st@{ currentState } =
    HH.div_
      [ list currentState
      , form st
      ]

  list gset = HH.ul_ <<< map renderItem <<< sort <<< Set.toArray <<< GSet.query $ gset

  renderItem str = HH.li_ [ HH.text str ]

  form { currentState, currentInput } =
    HH.div [ HP.classes $ H.ClassName <$> [ "field", "is-horizontal" ] ]
      [ HH.div [ HP.class_ $ H.ClassName "field-body" ]
          [ HH.div [ HP.class_ $ H.ClassName "field" ]
              [ HH.input
                  [ HP.class_ $ H.ClassName "input"
                  , HP.placeholder "New element"
                  , HE.onValueChange (EditorSpecificAction <<< SetCurrentInput)
                  , HP.value currentInput
                  ]
              ]
          , HH.button
              [ HP.classes $ H.ClassName <$> [ "button", "is-link" ]
              , HE.onClick $ submit currentInput
              ]
              [ HH.text "Add" ]
          ]
      ]

  handleAction = case _ of
    SetCurrentInput input -> H.modify_ \st -> st { currentInput = input }

  submit currentInput _ = NotifyParentAboutNewState $ GSet.insert currentInput
