module UI.Components.StateBasedCRDT.TwoPhaseSet where

import Prelude
import Data.Array (sort)
import Data.CRDT.TwoPhaseSet as TwoPhaseSet
import Data.HashSet as Set
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
    , currentState :: (TwoPhaseSet.TwoPhaseSet String)
    , currentView :: View
    }

data Action
  = SetView View
  | SetCurrentInput String

editor :: forall m. Editor (TwoPhaseSet.TwoPhaseSet String) m
editor =
  mkEditor
    { initialState
    , updateState
    , render
    , handleAction
    }
  where
  initialState _ set = { currentInput: "", currentState: set, currentView: Combined }

  updateState _ set st = st { currentState = set, currentInput = "" }

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
      , HH.div [ HP.class_ $ H.ClassName "content" ]
          [ case st.currentView of
              Combined -> renderMembers TwoPhaseSet.query st.currentState
              Split -> renderSplit st.currentState
          ]
      , form st
      ]

  renderSplit set =
    HH.div [ HP.class_ $ H.ClassName "mt-3" ]
      [ HH.h6_ [ HH.text "Add-set" ]
      , renderMembers TwoPhaseSet.addSet set
      , HH.h6_ [ HH.text "Tombstone-set" ]
      , renderMembers TwoPhaseSet.tombstoneSet set
      ]

  renderMembers toHashSet set
    | Set.isEmpty $ toHashSet set =
      HH.p
        [ HP.classes $ H.ClassName <$> [ "mt-4", "has-text-grey", "has-text-centered" ]
        ]
        [ HH.text "Nothing here yet" ]

  renderMembers toHashSet set = HH.ul_ $ toHashSet set # Set.toArray # sort # map (renderMember set)

  renderMember set el =
    HH.li_
      [ HH.div [ HP.class_ $ H.ClassName "center-and-space-between" ]
          [ HH.text el
          , if TwoPhaseSet.member set el then removeButton el else HH.text ""
          ]
      ]

  removeButton el =
    HH.button
      [ HP.classes $ H.ClassName <$> [ "button", "is-small", "is-danger" ]
      , HE.onClick $ remove el
      ]
      [ HH.text "-" ]

  form { currentState, currentInput } =
    HH.div [ HP.classes $ H.ClassName <$> [ "field", "is-horizontal" ] ]
      [ HH.div [ HP.class_ $ H.ClassName "field-body" ]
          [ HH.div [ HP.class_ $ H.ClassName "field" ]
              [ HH.input
                  [ HP.class_ $ H.ClassName "input"
                  , HP.placeholder "New element"
                  , HE.onValueInput (EditorSpecificAction <<< SetCurrentInput)
                  , HP.value currentInput
                  ]
              ]
          , HH.button
              [ HP.classes $ H.ClassName <$> [ "button", "is-link" ]
              , HE.onClick $ submit currentInput
              , HP.disabled $ currentInput == ""
              ]
              [ HH.text "Add" ]
          ]
      ]

  submit input _ = NotifyParentAboutNewState $ TwoPhaseSet.insert input

  remove el _ = NotifyParentAboutNewState $ TwoPhaseSet.remove el

  handleAction = case _ of
    SetView view -> H.modify_ \st -> st { currentView = view }
    SetCurrentInput input -> H.modify_ \st -> st { currentInput = input }
