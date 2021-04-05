module UI.Components.StateBasedCRDT
  ( mkDemo
  , EditorInput(..)
  , EditorOutput(..)
  , Editor
  , Query
  , mkEditor
  , EditorAction(..)
  , ParentOutput(..)
  ) where

import Prelude

import Data.Array (intersperse, last, sort, sortBy)
import Data.CRDT (class StateBasedCRDT, merge)
import Data.Const (Const)
import Data.Foldable (fold, foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.HashMap as Map
import Data.Maybe (Maybe(..))
import Data.Replication (ReplicaId(..))
import Data.Replication.VectorClock (VectorClock(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Halogen (defaultEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type DemoState t
  = { replicas :: Map.HashMap ReplicaId t }

-- Parent component
----------------------------------------------------------------------------------------------------
data Action t
  = Sync ReplicaId ReplicaId
  | SyncAll
  | AddReplica
  | RemoveReplica ReplicaId
  | ModifyReplica ReplicaId (t -> t)

type Query :: forall k. k -> Type
type Query
  = Const Void

type ParentInput
  = Unit

type ParentOutput
  = Void

type DemoSpec t m
  = { title :: String
    , editor :: Editor t m
    , initialState :: t
    }

type ParentSlots t
  = ( "editor" :: H.Slot Query (EditorOutput t) ReplicaId )

mkDemo ::
  forall t m.
  Eq t =>
  StateBasedCRDT t =>
  DemoSpec t m -> H.Component Query ParentInput ParentOutput m
mkDemo spec =
  H.mkComponent
    { initialState: const $ defaultDemoState spec.initialState
    , render: render spec
    , eval: H.mkEval defaultEval { handleAction = handleAction }
    }
  where
  render { title, editor } state@{ replicas } =
    HH.div_
      [ controlBar title state
      , HH.div [ HP.class_ $ H.ClassName "editors-container" ]
          (Map.values $ mapWithIndex (editorEl state editor) replicas)
      ]

  controlBar title st@{ replicas } =
    HH.nav [ HP.class_ $ H.ClassName "control-bar" ]
      [ HH.h3_ [ HH.text title ]
      , HH.div_
          [ roundButton [ HE.onClick \_ -> AddReplica ] [ HH.text "+" ]
          , HH.text <<< show <<< Map.size $ replicas
          , removeReplicaBtn st
          , HH.button
              [ HP.classes $ H.ClassName <$> [ "button", "is-link", "is-rounded", "is-small" ]
              , HE.onClick \_ -> SyncAll
              ]
              [ HH.text "Sync everyhting" ]
          ]
      ]

  editorEl state editor rid replica =
    HH.div [ HP.classes $ H.ClassName <$> [ "column", "is-one-quarter-desktop", "is-one-third-tablet" ] ]
      [ HH.div [ HP.class_ $ H.ClassName "box" ]
          [ HH.h5
              [ HP.classes $ H.ClassName <$> [ "title", "is-6", "has-text-centered" ] ]
              [ HH.text $ "Replica " <> show rid ]
          , HH.slot (Proxy :: Proxy "editor") rid editor (EditorInput rid replica) (adaptEditorOutput rid)
          , HH.br_
          , HH.div [ HP.classes $ H.ClassName <$> [ "center-and-space-between", "my-1" ] ]
              [ HH.text "Sync with"
              , syncButtons state rid
              ]
          ]
      ]

  syncButtons { replicas } currentReplicaId =
    HH.div [ HP.classes $ H.ClassName <$> [ "buttons", "has-addons" ] ]
      ( replicas
          # Map.delete currentReplicaId
          # Map.keys
          # sort
          # map (syncButton currentReplicaId)
      )

  syncButton currentReplicaId rid =
    HH.button
      [ HP.class_ $ H.ClassName "button"
      , HE.onClick \_ -> Sync currentReplicaId rid
      ]
      [ HH.text (show rid) ]

  removeReplicaBtn st@{ replicas } = case lastReplica st of
    Just rid
      | Map.size replicas > 1 -> roundButton [ HE.onClick \_ -> RemoveReplica rid ] [ HH.text "-" ]
    _ -> roundButton [ HP.disabled true ] [ HH.text "-" ]

  roundButton props = HH.button $ props <> [ HP.classes $ H.ClassName <$> [ "button is-rounded is-small is-link" ] ]

  handleAction ::
    Action t -> H.HalogenM (DemoState t) (Action t) (ParentSlots t) ParentOutput m Unit
  handleAction = case _ of
    AddReplica -> H.modify_ $ addReplica spec.initialState
    RemoveReplica rid -> H.modify_ $ removeReplica rid
    ModifyReplica rid newState -> H.modify_ $ modifyReplica rid newState
    Sync a b -> H.modify_ $ sync a b
    SyncAll -> H.modify_ syncAll

-- Editor
----------------------------------------------------------------------------------------------------
type Viewer t
  = t -> forall w i. HH.HTML w i

data EditorInput t
  = EditorInput ReplicaId t

newtype EditorOutput t
  = ModifiedState (t -> t)

data EditorAction act t
  = ReceiveParentInput ReplicaId t
  | NotifyParentAboutNewState (t -> t)
  | EditorSpecificAction act

type Editor t m
  = H.Component Query (EditorInput t) (EditorOutput t) m

type EditorSpec state act t m
  = { initialState :: ReplicaId -> t -> state
    , updateState :: ReplicaId -> t -> state -> state
    , render :: state -> HH.ComponentHTML (EditorAction act t) () m
    , handleAction :: act -> H.HalogenM state (EditorAction act t) () (EditorOutput t) m Unit
    }

adaptEditorOutput :: forall t. ReplicaId -> EditorOutput t -> Action t
adaptEditorOutput rid (ModifiedState t) = ModifyReplica rid t

mkEditor :: forall state act t m. StateBasedCRDT t => EditorSpec state act t m -> Editor t m
mkEditor spec =
  H.mkComponent
    { initialState
    , render: spec.render
    , eval:
        H.mkEval
          H.defaultEval
            { receive = receive
            , handleAction = handleAction
            }
    }
  where
  initialState (EditorInput rid t) = spec.initialState rid t

  receive (EditorInput rid t) = Just $ ReceiveParentInput rid t

  handleAction = case _ of
    ReceiveParentInput rid t -> H.modify_ $ spec.updateState rid t
    NotifyParentAboutNewState fn -> H.raise $ ModifiedState fn
    EditorSpecificAction act -> spec.handleAction act

  -- Partials
  ----------------------------------------------------------------------------------------------------
  renderClock :: forall w i. VectorClock -> HH.HTML w i
  renderClock (VectorClock components)
    | Map.isEmpty components = HH.text ""

  renderClock (VectorClock components) =
    HH.div [ HP.class_ $ H.ClassName "clock" ]
      $ [ HH.strong_ [ HH.text "Clock: " ] ]
      <> ( components
            # Map.toArrayBy Tuple
            # sortBy sortByKey
            # map renderClockComponent
            # intersperse (HH.text " / ")
        )
    where
    sortByKey (a /\ _) (b /\ _) = compare a b

    renderClockComponent (rid /\ time) = HH.text $ show rid <> ":" <> show time

-- Utility functions
----------------------------------------------------------------------------------------------------
maxReplicas :: Int
maxReplicas = 4

defaultDemoState :: forall t. t -> DemoState t
defaultDemoState initialState = { replicas: _ } <<< Map.fromArray <<< map makePair $ [ 1, 2 ]
  where
  makePair i = ReplicaId i /\ initialState

addReplica :: forall t. t -> DemoState t -> DemoState t
addReplica initialState st@{ replicas } =
  let
    currentSize = Map.size replicas

    nextReplicaId = ReplicaId $ currentSize + 1

    newDemoState = st { replicas = Map.insert nextReplicaId initialState replicas }
  in
    if currentSize < maxReplicas then newDemoState else st

-- | Given two replica ids, updates the state of the second replica by merging it with the state
-- | of the first replica.
sync ::
  forall t.
  Eq t =>
  StateBasedCRDT t =>
  ReplicaId -> ReplicaId -> DemoState t -> DemoState t
sync sourceId targetId st@{ replicas } = case merge <$> lookup' sourceId <*> lookup' targetId of
  Just mergedState -> st { replicas = Map.insert targetId mergedState replicas }
  _ -> st
  where
  lookup' = lookup st

syncAll :: forall t. Eq t => StateBasedCRDT t => DemoState t -> DemoState t
syncAll st @ {replicas} = st { replicas = replicas $> fold replicas }

-- | Modifies the state of a specific replica using  a function, returns an updated 'DemoState'
modifyReplica :: forall t. ReplicaId -> (t -> t) -> DemoState t -> DemoState t
modifyReplica replicaId fn st@{ replicas } = st { replicas = Map.update (Just <<< fn) replicaId replicas }

lookup :: forall t. DemoState t -> ReplicaId -> Maybe t
lookup { replicas } rid = Map.lookup rid replicas

removeReplica :: forall t. ReplicaId -> DemoState t -> DemoState t
removeReplica id st@{ replicas } = st { replicas = Map.delete id replicas }

lastReplica :: forall t. DemoState t -> Maybe ReplicaId
lastReplica { replicas } = last <<< sort <<< Map.keys $ replicas
