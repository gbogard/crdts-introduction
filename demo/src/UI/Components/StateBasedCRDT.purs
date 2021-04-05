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
import Data.Array (last, sort)
import Data.CRDT (class StateBasedCRDT, WithTimestamp, mergeStates)
import Data.Const (Const)
import Data.FunctorWithIndex (mapWithIndex)
import Data.HashMap as Map
import Data.Maybe (Maybe(..))
import Data.Replication (ReplicaId(..))
import Data.Replication.VectorClock (increment)
import Data.Tuple.Nested ((/\))
import Halogen (defaultEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type DemoState t
  = { replicas :: Map.HashMap ReplicaId (ReplicaState t) }

type ReplicaState t
  = WithTimestamp t

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
          (Map.values $ mapWithIndex (editorEl editor) replicas)
      ]

  controlBar title st@{ replicas } =
    HH.nav [ HP.class_ $ H.ClassName "control-bar" ]
      [ HH.h3_ [ HH.text title ]
      , HH.div_
          [ roundButton [ HE.onClick \_ -> AddReplica ] [ HH.text "+" ]
          , HH.text <<< show <<< Map.size $ replicas
          , removeReplicaBtn st
          ]
      ]

  editorEl editor rid replica =
    HH.div [ HP.classes $ H.ClassName <$> [ "column", "is-one-quarter-desktop", "is-one-third-tablet" ] ]
      [ HH.div [ HP.class_ $ H.ClassName "box" ]
          [ HH.h5
              [ HP.classes $ H.ClassName <$> [ "title", "is-6", "has-text-centered" ] ]
              [ HH.text $ "Replica " <> show rid ]
          , HH.slot (Proxy :: Proxy "editor") rid editor (EditorInput replica.data) (adaptEditorOutput rid)
          ]
      ]

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
    _ -> pure unit

-- Editor
----------------------------------------------------------------------------------------------------
type Viewer t
  = t -> forall w i. HH.HTML w i

newtype EditorInput t
  = EditorInput t

newtype EditorOutput t
  = ModifiedState (t -> t)

data EditorAction act t
  = ReceiveParentInput t
  | NotifyParentAboutNewState (t -> t)
  | EditorSpecificAction act

type Editor t m
  = H.Component Query (EditorInput t) (EditorOutput t) m

type EditorSpec state act t m
  = { initialState :: t -> state
    , updateState :: t -> state -> state
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
  initialState (EditorInput t) = spec.initialState t

  receive (EditorInput t) = Just $ ReceiveParentInput t

  handleAction = case _ of
    ReceiveParentInput t -> H.modify_ $ spec.updateState t
    NotifyParentAboutNewState fn -> H.raise $ ModifiedState fn
    EditorSpecificAction act -> spec.handleAction act

-- Utility functions
----------------------------------------------------------------------------------------------------
maxReplicas :: Int
maxReplicas = 4

defaultReplicaState :: forall t. t -> ReplicaState t
defaultReplicaState initialState = { data: initialState, timestamp: mempty }

defaultDemoState :: forall t. t -> DemoState t
defaultDemoState initialState = { replicas: _ } <<< Map.fromArray <<< map makePair $ [ 1, 2 ]
  where
  makePair i = ReplicaId i /\ defaultReplicaState initialState

addReplica :: forall t. t -> DemoState t -> DemoState t
addReplica initialState st@{ replicas } =
  let
    currentSize = Map.size replicas

    nextReplicaId = ReplicaId $ currentSize + 1

    newReplicaState = defaultReplicaState initialState

    newDemoState = st { replicas = Map.insert nextReplicaId newReplicaState replicas }
  in
    if currentSize < maxReplicas then newDemoState else st

sync ::
  forall t.
  StateBasedCRDT t =>
  DemoState t ->
  ReplicaId -> ReplicaId -> DemoState t
sync st@{ replicas } sourceId targetId = case (/\) <$> lookup' sourceId <*> lookup' targetId of
  Just (sourceState /\ targetState) ->
    let
      replicas' = Map.insert targetId (mergeStates sourceState targetState) replicas
    in
      st { replicas = replicas' }
  Nothing -> st
  where
  lookup' = lookup st

modifyReplica :: forall t. ReplicaId -> (t -> t) -> DemoState t -> DemoState t
modifyReplica replicaId fn st@{ replicas } = st { replicas = Map.update updateAndIncTime replicaId replicas }
  where
  updateAndIncTime state = Just $ { data: fn state.data, timestamp: increment state.timestamp replicaId }

lookup :: forall t. DemoState t -> ReplicaId -> Maybe (ReplicaState t)
lookup { replicas } rid = Map.lookup rid replicas

removeReplica :: forall t. ReplicaId -> DemoState t -> DemoState t
removeReplica id st@{ replicas } = st { replicas = Map.delete id replicas }

lastReplica :: forall t. DemoState t -> Maybe ReplicaId
lastReplica { replicas } = last <<< sort <<< Map.keys $ replicas
