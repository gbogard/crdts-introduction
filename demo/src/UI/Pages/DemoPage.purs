module UI.Pages.DemoPage where

import Prelude
import Capability.Navigate (class Navigate)
import Data.Const (Const)
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Data.Route (DemoType(..), Route)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))
import UI.Components.Layout (layout)
import UI.Components.Link as Link
import UI.Components.StateBasedCRDT as StateBasedCRDT
import UI.Components.StateBasedCRDT.GSet as GSetDemo
import UI.Components.StateBasedCRDT.TwoPhaseSet as TwoPhaseSetDemo
import UI.Components.StateBasedCRDT.GCounter as GCounterDemo

type Slots
  = ( demo :: H.Slot (Const Void) Void Unit )

data Action
  = Navigate Route

-- | Builds an action from a route
_route :: Prism' Action Route
_route =
  prism' Navigate case _ of
    Navigate route -> Just route

page :: forall m. Navigate m => DemoType -> H.Component (Const Void) Unit Void m
page demoType =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  render :: Unit -> HH.ComponentHTML Action Slots m
  render _ =
    layout _route
      [ HH.slot_ (Proxy :: Proxy "demo") unit demo unit
      ]

  demo = case demoType of
    GSet ->
      StateBasedCRDT.mkDemo
        { title: "GSet - Grow-only Set"
        , initialState: mempty
        , editor: GSetDemo.editor
        }
    TwoPhaseSet ->
      StateBasedCRDT.mkDemo
        { title: "2PSet - Two-phase Set"
        , initialState: mempty
        , editor: TwoPhaseSetDemo.editor
        }
    GCounter ->
      StateBasedCRDT.mkDemo
        { title: "GCounter - Grow-only Counter"
        , initialState: mempty
        , editor: GCounterDemo.editor
        }

  handleAction :: Action -> H.HalogenM Unit Action Slots Void m Unit
  handleAction = Link.handleAction _route
