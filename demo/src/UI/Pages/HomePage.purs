module UI.Pages.HomePage (homePage) where

import Prelude
import Capability.Navigate (class Navigate)
import Data.Const (Const)
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Data.Route (Route(..), DemoType(..))
import Halogen as H
import Halogen.HTML as HH
import UI.Components.Layout (layout)
import UI.Components.Link as Link

data Action
  = Navigate Route

-- | Builds an action from a route
_route :: Prism' Action Route
_route =
  prism' Navigate case _ of
    Navigate route -> Just route

homePage :: forall m. Navigate m => H.Component (Const Void) Unit Void m
homePage =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall m. Navigate m => Unit -> HH.ComponentHTML Action () m
render _ =
  layout _route
    [ HH.h1_ [ HH.text "CRDT Demo" ]
    , Link.link_ _route HomePage [ HH.text "Home" ]
    , HH.br_
    , Link.link_ _route (Demo GSet) [ HH.text "GSet demo" ]
    ]

handleAction :: forall m. Navigate m => Action -> H.HalogenM Unit Action () Void m Unit
handleAction = Link.handleAction _route
