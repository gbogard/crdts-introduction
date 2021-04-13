module UI.Pages.HomePage (homePage) where

import Prelude
import Capability.Navigate (class Navigate)
import Data.Const (Const)
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Data.Route (Route(..), DemoType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import UI.Components.Layout (layout)
import UI.Components.Link as Link
import Html.Renderer.Halogen as RH
import UI.Pages.Static as Static

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
    [ HH.div [ HP.class_ $ H.ClassName "centered-container" ]
        [ HH.img
            [ HP.src Static.firstSlide
            ]
        , RH.render_ Static.readme
        ]
    ]

handleAction :: forall m. Navigate m => Action -> H.HalogenM Unit Action () Void m Unit
handleAction = Link.handleAction _route
