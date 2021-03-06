module UI.Main where

import Prelude
import UI.AppM (Env, runAppM)
import Data.Route (Route)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.PushState (makeInterface, matches)
import UI.Components.Router as Router

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    routingInterface <- liftEffect makeInterface
    let
      env :: Env
      env = { routingInterface }

      rootComponent = H.hoist (runAppM env) Router.component
    halogenIO <- runUI rootComponent {} body
    let
      routeListener :: Maybe Route -> Route -> Effect Unit
      routeListener old new = launchAff_ $ halogenIO.query $ Router.Navigate new
    void <<< liftEffect $ matches Router.matchRoute routeListener routingInterface
