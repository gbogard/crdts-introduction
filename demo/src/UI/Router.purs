module UI.Router where

import Prelude
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Route (DemoType(..), Route(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Routing.Match (Match, lit, root, end)

type State
  = Maybe Route

data Query :: forall k. k -> Type
data Query t
  = Navigate Route

matchDemoType :: Match DemoType
matchDemoType = oneOf [ GSet <$ lit "gset" ]

matchRoute :: Match Route
matchRoute = root *> oneOf [ HomePage <$ end, Demo <$> (lit "demo" *> matchDemoType <* end) ]

component :: forall input output m. MonadEffect m => H.Component Query input output m
component =
  H.mkComponent
    { initialState: \_ -> Nothing
    , render
    , eval: H.mkEval H.defaultEval { handleQuery = handleQuery }
    }

render :: forall m action. Maybe Route -> H.ComponentHTML action () m
render = case _ of
  Nothing -> HH.text "not found"
  Just HomePage -> HH.text "home"
  (Just (Demo GSet)) -> HH.text "gset demo"

handleQuery :: forall t m action output. MonadEffect m => Query t -> H.HalogenM State action () output m (Maybe t)
handleQuery = case _ of
  Navigate route -> do
    liftEffect <<< log <<< show $ route
    H.modify_ (\_ -> Just route) $> Nothing
