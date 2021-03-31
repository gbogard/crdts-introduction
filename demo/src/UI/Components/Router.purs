module UI.Components.Router where

import Prelude
import Capability.Navigate (class Navigate)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Route (DemoType(..), Route(..))
import Halogen as H
import Halogen.HTML as HH
import Routing.Match (Match, lit, root, end)
import Type.Proxy (Proxy(..))
import UI.Pages.HomePage as HomePage

type State
  = Maybe Route

data Query :: forall k. k -> Type
data Query t
  = Navigate Route

type Slots
  = HomePage.WithHomePage ()

matchDemoType :: Match DemoType
matchDemoType = oneOf [ GSet <$ lit "gset" ]

matchRoute :: Match Route
matchRoute = root *> oneOf [ HomePage <$ end, Demo <$> (lit "demo" *> matchDemoType <* end) ]

component :: forall input output m. Navigate m => H.Component Query input output m
component =
  H.mkComponent
    { initialState: \_ -> Nothing
    , render
    , eval: H.mkEval H.defaultEval { handleQuery = handleQuery }
    }

render :: forall m action. Navigate m => Maybe Route -> H.ComponentHTML action Slots m
render = case _ of
  Nothing -> HH.text "not found"
  Just HomePage -> HH.slot_ (Proxy :: Proxy "homepage") unit HomePage.homePage unit
  (Just (Demo GSet)) -> HH.text "gset demo"

handleQuery ::
  forall t m action output.
  Navigate m =>
  Query t -> H.HalogenM State action Slots output m (Maybe t)
handleQuery = case _ of
  Navigate route -> do
    H.modify_ (\_ -> Just route) $> Nothing
