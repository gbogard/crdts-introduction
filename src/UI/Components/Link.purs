module UI.Components.Link where

import Prelude
import Capability.Navigate (class Navigate, navigate)
import Data.Foldable (for_)
import Data.Lens (Prism', preview, review)
import Data.Route (Route)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

type Props
  = { additionalClasses :: Array (HH.ClassName)
    }

defaultProps :: Props
defaultProps = { additionalClasses: [] }

link_ ::
  forall props act.
  Prism' act Route ->
  Route ->
  Array (HH.HTML props act) ->
  HH.HTML props act
link_ = link defaultProps

link ::
  forall props act.
  Props ->
  Prism' act Route ->
  Route ->
  Array (HH.HTML props act) ->
  HH.HTML props act
link { additionalClasses } _route route =
  HH.a
    [ HE.onClick \_ -> review _route route
    , HP.classes additionalClasses
    ]

handleAction ::
  forall st act slots output m.
  Navigate m =>
  Prism' act Route ->
  act ->
  H.HalogenM st act slots output m Unit
handleAction _route act = for_ (preview _route act) navigate
