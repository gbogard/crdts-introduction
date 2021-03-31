module UI.Components.Layout where

import Data.Lens
import Prelude
import Data.Route (Route(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import UI.Components.Link as Link

navbar :: forall props act. Prism' act Route -> HH.HTML props act
navbar _route =
  HH.nav
    [ HP.classes [ HH.ClassName "navbar" ]
    , ARIA.role "navigation"
    , ARIA.label "main navigation"
    ]
    [ navbarBrand [ navbarTitle _route HomePage [ HH.text "CRDTs for mortals" ] ]
    , navbarLink _route HomePage [ HH.text "Slides" ]
    ]
  where
  navbarBrand = HH.div [ HP.classes [ HH.ClassName "navbar-brand" ] ]

  navbarLink = Link.link { additionalClasses: [ HH.ClassName "navbar-item" ] }

  navbarTitle =
    Link.link
      { additionalClasses:
          [ HH.ClassName "navbar-item", HH.ClassName "title", HH.ClassName "is-6"
          ]
      }

layout :: forall props act. Prism' act Route -> Array (HH.HTML props act) -> HH.HTML props act
layout _route children = HH.div_ $ [ navbar _route, container children ]
  where
  container = HH.div [ HP.classes [ HH.ClassName "box" ] ]
