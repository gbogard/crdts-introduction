module UI.Components.Layout where

import Data.Lens
import Prelude
import Data.Route (DemoType(..), Route(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import UI.Components.Link as Link

navbar :: forall props act. Prism' act Route -> HH.HTML props act
navbar _route =
  HH.nav
    [ HP.classes $ HH.ClassName <$> [ "navbar", "is-primary" ]
    , ARIA.role "navigation"
    , ARIA.label "main navigation"
    ]
    [ navbarBrand [ navbarTitle _route HomePage [ HH.text "A gentle introduction to CRDTs" ] ]
    , navbarMenu
        [ menu "State-based CRDTs"
            [ navbarLink _route (Demo GSet) [ HH.text "GSet" ]
            , navbarLink _route (Demo GCounter) [ HH.text "GCounter" ]
            , navbarLink _route (Demo TwoPhaseSet) [ HH.text "Two-Phase Set" ]
            ]
        , menu "Operation-based CRDTs" []
        , HH.a [ HP.class_ $ HH.ClassName "navbar-item", HP.href "" ] [ HH.text "Slides" ]
        ]
        [ HH.a
            [ HP.classes $ HH.ClassName <$> [ "navbar-item", "has-text-weight-semibold" ]
            , HP.href "https://github.com/gbogard/crdts-introduction"
            ]
            [ HH.text "Github"
            ]
        ]
    ]
  where
  navbarBrand = HH.div [ HP.class_ $ HH.ClassName "navbar-brand" ]

  navbarLink = Link.link { additionalClasses: [ HH.ClassName "navbar-item" ] }

  navbarMenu start end =
    HH.div [ HP.class_ $ HH.ClassName "navbar-menu" ]
      [ HH.div [ HP.class_ $ HH.ClassName "navbar-start" ] start
      , HH.div [ HP.class_ $ HH.ClassName "navbar-end" ] end
      ]

  menu name children =
    HH.div [ HP.classes $ HH.ClassName <$> [ "navbar-item", "has-dropdown", "is-hoverable" ] ]
      [ HH.a [ HP.class_ $ HH.ClassName "navbar-link" ] [ HH.text name ]
      , HH.div [ HP.class_ $ HH.ClassName "navbar-dropdown" ] children
      ]

  navbarTitle =
    Link.link
      { additionalClasses: HH.ClassName <$> [ "navbar-item", "title", "is-6" ] }

layout :: forall props act. Prism' act Route -> Array (HH.HTML props act) -> HH.HTML props act
layout _route children = HH.div_ $ [ navbar _route ] <> children
