module UI.Render where

import Prelude
import Data.HashMap as HashMap
import Data.HashSet as HashSet
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

class Render t where
  render :: forall w i. t -> HH.HTML w i

instance hashMapRender :: (Show k, Render v) => Render (HashMap.HashMap k v) where
  render map = HH.table [ HP.classes [ HH.ClassName "table" ] ] [ HH.tbody_ children ]
    where
    children = HashMap.toArrayBy renderChild map

    renderChild k v =
      HH.tr_
        [ HH.th_ [ HH.text <<< show $ k ]
        , HH.td_ [ render v ]
        ]

instance hashSetRender :: Render v => Render (HashSet.HashSet v) where
  render set = HH.table [ HP.classes [ HH.ClassName "table" ] ] [ HH.tbody_ children ]
    where
    children = render <$> HashSet.toArray set
