module Main where

import Debug.Trace

import Control.Monad.Eff

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as H
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Events as T
import qualified Thermite.Action as T
import qualified Thermite.Types as T

type State = {count :: Number, locked :: Boolean}

data Action = Increment | Decrement | Reset | Lock

initialState :: State
initialState = {count: 0, locked: false}

render :: T.Render _ State _ Action
render ctx s _ _ =
  H.div (A.className "container")
    [ H.h1' [ T.text "Exercises" ],
      H.ol'
        [ H.li' [ T.text "Modify the state to include an integer count." ],
          H.li' [ T.text "Add a label below to display the current state." ],
          H.li' [ T.text "Add a button to increment the count" ],
          H.li' [ T.text "Add a button to reset the count" ]
        ],
      H.p' [
        T.text "Count: ",
        T.text $ show s.count
      ],
      H.button (T.onClick ctx (const Increment)) [T.text "Increment Count"],
      H.button (T.onClick ctx (const Decrement)) [T.text "Decrement Count"],
      H.button (T.onClick ctx (const Reset)) [T.text "Reset Count"],
      H.button (T.onClick ctx (const Lock)) [T.text (if s.locked then "Unlock" else "Lock")]
    ]


performAction :: T.PerformAction _ State _ Action
performAction _ Increment = T.modifyState incrementCount
performAction _ Decrement = T.modifyState decrementCount
performAction _ Reset     = T.modifyState resetCount
performAction _ Lock      = T.modifyState toggleLocked
performAction s _ = pure s

incrementCount :: State -> State
incrementCount = modifyCount (+1)

decrementCount :: State -> State
decrementCount = modifyCount (\x -> x - 1)

resetCount :: State -> State
resetCount = modifyCount (const 0)

modifyCount :: (Number -> Number) -> State -> State
modifyCount f s
  | s.locked  = s
  | otherwise = {count: f s.count, locked: s.locked}

toggleLocked :: State -> State
toggleLocked s = {count: s.count, locked: not s.locked}

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render

main = do
  let component = T.createClass spec
  T.render component unit
