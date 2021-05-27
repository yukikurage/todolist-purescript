module Main where

import Data.Functor
import Data.Array (cons, deleteAt, length, modifyAt, zipWith, (..), (:))
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind, map, pure, unit, ($), (-), (<<<), (=<<))

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type Task
  = { name :: String
    , isFinished :: Boolean
    }

type State
  = { newTaskName :: String
    , tasks :: Maybe (Array Task)
    }

data Action
  = AddTask
  | SetTaskName String
  | DeleteTask Int
  | FinishTask Int

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ =
  { newTaskName: ""
  , tasks: pure []
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.div_ [ HH.text "タスク一覧" ]
    , HH.div_ <<< map (renderTask) <<< withItr <<< fromMaybe [] $ state.tasks
    , HH.div_
        [ HH.label_
            [ HH.text "タスク名"
            , HH.input
                [ HP.value state.newTaskName
                , HE.onValueInput \str -> SetTaskName str
                ]
            ]
        , HH.button [ HE.onClick \_ -> AddTask ] [ HH.text "追加" ]
        ]
    ]

renderTask :: forall m. { itr :: Int, val :: Task } -> H.ComponentHTML Action () m
renderTask taskItr =
  HH.div
    [ HP.class_ (HH.ClassName "task") ]
    $ HH.text taskItr.val.name
    : if taskItr.val.isFinished then
        [ HH.button
            [ HE.onClick \_ -> DeleteTask taskItr.itr ]
            [ HH.text "削除" ]
        ]
      else
        [ HH.button
            [ HE.onClick \_ -> FinishTask taskItr.itr ]
            [ HH.text "完了" ]
        ]

withItr :: forall a. Array a -> Array { itr :: Int, val :: a }
withItr xs = zipWith { itr: _, val: _ } (0 .. (length xs - 1)) xs

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  AddTask -> H.modify_ \state -> { tasks: cons { name: state.newTaskName, isFinished: false } <$> state.tasks, newTaskName: "" }
  (SetTaskName str) -> H.modify_ _ { newTaskName = str }
  (DeleteTask itr) -> H.modify_ \state -> state { tasks = deleteAt itr =<< state.tasks }
  (FinishTask itr) ->
    H.modify_ \state ->
      state { tasks = modifyAt itr (_ { isFinished = true }) =<< state.tasks }
