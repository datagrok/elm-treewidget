import Html exposing (..)
import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (type', placeholder, value)
import DictTree
import Dict
import String

main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { path : String
  , tree : DictTree.DictTree String
  }

init = (Model "" DictTree.empty, Cmd.none)

type Msg
  = Update String
  | Submit

update msg model =
  case msg of
    Update p ->
      ({ model | path = p }, Cmd.none)
    Submit ->
      let
        path = String.split "/" model.path
      in
        ({ model | tree = DictTree.addPath path model.tree }, Cmd.none)

subscriptions model =
  Sub.none

view model =
  div [] <| List.append (viewdicttree model.tree)
    [form [onSubmit Submit]
      [ input [ type' "text", placeholder "foo/bar/baz", onInput Update ] []
      , input [ type' "submit", value "Add" ] []
      ]
    ]

viewdicttree dtn = 
  if Dict.isEmpty <| DictTree.root dtn then
    []
  else
    [ul [] <| List.map (\(k,v) -> li [] (text k :: viewdicttree v)) <| Dict.toList <| DictTree.root dtn]


-- vim: set ts=2 sw=2 et:
