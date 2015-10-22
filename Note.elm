module Note (Model, view, Action, update, init, Context) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Signal exposing (Signal, Address)
import String
import Window
import Json.Decode as Json

-- MODEL

type alias Model = 
    { body : String
    , editing : Bool
    , id : Int
    }

init : String -> Int -> Model
init body id = 
    { body = ""
    , editing = False
    , id = id
    }

-- UPDATE

type Action 
        = EditingNote Bool
        | UpdateNote Int String

update : Action -> Model -> Model
update action model =
    case action of
        EditingNote isEditing ->
          { model | editing <- isEditing }

        UpdateNote id body ->
           { model | body <- body }          



-- VIEW

type alias Context =
    { actions : Signal.Address Action
    , remove : Signal.Address ()
    }

view : Context -> Model -> Html
view context model = 
    div [ classList [ ("note col-sm-3", True ) , ("editing", model.editing) ]
        , onDoubleClick context.actions (EditingNote True)
        ] 
        [ p[] [ text model.body ]
        , span [] 
            [ button [ onClick context.actions (EditingNote True)
                     , class "btn btn-primary glyphicon glyphicon-pencil" 
                     , style[("margin", "2px")]
                     ]
                     []
            , button [ onClick context.remove ()
                     , class "btn btn-danger glyphicon glyphicon-trash"
                     , style[("margin", "2px")] 
                     ] 
                     []
            ]
        , input
            [ class "edit form-control"
            , value model.body
            , name "title"
            , id ("todo-" ++ toString model.id)
            , on "input" targetValue (Signal.message context.actions << UpdateNote model.id)
            , onBlur context.actions (EditingNote False)
            , onEnter context.actions (EditingNote False)
            ]
            []
        ]

noteStyle : Attribute
noteStyle =
    style 
        [ ("height", "150px")
        , ("width", "150px")
        , ("background-color", "yellow")
        , ("margin", "2px 2px")
        , ("cursor", "-webkit-grab")
        , ("-webkit-box-shadow", "5px 5px 15px 0 rgba(0, 0, 0, .2)")
        , ("box-shadow", "5px 5px 15px 0 rgba(0, 0, 0, .2)")
        ]

onEnter : Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"



-- INPUTS

