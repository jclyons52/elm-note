module Board (Model, view, Action, update, init, Context) where

import Note
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MODEL

type alias Model = 
    { notes : List ( ID, Note.Model )
    , nextID : ID
    }

type alias ID = Int

init : Model
init = 
    { notes = []
    , nextID = 0
    }

-- UPDATE
type Action 
        = Insert
        | Remove ID
        | Modify ID Note.Action

update : Action -> Model -> Model
update action model =
    case action of
        Insert -> 
            { model |
                notes <- ( model.nextID, Note.init "add your text" model.nextID) :: model.notes,
                nextID <- model.nextID + 1
            } 

        Remove id -> 
            { model |
                notes <- List.filter (\(noteID, _) -> noteID /= id) model.notes
            }

        Modify id noteAction ->
            let updateNote (noteID, noteModel) =
                if noteID == id
                    then (noteID, Note.update noteAction noteModel)
                    else (noteID, noteModel)
            in
                { model | notes <- List.map updateNote model.notes }



-- VIEW

type alias Context =
    { actions : Signal.Address Action
    }

view : Context -> Model -> Html
view context model = 
    let insert = button [ class "btn btn-sm btn-success glyphicon glyphicon-plus"
                        , onClick context.actions Insert 
                        , addStyles
                        ]
                        []

    in 
        div [ class "board" ] (insert :: List.map (viewNote context.actions) model.notes)

addStyles : Attribute
addStyles =
    style
        [ ("position", "fixed")
        , ("top", "10px")
        , ("right", "10px")
        ]

viewNote : Signal.Address Action -> (ID, Note.Model) -> Html
viewNote address (id, model) =
    let context =
        Note.Context
            (Signal.forwardTo address (Modify id))
            (Signal.forwardTo address (always (Remove id)))
    in
        Note.view context model


