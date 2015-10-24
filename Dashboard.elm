module Dashboard where

import Board
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Signal, Address)

-- MODELS

type alias Model =
    { boards : List (ID, Board.Model)
    , nextID : Int
    }

type alias ID = Int

init : Model
init =
    { boards = []
    , nextID = 0
    }

--UPDATES

type Action 
        = Insert 


update : Action -> Model -> Model
update action model =
    case action of 
        Insert -> 
            { model | 
                boards <- Board.init :: model.boards,
                nextID <- model.nextID + 1  
            }

--VIEWS

view : Signal.Address Action -> Model -> List Html
view address model =
    let insert = div [ class "row" ]
                     [ button [ class "btn btn-success glyphicon glyphicon-plus"
                              , onClick address Insert
                              ]
                              []
                     ]
    in 
        (insert :: List.map (viewBoard address) model.boards)


viewBoard : Signal.Address Action ->  (ID, Board.Model) -> Html
viewBoard address (id, model) =
     let context = Board.Context
     in
      Board.view context model




            

