module App where

import Board exposing (update, view, init)
import StartApp.Simple exposing (start)


main =
  start
    { model = init
    , update = update
    , view = view
    }