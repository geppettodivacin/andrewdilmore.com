module Aliases exposing (Album, Collection)


type alias Album =
    { title : String
    , slug : String
    , thumbnailUrl : String
    }


type alias Collection =
    { title : String
    , slug : String
    , albums : List Album
    , heightAdjustment : Int
    }
