module AppModels exposing (..)

-- Loaded from Fana protocol api


type alias TelegramChannel =
    { title : String
    , about : String
    , username : String
    , isVerified : Bool
    , isFake : Bool
    , isScam : Bool
    , totalMembers : Int
    , totalMessage : Maybe Int --
    , photoId : String --
    , online_member : Maybe Int --
    , totalAdmin : Maybe Int --
    }



-- Error from Fana protocol api


type alias LoadTelegramChannelError =
    { error : String
    , errorDescription : String
    }


initLoadTelegramChannelError : LoadTelegramChannelError
initLoadTelegramChannelError =
    { error = ""
    , errorDescription = ""
    }



-- Created from user input


type alias NewTelegramChannel =
    { username : String
    , tags : List String
    }


initNewTelegramChannel : NewTelegramChannel
initNewTelegramChannel =
    { username = "", tags = [] }



-- Loaded from supabase


type alias TelegramChannelData =
    { id : Int
    , username : String
    , tags : List String
    , createdAt : String
    }
