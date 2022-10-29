module AppDecoder exposing (..)

import AppModels exposing (..)
import Json.Decode as Decode exposing (Decoder, bool, field, int, list, nullable, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


decodeTelegramChannel : Decoder TelegramChannel
decodeTelegramChannel =
    Decode.succeed TelegramChannel
        |> required "title" string
        |> required "about" string
        |> required "username" string
        |> required "isVerified" bool
        |> required "isFake" bool
        |> required "isScam" bool
        |> required "totalMember" int
        |> required "totalMessage" (nullable int)
        |> required "photoId" string
        |> required "onlineMember" (nullable int)
        |> required "totalAdmin" (nullable int)


encodeNewTelegramChannel : NewTelegramChannel -> Encode.Value
encodeNewTelegramChannel new_telegram_channel =
    Encode.object
        [ ( "username", Encode.string new_telegram_channel.username )
        , ( "tags", Encode.list Encode.string new_telegram_channel.tags )
        ]


decodeLoadTelegramChannelError : Decoder LoadTelegramChannelError
decodeLoadTelegramChannelError =
    Decode.map2 LoadTelegramChannelError
        (field "error-code" string)
        (field "error-message" string)


decodeTelegramChannelData : Decoder TelegramChannelData
decodeTelegramChannelData =
    Decode.map4 TelegramChannelData
        (field "id" int)
        (field "username" string)
        (field "tags" (list string))
        (field "created_at" string)


decodeListTelegramChannelData : Decode.Value -> Result Decode.Error (List TelegramChannelData)
decodeListTelegramChannelData =
    Decode.decodeValue (list decodeTelegramChannelData)
