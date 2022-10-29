module FanaProtocol exposing (requestFanaProtocol)

import AppDecoder exposing (decodeLoadTelegramChannelError, decodeTelegramChannel)
import AppModels exposing (LoadTelegramChannelError, TelegramChannel, initLoadTelegramChannelError)
import Http exposing (Expect, Response)
import Json.Decode exposing (Decoder, decodeString)
import RemoteData exposing (RemoteData(..))


requestFanaProtocol : String -> String -> (Result error (RemoteData LoadTelegramChannelError TelegramChannel) -> msg) -> Cmd msg
requestFanaProtocol apiToken username toMsg =
    Http.request
        { method = "GET"
        , headers = [ Http.header "token" apiToken ]
        , url = "https://fanaprotocol.herokuapp.com/api/v1/channel/get_info?username=" ++ username
        , body = Http.emptyBody
        , expect =
            expectRemoteData
                toMsg
                decodeTelegramChannel
                decodeLoadTelegramChannelError
        , timeout = Nothing
        , tracker = Nothing
        }


expectRemoteData : (Result error (RemoteData LoadTelegramChannelError TelegramChannel) -> msg) -> Decoder TelegramChannel -> Decoder LoadTelegramChannelError -> Expect msg
expectRemoteData toMsg ok_decoder err_decoder =
    Http.expectStringResponse toMsg (toResultWithRemoteData ok_decoder err_decoder)


toResultWithRemoteData : Decoder TelegramChannel -> Decoder LoadTelegramChannelError -> Response String -> Result error (RemoteData LoadTelegramChannelError TelegramChannel)
toResultWithRemoteData okDecoder errDecoder response =
    case response of
        Http.BadStatus_ _ body ->
            case decodeString errDecoder body of
                Ok value ->
                    Failure value |> Ok

                Err _ ->
                    Failure initLoadTelegramChannelError |> Ok

        Http.GoodStatus_ _ body ->
            case decodeString okDecoder body of
                Ok value ->
                    Success value |> Ok

                Err _ ->
                    Failure initLoadTelegramChannelError |> Ok

        _ ->
            Failure initLoadTelegramChannelError |> Ok
