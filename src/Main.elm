port module Main exposing (..)

import AppDecoder exposing (..)
import AppModels exposing (..)
import Browser exposing (Document)
import Browser.Navigation as Nav
import FanaProtocol exposing (requestFanaProtocol)
import Html exposing (button, div, h1, header, img, input, label, nav, p, span, text)
import Html.Attributes exposing (alt, class, disabled, for, href, name, placeholder, src, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Icons
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData)
import Set
import Url exposing (Url)



-- MAIN


main : Program String Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = \_ -> NoOp
        , onUrlRequest = \_ -> NoOp
        }



-- PORTS


port sign_in_with_google : () -> Cmd msg


port sign_in_with_github : () -> Cmd msg


port sign_out : () -> Cmd msg


port create_a_new_telegram_channel : Encode.Value -> Cmd msg


port load_telegram_channels : () -> Cmd msg


port user_signed_in : (() -> msg) -> Sub msg


port user_signed_out : (() -> msg) -> Sub msg


port get_telegram_channels : (Decode.Value -> msg) -> Sub msg


type alias Model =
    { apiToken : String
    , pageData : PageData
    }


type PageData
    = SignedIn
        { newTelegramChannel : NewTelegramChannel
        , channelPreview : RemoteData LoadTelegramChannelError TelegramChannel
        , feed : List ( TelegramChannelData, TelegramChannel )
        }
    | SignedOut


init : String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init apiToken _ _ =
    ( { apiToken = apiToken
      , pageData = SignedOut
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | SignInWithGoogle
    | SignInWithGitHub
    | SignOut
    | UserSignedIn ()
    | UserSignedOut ()
    | UpdateNewTelegram NewTelegramChannel
    | LoadChannelPreview (RemoteData LoadTelegramChannelError TelegramChannel)
    | Save
    | GetTelegramChannels Decode.Value
    | LoadChannelsFeed ( TelegramChannelData, TelegramChannel )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SignInWithGoogle ->
            ( model, sign_in_with_google () )

        SignInWithGitHub ->
            ( model, sign_in_with_github () )

        SignOut ->
            ( model, sign_out () )

        UserSignedIn () ->
            ( { model
                | pageData =
                    SignedIn
                        { newTelegramChannel = initNewTelegramChannel
                        , feed = []
                        , channelPreview = RemoteData.NotAsked
                        }
              }
            , load_telegram_channels ()
            )

        UserSignedOut () ->
            ( { model | pageData = SignedOut }, Cmd.none )

        UpdateNewTelegram t ->
            case model.pageData of
                SignedIn m ->
                    ( { model | pageData = SignedIn { m | newTelegramChannel = t } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoadChannelPreview res ->
            case model.pageData of
                SignedIn m ->
                    case res of
                        RemoteData.Success _ ->
                            let
                                newTelegram =
                                    { username = m.newTelegramChannel.username
                                    , tags = m.newTelegramChannel.tags |> List.map String.trim |> List.filter ((/=) "") |> Set.fromList |> Set.toList
                                    }
                            in
                            ( { model | pageData = SignedIn { m | channelPreview = res } }
                            , create_a_new_telegram_channel (encodeNewTelegramChannel newTelegram)
                            )

                        _ ->
                            ( { model | pageData = SignedIn { m | channelPreview = res } }, Cmd.none )

                SignedOut ->
                    ( model, Cmd.none )

        Save ->
            case model.pageData of
                SignedIn m ->
                    ( { model | pageData = SignedIn { m | channelPreview = RemoteData.Loading } }, loadTelegramChannel model.apiToken m.newTelegramChannel.username )

                SignedOut ->
                    ( model, Cmd.none )

        GetTelegramChannels json ->
            case decodeListTelegramChannelData json of
                Ok data ->
                    ( model, loadChannelsFeed model.apiToken data )

                Err _ ->
                    ( model, Cmd.none )

        LoadChannelsFeed channel ->
            case model.pageData of
                SignedIn m ->
                    ( { model | pageData = SignedIn { m | feed = channel :: m.feed } }, Cmd.none )

                SignedOut ->
                    ( model, Cmd.none )


updateNewTelegramUsername : NewTelegramChannel -> String -> Msg
updateNewTelegramUsername newTelegramChannel newUsername =
    UpdateNewTelegram { newTelegramChannel | username = newUsername }


updateNewTelegramTags : NewTelegramChannel -> String -> Msg
updateNewTelegramTags newTelegramChannel tags =
    UpdateNewTelegram { newTelegramChannel | tags = tags |> String.split "," }


loadChannelsFeed : String -> List TelegramChannelData -> Cmd Msg
loadChannelsFeed apiToken datas =
    datas |> List.map (loadTelegramChannelData apiToken) |> Cmd.batch


loadTelegramChannelData : String -> TelegramChannelData -> Cmd Msg
loadTelegramChannelData apiToken data =
    requestFanaProtocol apiToken
        data.username
        (\res ->
            case res of
                Ok (RemoteData.Success info) ->
                    LoadChannelsFeed ( data, info )

                _ ->
                    NoOp
        )


loadTelegramChannel : String -> String -> Cmd Msg
loadTelegramChannel apiToken username =
    requestFanaProtocol apiToken
        username
        (\res ->
            case res of
                Ok remote_data ->
                    LoadChannelPreview remote_data

                Err _ ->
                    NoOp
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ user_signed_in UserSignedIn
        , user_signed_out UserSignedOut
        , get_telegram_channels GetTelegramChannels
        ]


view : Model -> Document Msg
view model =
    { title = "Katarogu"
    , body =
        [ div []
            (case model.pageData of
                SignedIn { newTelegramChannel, channelPreview, feed } ->
                    [ div []
                        [ nav [ class "bg-gray-900 p-4 h-16 flex justify-between items-center" ]
                            [ img [ src "/favicon.svg", class "h-10 w-auto" ] []
                            , button
                                [ class "bg-indigo-600 text-white px-3 py-2 rounded-md text-sm font-medium shadow-sm shadow-indigo-900"
                                , onClick SignOut
                                ]
                                [ text "Sign Out" ]
                            ]
                        , div [ class "p-4 mt-6 grid grid-cols-1 gap-y-10 gap-x-6 sm:grid-cols-2 lg:grid-cols-3 xl:gap-x-8" ]
                            (viewNewTelegramChannelInput newTelegramChannel channelPreview
                                :: (feed |> List.map viewTelegramChannel)
                            )
                        ]
                    ]

                SignedOut ->
                    [ div [ class "min-h-screen flex items-center justify-center py-12 px-2 sm:px-6 lg:px-8" ]
                        [ div [ class "w-full max-w-md space-y-8 p-2" ]
                            [ img [ class "mx-auto h-24 w-auto", src "/favicon.svg", alt "Logo" ]
                                []
                            , header []
                                [ h1
                                    [ class "mt-6 text-center text-3xl font-bold tracking-tight text-white" ]
                                    [ text "Katarogu" ]
                                , p
                                    [ class "text-center text-white" ]
                                    [ text "A crowdsourced catalog of telegram channels" ]
                                ]
                            , div [ class "flex flex-col gap-2" ]
                                [ signInButton { msg = SignInWithGoogle, icon = Icons.google, label = "Sign in with Google" }
                                , signInButton { msg = SignInWithGitHub, icon = Icons.github, label = "Sign in with GitHub" }
                                ]
                            ]
                        ]
                    ]
            )
        ]
    }


textInput : { msg : String -> msg, name : String, label : String, placeholder : String, value : String } -> Html.Html msg
textInput opts =
    div []
        [ input
            [ class "bg-slate-900 p-2 my-2 block w-full rounded-md border border-gray-300 text-white"
            , type_ "text"
            , name opts.name
            , placeholder opts.placeholder
            , value opts.value
            , onInput opts.msg
            ]
            []
        , label [ for opts.name, class "block text-sm font-light text-white" ] [ text opts.label ]
        ]


viewNewTelegramChannelInput : NewTelegramChannel -> RemoteData LoadTelegramChannelError TelegramChannel -> Html.Html Msg
viewNewTelegramChannelInput newTelegramChannel channelPreview =
    div [ class "px-4 py-8 bg-slate-900 rounded-md grid grid-cols-1 gap-4 shadow-lg" ]
        [ h1 [ class "text-white text-2xl" ] [ text "Add A New Telegram Channel" ]
        , viewTelegramChannelPreview channelPreview
        , textInput
            { msg = updateNewTelegramUsername newTelegramChannel
            , name = "telegram_channel_username"
            , label = "telegram channel username"
            , value = newTelegramChannel.username
            , placeholder = "e.g. @codenight_info"
            }
        , div []
            [ div [] (newTelegramChannel.tags |> List.map String.trim |> List.filter ((/=) "") |> List.map viewTag)
            , textInput
                { msg = updateNewTelegramTags newTelegramChannel
                , name = "telegram_channel_tags"
                , label = "tags that describe the channel"
                , value = String.join "," newTelegramChannel.tags
                , placeholder = "e.g. programming,python,java"
                }
            ]
        , button
            [ class "bg-indigo-600 text-white px-3 py-2 rounded-md text-sm font-medium active:scale-95 disabled:bg-gray-300"
            , onClick Save
            , disabled (RemoteData.isLoading channelPreview)
            ]
            [ text
                (case channelPreview of
                    RemoteData.Loading ->
                        "Saving..."

                    _ ->
                        "Save"
                )
            ]
        ]


viewTelegramChannelPreview : RemoteData LoadTelegramChannelError TelegramChannel -> Html.Html msg
viewTelegramChannelPreview data =
    div [ class "my-1 p-2 rounded-md text-white shadow-sm shadow-indigo-400 none-if-empty" ]
        (case data of
            RemoteData.NotAsked ->
                []

            RemoteData.Loading ->
                [ div [] [ text "Validating..." ] ]

            RemoteData.Failure { error } ->
                [ h1 [ class "text-red-600" ]
                    [ text
                        (case error of
                            "E-105" ->
                                "Channel does not exist"

                            "E-106" ->
                                "Username is not a channel"

                            _ ->
                                "Something Went Wrong"
                        )
                    ]
                ]

            RemoteData.Success { title, about } ->
                [ h1 [ class "text-indigo-600 font-bold" ] [ text title ]
                , p [ class "my-2 font-light" ] [ text about ]
                ]
        )


viewTelegramChannel : ( TelegramChannelData, TelegramChannel ) -> Html.Html msg
viewTelegramChannel ( data, info ) =
    div [ class "p-4 bg-blue-700 text-white bg-opacity-40 rounded-md shadow-lg" ]
        [ div []
            [ if info.isVerified then
                viewTagWithClass (class "bg-green-600 text-white") "verified"

              else
                text ""
            , if info.isFake then
                viewTagWithClass (class "bg-red-600 text-white") "fake"

              else
                text ""
            , if info.isScam then
                viewTagWithClass (class "bg-purple-600 text-white") "scam"

              else
                text ""
            ]
        , h1 [ class "font-bold text-2xl" ] [ text ("@" ++ info.username) ]
        , h1 [ class "font-bold text-md" ] [ text info.title ]
        , p [ class "my-2" ] [ text info.about ]
        , div [ class "mt-6" ]
            (data.tags |> List.map viewTag)
        , div [ class "inline-flex items-center gap-1 px-3 py-0.5 rounded-full text-sm font-medium bg-slate-900 text-white mr-3" ]
            [ Icons.users, text (String.fromInt info.totalMembers) ]
        , Html.a
            [ class "flex justify-between mt-6 w-full bg-indigo-600 text-white px-3 py-2 rounded-md text-sm font-medium active:scale-95 disabled:bg-gray-300"
            , href ("https://t.me/" ++ info.username)
            , target "_blank"
            ]
            [ text "Open"
            , Icons.arrowUpRight
            ]
        ]


viewTag : String -> Html.Html msg
viewTag t =
    span
        [ class "inline-flex items-center px-3 py-0.5 rounded-full text-sm font-medium bg-indigo-100 text-indigo-800 mr-2 mb-2"
        ]
        [ text t ]


viewTagWithClass : Html.Attribute msg -> String -> Html.Html msg
viewTagWithClass c t =
    span
        [ class "inline-flex items-center px-3 py-0.5 rounded-full text-sm font-medium mr-2 mb-2"
        , c
        ]
        [ text t ]


signInButton : { msg : msg, icon : Html.Html msg, label : String } -> Html.Html msg
signInButton { msg, icon, label } =
    button
        [ onClick msg
        , class "group relative flex w-full justify-center rounded-md border border-transparent bg-indigo-600 py-2 px-4 text-sm font-medium text-white hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2"
        ]
        [ span [ class "absolute inset-y-0 left-0 flex items-center pl-3" ] [ icon ], text label ]
