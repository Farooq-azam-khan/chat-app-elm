port module ChatPage exposing (..)

import Html exposing (..)
import Html.Attributes exposing (autocomplete, class, href, id, placeholder, required, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Types exposing (LoginForm)


type alias MessageRecord =
    { username : String, text : String, time : String }


type alias UserRecord =
    { id : String, room : String, username : String }


type alias Model =
    { messages : List MessageRecord
    , message : String
    , room_users : List UserRecord
    }


type Msg
    = SendMessage
    | UpdateMsg String
    | ReceiveUsersFromServer (List UserRecord)
    | ReceiveMessageFromServer MessageRecord


port sendMessagetoSocket : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveMessageFromServer the_msg ->
            let
                add_msg =
                    List.append [ the_msg ] model.messages
            in
            ( { model | messages = add_msg }, Cmd.none )

        SendMessage ->
            ( { model | message = "" }
            , sendMessagetoSocket model.message
            )

        UpdateMsg the_msg ->
            ( { model | message = the_msg }, Cmd.none )

        ReceiveUsersFromServer room_users ->
            ( { model | room_users = room_users }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "chat-container" ]
        [ header [ class "chat-header" ]
            [ h1 [] [ text "chatapp" ]
            , a
                [ href "/", class "btn", id "leave-btn" ]
                [ text "Leave Room" ]
            ]
        , main_ [ class "chat-main" ]
            [ div [ class "chat-sidebar" ]
                [ h2 [ id "room-name" ] []
                , h3 [] [ text "Online" ]
                , ul [ id "users" ] (List.map (\user -> li [] [ text user.username ]) model.room_users)
                ]
            , div [ class "chat-messages" ]
                [ ul []
                    (List.map
                        (\msg ->
                            li []
                                [ text <| msg.username ++ " said: " ++ msg.text ++ " (at " ++ msg.time ++ ")" ]
                        )
                        model.messages
                    )
                ]
            ]
        , div [ class "chat-form-container" ]
            [ div [ id "chat-form" ]
                [ input
                    [ id "msg"
                    , type_ "text"
                    , placeholder "Type a Message"
                    , required True
                    , autocomplete False
                    , value model.message
                    , onInput UpdateMsg
                    ]
                    []
                , button [ class "btn-plane", Html.Events.onClick SendMessage ] [ text "send" ]
                ]
            ]
        ]



-- TODO


type alias RoomUserAlias =
    { id : String, username : String, room : String }


port receiveUsers : (List UserRecord -> msg) -> Sub msg


port receiveMessage : (MessageRecord -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveUsers ReceiveUsersFromServer
        , receiveMessage ReceiveMessageFromServer
        ]


init : LoginForm -> Model
init _ =
    { message = "", room_users = [], messages = [] }
