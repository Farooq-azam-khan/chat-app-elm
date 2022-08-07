port module ChatPage exposing (..)

import Html exposing (..)
import Html.Attributes exposing (autocomplete, class, href, id, placeholder, required, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Types exposing (LoginForm)
import Url exposing (Url)


type alias MessageRecord =
    { username : String, text : String, time : String }


type alias UserRecord =
    { id : String, room : String, username : String }


type alias Model =
    { messages : List MessageRecord
    , message : String
    , room_users : List UserRecord
    , logged_in_user : LoggedInUser

    --     , room_name : String
    }


type alias LoggedInUser =
    { username : String }


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
    div [ class "mb-10" ]
        [ a
            [ href "/"
            , class "px-3 py-2 rounded-md bg-red-100 text-red-900 hover:bg-red-900 hover:text-red-100"
            , id "leave-btn"
            ]
            [ text "Leave Room" ]
        , div [ class "mt-5" ]
            [ div [ class "chat-sidebar" ]
                [ h2 [ id "room-name" ] []
                , h3 [] [ text "Online" ]
                , ul [ id "users" ] (List.map (\user -> li [] [ text user.username ]) model.room_users)
                ]
            , div [ class "chat-messages" ]
                [ ul [ class "space-y-3" ]
                    (List.map
                        (\msg ->
                            li [ class "" ]
                                [ chat_bubble model.logged_in_user msg.username msg.text ]
                         -- ++ msg.time ++ ")" ]
                        )
                        model.messages
                    )
                ]
            ]
        , div [ class "mt-3 " ]
            [ div [ id "w-full flex items-center" ]
                [ input
                    [ id "msg"
                    , class "w-4/5 bg-gray-100 rounded-l-md border border-indigo-300 px-3 py-2"
                    , type_ "text"
                    , placeholder "Type a Message"
                    , required True
                    , autocomplete False
                    , value model.message
                    , onInput UpdateMsg
                    ]
                    []
                , button
                    [ class "btn-plane"
                    , Html.Events.onClick SendMessage
                    , class "bg-indigo-800 w-1/5 text-white px-3 py-2 hover:bg-indigo-900 rounded-r-md"
                    ]
                    [ text "send" ]
                ]
            ]
        ]


chat_bubble : LoggedInUser -> String -> String -> Html Msg
chat_bubble logged_in_user username message =
    if logged_in_user.username == username then
        div
            [ class "flex flex-col items-end space-y-2 text-gray-900  px-3 py-2 bg-indigo-300 rounded-lg" ]
            [ div [ class "text-sm font-bold" ] [ text username ]
            , div [] [ text message ]
            ]

    else
        div
            [ class "flex flex-col space-y-2 text-gray-900  px-3 py-2 bg-gray-300 rounded-lg" ]
            [ div [ class "text-sm font-bold" ] [ text username ]
            , div [] [ text message ]
            ]


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
init login_form =
    { logged_in_user = { username = login_form.username }
    , message = ""
    , room_users = []
    , messages = []
    }
