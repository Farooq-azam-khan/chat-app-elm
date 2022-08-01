module ChatPage exposing (..)

import Html exposing (..)
import Html.Attributes exposing (autocomplete, class, href, id, placeholder, required, type_)
import Types exposing (LoginForm)


type alias Model =
    {}


view : Model -> Html msg
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
                , ul [ id "users" ] []
                ]
            , div [ class "chat-messages" ] []
            ]
        , div [ class "chat-form-container" ]
            [ form [ id "chat-form" ]
                [ input
                    [ id "msg"
                    , type_ "text"
                    , placeholder "Type a Message"
                    , required True
                    , autocomplete False
                    ]
                    []
                , button [ class "btn-plane" ] [ text "send" ]
                ]
            ]
        ]


init : LoginForm -> Model
init _ =
    {}
