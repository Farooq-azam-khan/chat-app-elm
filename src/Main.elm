port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import ChatPage
import Html exposing (..)
import Html.Attributes exposing (alt, autocomplete, class, for, href, id, name, placeholder, required, src, target, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Encode as Encode
import Types exposing (..)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), Parser)


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map HomeR Parser.top
        , Parser.map ChatR (Parser.s "chat")
        ]


type Route
    = HomeR
    | ChatR


port join_room : Encode.Value -> Cmd msg


encode_login_form : LoginForm -> Encode.Value
encode_login_form login_form =
    Encode.object
        [ ( "username", Encode.string login_form.username )
        , ( "email", Encode.string login_form.email )
        , ( "chatRoom", Encode.string <| chatRoomToString login_form.chatRoom )
        ]


type alias Model =
    { login_form : LoginForm
    , page : Page
    , key : Nav.Key
    }


type Page
    = HomePage
    | ChatPage ChatPage.Model


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | LoginMsg LoginForm
    | UpdateEmail String
    | UpdateUsername String
    | UpdateSelection String
    | ChatPageMessages ChatPage.Msg


chatRoomToString : ChatRoom -> String
chatRoomToString chat_room =
    case chat_room of
        JustCats ->
            "JustCats"

        Business ->
            "Business"

        Sports ->
            "Sports"

        Technology ->
            "Technology"

        InternationalNews ->
            "International News"


login_form_view : LoginForm -> Html Msg
login_form_view login_form =
    form [ onSubmit <| LoginMsg login_form ]
        [ div [ class "form-control" ]
            [ label [ for "email" ] [ text "Email Address" ]
            , input
                [ type_ "email"
                , name "email"
                , id "email"
                , placeholder "Enter email Address..."
                , required True
                , value login_form.email
                , onInput UpdateEmail
                ]
                []
            ]
        , div [ class "from-control" ]
            [ label [ for "username" ] [ text "Username" ]
            , input
                [ type_ "text"
                , name "username"
                , id "username"
                , placeholder "Enter username"
                , required True
                , value login_form.username
                , onInput UpdateUsername
                ]
                []
            ]
        , div [ class "form-control" ]
            [ label [ for "room" ] [ text "Chat Channel" ]
            , select
                [ name "room"
                , id "room"
                , value <| chatRoomToString login_form.chatRoom
                , onInput UpdateSelection
                ]
                [ option [ value "Business" ] [ text "Business" ]
                , option [ value "Technology" ] [ text "Technology" ]
                , option [ value "Sport" ] [ text "Sports" ]
                , option [ value "news" ] [ text "International News" ]
                , option [ value "cats" ] [ text "Just Cats" ]
                ]
            ]
        , div [ class "" ]
            [ button
                [ type_ "submit", class "btn" ]
                [ text "Login to Chat" ]
            ]
        ]


string_to_chat_room : String -> ChatRoom
string_to_chat_room s =
    case s of
        "Business" ->
            Business

        "Technology" ->
            Technology

        "Sport" ->
            Sports

        "news" ->
            InternationalNews

        "cats" ->
            JustCats

        _ ->
            JustCats


view : Model -> Browser.Document Msg
view model =
    let
        page_view =
            case model.page of
                HomePage ->
                    [ div
                        [ class "join-contianer" ]
                        [ header
                            [ class "join-header" ]
                            [ h1 [] [ text "Chat App" ] ]
                        , main_
                            [ class "join-main" ]
                            [ login_form_view model.login_form ]
                        ]
                    ]

                ChatPage chat_model ->
                    [ ChatPage.view chat_model |> Html.map ChatPageMessages ]
    in
    { title = "Chat App"
    , body = page_view
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoginMsg login_form ->
            -- http://localhost:3000/chat.html?email=asdf%40gmailc.om&username=asdf&room=Business
            let
                -- "/chat" ++ "/" ++ "email=" ++ login_form.email ++ "&username=" ++ login_form.username ++ "&room=" ++ chatRoomToString login_form.chatRoom
                format_url =
                    Builder.absolute
                        [ "chat" ]
                        [ Builder.string "email" login_form.email
                        , Builder.string "username" login_form.username
                        , Builder.string "room" (chatRoomToString login_form.chatRoom)
                        ]

                cmds =
                    Cmd.batch
                        [ Nav.pushUrl model.key format_url
                        , join_room (encode_login_form login_form)
                        ]
            in
            ( { model | page = ChatPage <| ChatPage.init login_form }, cmds )

        UpdateEmail new_email ->
            let
                model_login_form =
                    model.login_form

                updated_login_form =
                    { model_login_form | email = new_email }
            in
            ( { model | login_form = updated_login_form }, Cmd.none )

        UpdateUsername new_username ->
            let
                model_login_form =
                    model.login_form

                updated_login_form =
                    { model_login_form | username = new_username }
            in
            ( { model | login_form = updated_login_form }, Cmd.none )

        UpdateSelection selection_str ->
            let
                model_login_form =
                    model.login_form

                updated_login_form =
                    { model_login_form | chatRoom = string_to_chat_room selection_str }
            in
            ( { model | login_form = updated_login_form }, Cmd.none )

        ChangedUrl url ->
            case Parser.parse parser url of
                Just HomeR ->
                    ( { model | page = HomePage, login_form = init_login_form }, Cmd.none )

                Just ChatR ->
                    ( { model | page = ChatPage (ChatPage.init init_login_form) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChatPageMessages chat_pg_msg ->
            case model.page of
                ChatPage chat_model ->
                    toChatPage model (ChatPage.update chat_pg_msg chat_model)

                _ ->
                    ( model, Cmd.none )


toChatPage : Model -> ( ChatPage.Model, Cmd ChatPage.Msg ) -> ( Model, Cmd Msg )
toChatPage model ( chat_model, cmd ) =
    ( { model | page = ChatPage chat_model }
    , Cmd.map ChatPageMessages cmd
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        ChatPage chat_model ->
            ChatPage.subscriptions chat_model |> Sub.map ChatPageMessages

        _ ->
            Sub.none


init_login_form : LoginForm
init_login_form =
    { email = "", username = "", chatRoom = JustCats }


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        model =
            case Parser.parse parser url of
                Just HomeR ->
                    { login_form = init_login_form
                    , page = HomePage
                    , key = key
                    }

                Just ChatR ->
                    { login_form = init_login_form
                    , page = ChatPage <| ChatPage.init init_login_form
                    , key = key
                    }

                Nothing ->
                    { login_form = init_login_form
                    , page = HomePage
                    , key = key
                    }
    in
    ( model
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }
