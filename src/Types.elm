module Types exposing (ChatRoom(..), LoginForm)


type ChatRoom
    = JustCats
    | Business
    | Sports
    | Technology
    | InternationalNews


type alias LoginForm =
    { email : String, username : String, chatRoom : ChatRoom }
