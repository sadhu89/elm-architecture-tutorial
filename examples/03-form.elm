import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Char exposing (..)

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  }


model : Model
model =
  Model "" "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Age age ->
      { model | age = age }


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , input [ type_ "text", placeholder "Age", onInput Age ] []
    , viewValidation model
    ]


viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      if model.password /= model.passwordAgain then
        ("red", "Passwords do not match!")
      else if String.length model.password < 8 then
        ("red", "Password should be at least 8 characters.")
      else if String.any Char.isDigit model.password == False ||
              String.any Char.isUpper model.password == False ||
              String.any Char.isLower model.password == False then
        ("red", "Password should contains upper case, lower case, and numeric characters.")
      else if Result.withDefault -1 (String.toInt model.age) < 0  then
        ("red", "Age should be a positive number.")
      else
        ("green", "OK")

  in
    div [ style [("color", color)] ] [ text message ]
