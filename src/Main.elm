module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Background as Background
import Http
import Json.Decode as JD
import Json.Encode as JE
import String exposing (fromInt)



-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type IngredientType
  = Viande
  | Legume
  | Supplement
  | Sauce
  | None

type alias Ingredient =
  { name : String
  , quantity : Int
  , iType : IngredientType
  }

type alias IngredientList =
  { viandes : List Ingredient
  , legumes : List Ingredient
  , supplements : List Ingredient
  , sauces : List Ingredient
  }

type alias Tacos =
  { ingredients : List Ingredient
  , name : String
  }

type alias Model =
    { ingredients : IngredientList
    , tacos : Tacos
    , commands : List Tacos
    }

initModel : Model
initModel =
  { commands = []
  , tacos =
    { ingredients = []
    , name = ""
    }
  , ingredients =
    { viandes = []
    , legumes = []
    , supplements = []
    , sauces = []
    }
  }

init : () -> (Model, Cmd Msg)
init _ =
    (initModel, getIngredientsList)



-- UPDATE

type Msg
  = GotIngredients (Result Http.Error IngredientList)
  | GotCommands (Result Http.Error (List Tacos))
  | GetCommands
  | TacosPosted (Result Http.Error ())
  | SelectIngredient Ingredient
  | UpdateTacosName String
  | EnterWasPressed

updateIngredient : Ingredient -> Ingredient -> Ingredient
updateIngredient wantedIngredient ingredient =
  if wantedIngredient.name == ingredient.name then
    { ingredient | quantity = if ingredient.quantity == 0 then 1 else 0 }
  else
    ingredient

updateIngredientList : Ingredient -> IngredientList -> IngredientList
updateIngredientList ingredient ingredientList =
  case ingredient.iType of
    Viande ->
      { ingredientList
      | viandes = List.map
        (updateIngredient ingredient)
        ingredientList.viandes
      }

    Legume ->
      { ingredientList
      | legumes = List.map
        (updateIngredient ingredient)
        ingredientList.legumes
      }

    Supplement ->
      { ingredientList
      | supplements = List.map
        (updateIngredient ingredient)
        ingredientList.supplements
      }

    Sauce ->
      { ingredientList
      | sauces = List.map
        (updateIngredient ingredient)
        ingredientList.sauces
      }

    None ->
      ingredientList

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotIngredients result ->
      case result of
        Err _ -> (model, Cmd.none)
        Ok ingredientsList ->
          ({ model | ingredients = ingredientsList }, Cmd.none)

    GotCommands result ->
      case result of
        Err _ -> (model, Cmd.none)
        Ok commands ->
          ({ model | commands = commands }, Cmd.none)

    GetCommands ->
      (model, getCommands)

    TacosPosted _ ->
      (model, Cmd.none)

    SelectIngredient ingredient ->
      let
        newIngredientList = updateIngredientList ingredient model.ingredients
        newTacos =
          { ingredients = List.filter (\x -> x.quantity > 0) newIngredientList.viandes ++ List.filter (\x -> x.quantity > 0) newIngredientList.legumes ++ List.filter (\x -> x.quantity > 0) newIngredientList.supplements ++ List.filter (\x -> x.quantity > 0) newIngredientList.sauces
          , name = model.tacos.name
          }
      in
      ({ model
        | ingredients = newIngredientList
        , tacos = newTacos
      }, Cmd.none)

    UpdateTacosName name ->
      let
        newTacos =
          { ingredients = model.tacos.ingredients
          , name = name
          }
      in
        ({ model | tacos = newTacos }, Cmd.none)

    EnterWasPressed ->
      let
        tacos = model.tacos
        resetIngredients =
          { viandes = List.map (\x -> { x | quantity = 0 }) model.ingredients.viandes
          , legumes = List.map (\x -> { x | quantity = 0 }) model.ingredients.legumes
          , supplements = List.map (\x -> { x | quantity = 0 }) model.ingredients.supplements
          , sauces = List.map (\x -> { x | quantity = 0 }) model.ingredients.sauces
          }
      in
        ({ model
          | tacos =
            { ingredients = []
            , name = ""
            }
          , ingredients = resetIngredients
        }, postTacos tacos)




-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW

onEnter : msg -> Element.Attribute msg
onEnter msg =
  Element.htmlAttribute
    (Html.Events.on "keyup"
      (JD.field "key" JD.string
        |> JD.andThen
          (\key ->
            if key == "Enter" then
              JD.succeed msg

            else
              JD.fail "Not the enter key"
          )
      )
    )

viewIngredientButton : Ingredient -> Element Msg
viewIngredientButton ingredient =
  let
    selectAttributes = [ Background.color <| rgb255 117 179 201 ]
  in
    Input.button (if ingredient.quantity > 0 then selectAttributes else [])
      { onPress = Just <| SelectIngredient ingredient
      , label = text <| ingredient.name
      }

viewList : String -> List (Attribute Msg) -> (a -> Element Msg) -> List a -> Element Msg
viewList name attributes viewFunc mList =
  column attributes
    [ el [ Font.bold ] <| text name
    , column [] <| List.map viewFunc mList
    ]

concatIngredientsInString : String -> List Ingredient -> String
concatIngredientsInString string ingredients =
  case List.head ingredients of
    Nothing -> string
    Just head ->
      if string == "" then
        concatIngredientsInString head.name <| List.drop 1 ingredients
      else
        concatIngredientsInString (string ++ ", " ++ head.name) <| List.drop 1 ingredients

view : Model -> Html Msg
view model =
  layout [] <|
    row [ height fill, width fill ]
      [ column [ height fill, width <| fillPortion 1 ]
        [ viewList "Viandes" [ height fill, width fill ] viewIngredientButton model.ingredients.viandes
        , viewList "Légumes" [ height fill, width fill ] viewIngredientButton model.ingredients.legumes
        , viewList "Suppléments" [ height fill, width fill ] viewIngredientButton model.ingredients.supplements
        , viewList "Sauces" [ height fill, width fill ] viewIngredientButton model.ingredients.sauces
        ]
      , column [ height fill, width <| fillPortion 1 ]
        [ column [ height <| fillPortion 1, width fill ]
          [ viewList "Votre commande" [ width fill ] (\x -> el [] <| text x.name) model.tacos.ingredients
          , Input.text
            [ width (px 500)
            , onEnter EnterWasPressed
            ]
            { onChange = UpdateTacosName
            , text = model.tacos.name
            , placeholder = Just <| Input.placeholder [] (text "Nom du tacos")
            , label = Input.labelHidden "Nom du tacos"
            }
          ]
        , column [ height <| fillPortion 2, width fill ]
          [ viewList "Toutes les commandes" [ width fill ] (\tacos -> el [] <| text <| tacos.name ++ ": " ++ concatIngredientsInString "" tacos.ingredients) model.commands
          , Input.button []
            { onPress = Just GetCommands
            , label = text "Récupérer les commandes"
            }
          ]
        ]
      ]



-- HTTP

ingredientDecoder : IngredientType -> JD.Decoder Ingredient
ingredientDecoder iType =
  JD.map3 Ingredient
    JD.string
    (JD.succeed 0)
    (JD.succeed iType)

ingredientListDecoder : JD.Decoder IngredientList
ingredientListDecoder =
  JD.map4 IngredientList
    (JD.field "viandes" (JD.list <| ingredientDecoder Viande))
    (JD.field "legumes" (JD.list <| ingredientDecoder Legume))
    (JD.field "supplements" (JD.list <| ingredientDecoder Supplement))
    (JD.field "sauces" (JD.list <| ingredientDecoder Sauce))

getIngredientsList : Cmd Msg
getIngredientsList =
  Http.get
    { url = "https://elmchatworkshop.osc-fr1.scalingo.io/ingredients"
    , expect = Http.expectJson GotIngredients ingredientListDecoder
    }

commandsDecoder : JD.Decoder (List Tacos)
commandsDecoder =
  JD.list <|
    JD.map2 Tacos
      (JD.field "ingredients" <| JD.list <| ingredientDecoder None)
      (JD.field "name" JD.string)

getCommands : Cmd Msg
getCommands =
  Http.get
    { url = "https://elmchatworkshop.osc-fr1.scalingo.io/commands"
    , expect = Http.expectJson GotCommands commandsDecoder
    }

ingredientEncoder : Ingredient -> JE.Value
ingredientEncoder ingredient =
  JE.string ingredient.name

tacosEncoder : Tacos -> JE.Value
tacosEncoder tacos =
  JE.object
    [ ("ingredients", JE.list ingredientEncoder tacos.ingredients)
    , ("name", JE.string tacos.name)
    ]

postTacos : Tacos -> Cmd Msg
postTacos tacos =
  Http.post
    { url = "https://elmchatworkshop.osc-fr1.scalingo.io/commands"
    , body = Http.jsonBody <| tacosEncoder tacos
    , expect = Http.expectWhatever TacosPosted
    }
