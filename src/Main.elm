module Main exposing (main)

import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Background as Background
import Http
import Json.Decode as JD
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

type alias Model =
    { ingredients : IngredientList
    , basket : List Ingredient
    }

initModel : Model
initModel =
  { basket = []
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
  | SelectIngredient Ingredient

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotIngredients result ->
      case result of
        Err _ -> (model, Cmd.none)
        Ok ingredientsList ->
          ({ model | ingredients = ingredientsList }, Cmd.none)

    SelectIngredient ingredient ->
      let
        newIngredientList = updateIngredientList ingredient model.ingredients
      in
      ({ model
        | ingredients = newIngredientList
        , basket = List.filter (\x -> x.quantity > 0) newIngredientList.viandes ++ List.filter (\x -> x.quantity > 0) newIngredientList.legumes ++ List.filter (\x -> x.quantity > 0) newIngredientList.supplements ++ List.filter (\x -> x.quantity > 0) newIngredientList.sauces
      }, Cmd.none)




-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW

viewIngredient : Ingredient -> Element Msg
viewIngredient ingredient =
  let
    selectAttributes = [ Background.color <| rgb255 117 179 201 ]
  in
    Input.button (if ingredient.quantity > 0 then selectAttributes else [])
      { onPress = Just <| SelectIngredient ingredient
      , label = text <| ingredient.name ++ " x" ++ fromInt ingredient.quantity
      }

viewList : String -> List Ingredient -> Element Msg
viewList name mList =
  column [ height fill, width fill ]
    [ el [ Font.bold ] <| text name
    , column [] <| List.map viewIngredient mList
    ]

view : Model -> Html Msg
view model =
  layout [] <|
    row [ height fill, width fill ]
      [ column [ height fill, width <| fillPortion 1 ]
        [ viewList "Viandes" model.ingredients.viandes
        , viewList "Légumes" model.ingredients.legumes
        , viewList "Suppléments" model.ingredients.supplements
        , viewList "Sauces" model.ingredients.sauces
        ]
      , column [ height <| fillPortion 1, width <| fillPortion 1 ]
        [ el [ Font.bold ] <| text "Votre commande"
        , column [ height fill, width fill ] <|
          List.map (\ingredient -> el [] <| text ingredient.name) model.basket
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
