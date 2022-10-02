module Main exposing (..)

import Browser
import Browser.Events
import Element as E
import Element.Background as EBG
import Element.Border as EB
import Element.Font as EF
import Element.Input as EI
import Html
import Html.Attributes exposing (src)
import Http exposing (Error(..), emptyBody, header, jsonBody, stringBody)
import Json.Decode as JD
import Json.Encode as JE
import Svg as S
import Svg.Attributes as SA


type alias Model =
    { productName : String
    , productThumbnail : String
    , productKcal : Int 
    , productLink : String
    , productFoodId : Int
    , productId : Int
    , productProteins : Int
    , productFat : Int
    , productCarbohydrates : Int
    , poruka : String
    , results : List Product
    , resultProduct : Maybe Product
    , errorMessage : Maybe String
    , loading : Bool
    }


type alias Product =
    { name : String
    , thumbnail : Maybe String
    , link : String
    , proteins : Maybe Int
    , carbohydrates : Maybe Int
    , fat : Maybe Int
    , kcal : Maybe Int
    , id : Int
    }


type Msg
    = MsgGetFood
    | MsgGetProductsFruit
    | MsgGetProductsVegetables
    | MsgGetProductsFastFood
    | MsgGetProductsMilkAndDairy
    | MsgGetProductsMeat
    | MsgGetProductsCandyAndSweets
    | MsgGetProductsDrinksAndBeverages
    | MsgGetProductsOilAndFats
    | MsgGetProductsNutsAndSeeds
    | MsgGetProductsSoups
    | MsgGetProductsMostCaloric
    | MsgGotResults (Result Http.Error (List Product))
    | MsgSuccesfulPost (Result Http.Error ())
    | MsgInputTitleField String
    | MsgInputThumbnailField String
    | MsgInputKcalFieldAsString String
    | MsgInputProteinsField String
    | MsgInputFatField String
    | MsgInputCarbohydratesField String
    | MsgInputLinkField String
    | MsgInputFoodIdFieldAsString String
    | MsgAddProduct
    | MsgDeleteProduct Int
    | MsgInputIdFieldAsString String
    | MsgGotResult (Result Http.Error (Maybe Product))
    | MsgSuccessfulDelete (Result Http.Error ())


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, cmdSearchAll )


initModel : Model
initModel =
    { productName = ""
    , productThumbnail = ""
    , productKcal = 0 
    , productLink = ""
    , productProteins = 0
    , productFat = 0
    , productCarbohydrates = 0
    , productFoodId = 1
    , productId = 1
    , poruka = ""
    , results = []
    , resultProduct = Nothing
    , errorMessage = Nothing
    , loading = False
    }


view : Model -> Html.Html Msg
view model =
    viewLayout model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgInputTitleField newTextInput ->
            ( { model | productName = newTextInput }, Cmd.none )

        MsgInputThumbnailField newThumbnail ->
            ( { model | productThumbnail = newThumbnail }, Cmd.none )

        MsgInputKcalFieldAsString newKcal ->
            ( { model | productKcal = Maybe.withDefault 0 (String.toInt newKcal) }, Cmd.none )

        MsgInputProteinsField newProteins ->
            ( { model | productProteins = Maybe.withDefault 0 (String.toInt newProteins) }, Cmd.none )

        MsgInputFatField newFat ->
            ( { model | productFat = Maybe.withDefault 0 (String.toInt newFat) }, Cmd.none )

        MsgInputCarbohydratesField newCarbohydrates ->
            ( { model | productCarbohydrates = Maybe.withDefault 0 (String.toInt newCarbohydrates) }, Cmd.none )

        MsgInputLinkField newLink ->
            ( { model | productLink = newLink }, Cmd.none )

        MsgInputFoodIdFieldAsString newFoodId ->
            ( { model | productFoodId = Maybe.withDefault 0 (String.toInt newFoodId) }, Cmd.none )

        MsgAddProduct ->
            updateAddProduct model

        MsgDeleteProduct id ->
            updateDeleteProduct model id

        MsgGetFood ->
            updateSve model

        MsgGetProductsFruit ->
            updateFruit model

        MsgGetProductsVegetables ->
            updateVegetables model

        MsgGetProductsFastFood ->
            updateFastFood model

        MsgGetProductsMilkAndDairy ->
            updateMilkAndDairy model

        MsgGetProductsMeat ->
            updateMeat model

        MsgGetProductsCandyAndSweets ->
            updateCandyAndSweets model

        MsgGetProductsDrinksAndBeverages ->
            updateDrinksAndBeverages model

        MsgGetProductsOilAndFats ->
            updateOilAndFats model

        MsgGetProductsNutsAndSeeds ->
            updateNutsAndSeeds model

        MsgGetProductsSoups ->
            updateSoups model

        MsgGetProductsMostCaloric ->
            updateMostCaloric model

        MsgInputIdFieldAsString newId ->
            ( { model | productId = Maybe.withDefault 1 (String.toInt newId) }, Cmd.none )

        MsgSuccesfulPost result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | errorMessage = Nothing }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "404 Not found"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )

        MsgGotResults result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | results = data, errorMessage = Nothing }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "404 Not found"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )

        MsgGotResult result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | resultProduct = data, results = [], errorMessage = Nothing }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "404 Not found"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )

        MsgSuccessfulDelete result ->
            let
                newModel =
                    { model | loading = False }
            in
            case result of
                Ok data ->
                    ( { newModel | errorMessage = Nothing }, Cmd.none )

    
                Err error ->
                    let
                        errorMessage =
                            case error of
                                NetworkError ->
                                    "Network Error"

                                BadUrl _ ->
                                    "Bad URL"

                                Timeout ->
                                    "Timeout"

                                BadStatus _ ->
                                    "404 Not found"

                                BadBody reason ->
                                    reason
                    in
                    ( { newModel | errorMessage = Just errorMessage }, Cmd.none )


updateAddProduct : Model -> ( Model, Cmd Msg )
updateAddProduct model =
    ( { model | loading = True }, postProducts model )


updateDeleteProduct : Model -> Int -> ( Model, Cmd Msg )
updateDeleteProduct model id=
    ( { model | loading = True }, cmdDeleteProduct id )



updateSve : Model -> ( Model, Cmd Msg )
updateSve model =
    ( { model | loading = True }, cmdSearchAll )


updateFruit : Model -> ( Model, Cmd Msg )
updateFruit model =
    ( { model | loading = True }, cmdSearchFruit )


updateVegetables : Model -> ( Model, Cmd Msg )
updateVegetables model =
    ( { model | loading = True }, cmdSearchVegetables )


updateFastFood : Model -> ( Model, Cmd Msg )
updateFastFood model =
    ( { model | loading = True }, cmdSearchFastFood )


updateMilkAndDairy : Model -> ( Model, Cmd Msg )
updateMilkAndDairy model =
    ( { model | loading = True }, cmdSearchMilkAndDairy )


updateMeat : Model -> ( Model, Cmd Msg )
updateMeat model =
    ( { model | loading = True }, cmdSearchMeat )


updateCandyAndSweets : Model -> ( Model, Cmd Msg )
updateCandyAndSweets model =
    ( { model | loading = True }, cmdSearchCandyAndSweets )


updateDrinksAndBeverages : Model -> ( Model, Cmd Msg )
updateDrinksAndBeverages model =
    ( { model | loading = True }, cmdSearchDrinksAndBeverages )


updateOilAndFats : Model -> ( Model, Cmd Msg )
updateOilAndFats model =
    ( { model | loading = True }, cmdSearchOilAndFats )


updateNutsAndSeeds : Model -> ( Model, Cmd Msg )
updateNutsAndSeeds model =
    ( { model | loading = True }, cmdSearchNutsAndSeeds )


updateSoups : Model -> ( Model, Cmd Msg )
updateSoups model =
    ( { model | loading = True }, cmdSearchSoups )


updateMostCaloric : Model -> ( Model, Cmd Msg )
updateMostCaloric model =
    ( { model | loading = True }, cmdSearchMostCaloric )



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

viewLayout : Model -> Html.Html Msg
viewLayout model =
    E.layoutWith
        { options =
            [ E.focusStyle
                { borderColor = Just (E.rgb255 0x00 0x33 0x66)
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [EBG.color (E.rgba255 0xBC 0xBF 0xDF 0.59)]
        (E.column [ E.padding 2 ]
            [ E.row [ E.spacing 1, E.centerX, E.paddingXY 10 15, EF.bold]
            [ E.text "Nutrition Facts" ]
            , viewSearchBar model
            , viewErrorMessage model
            , viewResults model
            , viewResult model
            ]
        )


viewSearchBar : Model -> E.Element Msg
viewSearchBar model =
    E.column []
        [ E.row [ E.spacing 10, E.paddingXY 4 30 ]
            [ EI.search [ E.width (E.px 170) ]
                { onChange = MsgInputTitleField
                , text = model.productName
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Name:")
                }
            , EI.search [  E.width (E.px 190) ]
                { onChange = MsgInputThumbnailField
                , text = model.productThumbnail
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Thumbnail:")
                }
            , EI.search [  E.width (E.px 160) ]
                { onChange = MsgInputKcalFieldAsString
                , text = String.fromInt model.productKcal
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Kcal:")
                }
            ,
            EI.search [  E.width (E.px 160) ]
                { onChange = MsgInputProteinsField
                , text = String.fromInt model.productProteins
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Proteins:")
                }
            ,
            EI.search [  E.width (E.px 180) ]
                { onChange = MsgInputFatField
                , text = String.fromInt model.productFat
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Fat:")
                }
            ,
            EI.search [ E.width (E.px 155) ]
                { onChange = MsgInputCarbohydratesField
                , text = String.fromInt model.productCarbohydrates
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Carbs:")
                }
            ]
        , E.row [ E.spacing 10, E.paddingXY 10 20 ]
            [ EI.search []
                { onChange = MsgInputLinkField
                , text = model.productLink
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Link:")
                }
    
            , EI.search [ E.width (E.px 200) ]
                { onChange = MsgInputFoodIdFieldAsString
                , text = String.fromInt model.productFoodId
                , placeholder = Nothing
                , label = EI.labelLeft [ EF.color (E.rgb255 0x00 0x00 0x00), EF.bold ] (E.text "Food Id:")
                }
            , viewAddProductButton
            ]
        , E.column [] 
            [ E.row [ E.spacing 7, E.paddingXY 4 15 ]
                [ viewGetProductsButton
                , viewGetFruitButton
                , viewGetVegetablesButton
                , viewGetFastFoodButton
                , viewGetMilkProductsButton
                , viewGetMeatButton
                , viewGetCandyAndSweetsButton
                , viewGetDrinksButton
                , viewGetOilAndFatButton
                , viewGetNutsAndSeedsButton
                , viewGetSoupsButton
                , viewGetHighCalorieFoodButton
                , if model.loading then
                    E.html loadingImage

                else
                    E.none
                ]
            ]
        ]

viewM : Model -> Html.Html msg  
viewM model =
    S.svg [src "elm-architecture1.jpg", SA.width "100px", SA.height "100px"] []


loadingImage : Html.Html msg
loadingImage =
    S.svg
        [ SA.width "64px"
        , SA.height "64px"
        , SA.viewBox "0 0 48 48"
        ]
        [ S.circle
            [ SA.cx "24"
            , SA.cy "24"
            , SA.stroke "#6699AA"
            , SA.strokeWidth "4"
            , SA.r "8"
            , SA.fill "none"
            ]
            [ S.animate
                [ SA.attributeName "opacity"
                , SA.values "0;.8;0"
                , SA.dur "2s"
                , SA.repeatCount "indefinite"
                ]
                []
            ]
        ]


viewErrorMessage : Model -> E.Element msg
viewErrorMessage model =
    case model.errorMessage of
        Just errorMessage ->
            E.text errorMessage

        Nothing ->
            E.none


viewResults : Model -> E.Element Msg
viewResults model =
    E.wrappedRow [ E.spacing 17, E.paddingXY 6 40]
        (List.map viewProduct model.results)


viewResult : Model -> E.Element Msg
viewResult model =
    let
        productPlaceholder =
            case model.resultProduct of
                Just product ->
                    viewProduct product

                Nothing ->
                    E.none
    in
    E.wrappedRow [ E.spacing 19, E.paddingXY 0 10 ]
        [ productPlaceholder ]


viewProduct : Product -> E.Element Msg
viewProduct product =
    let 

        titleE =
            E.paragraph [ EF.bold, E.paddingXY 0 3
                , E.mouseOver
                    [ EF.color (E.rgba255 0x00 0x00 0x00 0.65)
                    ] 
                ] [ E.text ("" ++ product.name ++ " - ") ]

        thumbnailE =
            case product.thumbnail of
                Just thumbnail ->
                    viewProductCover thumbnail product.name

                Nothing ->
                    E.none

        kcalE =
            case product.kcal of
                Just kcal ->
                    E.paragraph [ EF.size 19 ]
                        [ E.text ("Calories (100g): " ++ String.fromInt kcal ++ " kcal") ]

                Nothing ->
                    E.none

        proteinsE =
            case product.proteins of
                Just proteins ->
                    E.paragraph [ EF.size 19 ]
                        [ E.text ("(Proteins:" ++ String.fromInt proteins ++ ", ")]

                Nothing ->
                    E.none

        fatE =
            case product.fat of
                Just fat ->
                    E.paragraph [ EF.size 19 ]
                        [ E.text ("Fat:" ++ String.fromInt fat ++ ",\t")]

                Nothing ->
                    E.none

        carbohydratesE = 
            case product.carbohydrates of
                Just carbohydrates ->
                    E.paragraph [EF.size 19]
                        [E.text ("Carbs:" ++ String.fromInt carbohydrates ++ ")")]
                
                Nothing ->
                    E.none


        deleteButtonE = 
            EI.button
                [ EF.color (E.rgba255 0x00 0x00 0x00 0.5)
                , EB.rounded 5
                , E.mouseOver
                    [ EF.color (E.rgba255 0x00 0x00 0x00 0.9)
                    ]
                , E.focused
                    [ EF.color (E.rgba255 0x00 0x00 0x00 0.9)
                    ]
                , E.height (E.px 20)
                , E.width (E.px 30)
                , EF.center
                ]
                { onPress = Just (MsgDeleteProduct product.id)
                , label = E.text "Delete"
                } 
                    
    in
    E.column
        [ E.width (E.px 363)
        , E.height (E.px 320)
        , EBG.color (E.rgba255 0xBC 0xBF 0xDF 3.19)
        , EB.rounded 20
        , E.padding 10
        , E.mouseOver
            [ EBG.color (E.rgba255 0xBC 0xBF 0xDF 0.84)
            ]
        ]
        [ E.column [E.centerX]
            [ E.row[E.centerX, E.spacing 10]
                [ E.newTabLink [E.centerX] { url = product.link , label = titleE }
                , deleteButtonE]]
                , E.column [ E.centerX ]
                    [ thumbnailE
                    , E.row [E.centerX] 
                        [kcalE]
                    , E.row [E.paddingXY 0 10, E.spacing 5]
                        [ proteinsE
                        , fatE
                        , carbohydratesE
                        ]          
            ]
        ]



viewProductCover : String -> String -> E.Element msg
viewProductCover thumbnail title =
    E.image []
        { src = thumbnail
        , description = title
        }

viewButtonGeneric : String -> Msg -> E.Element Msg
viewButtonGeneric naziv msg =
    EI.button
        [ EBG.color (E.rgb255 0x41 0x35 0x71)  
        , EF.color (E.rgb255 0xFD 0xFF 0xFD)
        , EB.rounded 5
        , E.padding 12
        , E.mouseOver
            [ EF.color (E.rgb255 0xDA 0xBD 0xDA)
            ]
        , E.focused
            [ 
             EF.color (E.rgb255 0xDA 0xBD 0xDA)
            ]
        ]
        { onPress = Just msg
        , label = E.text naziv
        }


viewAddProductButton : E.Element Msg
viewAddProductButton =
    viewButtonGeneric "Add Product" MsgAddProduct


viewGetProductsButton : E.Element Msg
viewGetProductsButton =
    viewButtonGeneric "See All" MsgGetFood


viewGetFruitButton : E.Element Msg
viewGetFruitButton =
    viewButtonGeneric "Fruit" MsgGetProductsFruit


viewGetVegetablesButton : E.Element Msg
viewGetVegetablesButton =
    viewButtonGeneric "Vegetables" MsgGetProductsVegetables


viewGetFastFoodButton : E.Element Msg
viewGetFastFoodButton =
    viewButtonGeneric "Fast Food" MsgGetProductsFastFood


viewGetMilkProductsButton : E.Element Msg
viewGetMilkProductsButton =
    viewButtonGeneric "Milk Products" MsgGetProductsMilkAndDairy


viewGetMeatButton : E.Element Msg
viewGetMeatButton =
    viewButtonGeneric "Meat" MsgGetProductsMeat


viewGetCandyAndSweetsButton : E.Element Msg
viewGetCandyAndSweetsButton =
    viewButtonGeneric "Candy and Sweets" MsgGetProductsCandyAndSweets


viewGetDrinksButton : E.Element Msg
viewGetDrinksButton =
    viewButtonGeneric "Drinks" MsgGetProductsDrinksAndBeverages


viewGetOilAndFatButton : E.Element Msg
viewGetOilAndFatButton =
    viewButtonGeneric "Oil and Fat" MsgGetProductsOilAndFats


viewGetNutsAndSeedsButton : E.Element Msg
viewGetNutsAndSeedsButton =
    viewButtonGeneric "Nuts and Seeds" MsgGetProductsNutsAndSeeds


viewGetSoupsButton : E.Element Msg
viewGetSoupsButton =
    viewButtonGeneric "Soups" MsgGetProductsSoups


viewGetHighCalorieFoodButton : E.Element Msg
viewGetHighCalorieFoodButton =
    viewButtonGeneric "High Calorie Food" MsgGetProductsMostCaloric


postProducts : Model -> Cmd Msg
postProducts model =
    Http.post
        { url = "http://localhost:5000/products"
        , body = jsonBody (encode model)
        , expect = Http.expectWhatever MsgSuccesfulPost
        }



cmdDeleteProduct : Int -> Cmd Msg
cmdDeleteProduct id =
    Http.post
        { url = "http://localhost:5000/products/" ++ String.fromInt id
        , body = emptyBody
        , expect = Http.expectWhatever MsgSuccessfulDelete
        }


cmdSearchAll : Cmd Msg
cmdSearchAll =
    Http.get
        { url = "http://localhost:5000/products/all"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchFruit : Cmd Msg
cmdSearchFruit =
    Http.get
        { url = "http://localhost:5000/products/fruit"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchVegetables : Cmd Msg
cmdSearchVegetables =
    Http.get
        { url = "http://localhost:5000/products/vegetables"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchFastFood : Cmd Msg
cmdSearchFastFood =
    Http.get
        { url = "http://localhost:5000/products/fastFood"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchMilkAndDairy : Cmd Msg
cmdSearchMilkAndDairy =
    Http.get
        { url = "http://localhost:5000/products/milkAndDairy"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchMeat : Cmd Msg
cmdSearchMeat =
    Http.get
        { url = "http://localhost:5000/products/meat"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchCandyAndSweets : Cmd Msg
cmdSearchCandyAndSweets =
    Http.get
        { url = "http://localhost:5000/products/candyAndSweets"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchDrinksAndBeverages : Cmd Msg
cmdSearchDrinksAndBeverages =
    Http.get
        { url = "http://localhost:5000/products/drinksAndBeverages"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchOilAndFats : Cmd Msg
cmdSearchOilAndFats =
    Http.get
        { url = "http://localhost:5000/products/oilAndFats"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchNutsAndSeeds : Cmd Msg
cmdSearchNutsAndSeeds =
    Http.get
        { url = "http://localhost:5000/products/nutsAndSeeds"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchSoups : Cmd Msg
cmdSearchSoups =
    Http.get
        { url = "http://localhost:5000/products/soups"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


cmdSearchMostCaloric : Cmd Msg
cmdSearchMostCaloric =
    Http.get
        { url = "http://localhost:5000/products/join"
        , expect = Http.expectJson MsgGotResults decodeItems
        }


encode : Model -> JE.Value
encode model =
    JE.object
        [ ( "name", JE.string model.productName )
        , ( "thumbnail", JE.string model.productThumbnail )
        , ( "kcal", JE.int model.productKcal )
        , ( "link", JE.string model.productLink )
        , ( "proteins", JE.int model.productProteins )
        , ( "fat", JE.int model.productFat )
        , ( "carbohydrates", JE.int model.productCarbohydrates )
        , ( "foodId", JE.int model.productFoodId )
        ]


decodeItems : JD.Decoder (List Product)
decodeItems =
    JD.list decodeItem


decodeItem : JD.Decoder Product
decodeItem =
    JD.map8 Product
        (JD.field "name" JD.string)
        (JD.maybe (JD.field "thumbnail" JD.string))
        (JD.field "link" JD.string)
        (JD.maybe (JD.field "proteins" JD.int))
        (JD.maybe (JD.field "carbohydrates" JD.int))
        (JD.maybe (JD.field "fat" JD.int))
        (JD.maybe (JD.field "kcal" JD.int))
        (JD.field "id" JD.int)
