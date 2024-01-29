module Main exposing (main)

import Browser
import Browser.Events
import Char exposing (isAlphaNum)
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import List
import List.Extra
import Random
import Task


main : Program Int Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


dayLength : Float
dayLength =
    1000 * 30



-- MODEL


type Tag
    = Plate
    | Cutlery
    | Napkins
    | Cup
    | Decor


tagToString : Tag -> String
tagToString tag =
    case tag of
        Plate ->
            "Plate"

        Cutlery ->
            "Cutlery"

        Napkins ->
            "Napkins"

        Cup ->
            "Cup"

        Decor ->
            "Decor"


tagToStringPlural : Tag -> String
tagToStringPlural tag =
    case tag of
        Plate ->
            "Plates"

        Cutlery ->
            "Cutlery"

        Napkins ->
            "Napkins"

        Cup ->
            "Cups"

        Decor ->
            "Decor"


type alias Item =
    { name : String
    , img : String
    , price : Int
    , value : Int
    , tags : List Tag
    }


type alias Model =
    { seed : Random.Seed
    , keysDown : List Key -- Which keys are currently being pressed by user
    , playerMoney : Int
    , shelf : List ShelfItem
    , purchases : List Item
    , endTimer : Float
    , started : Bool -- if not true, the player hasn't indicated to start the game yet
    }


type ShelfItem
    = ShelfItem Item
    | EmptyShelfItem


item1 : Item
item1 =
    { name = "Plebb plate"
    , img = "/plate_basic.png"
    , price = 50
    , value = 50
    , tags = [ Plate ]
    }


otherItems : List Item
otherItems =
    [ { name = "Plebb cutlery set"
      , img = "/cutlery_basic.png"
      , price = 65
      , value = 65
      , tags = [ Cutlery ]
      }
    , { name = "Plebb cup"
      , img = "/cup_basic.png"
      , price = 25
      , value = 25
      , tags = [ Cup ]
      }
    , { name = "Plebb napkins"
      , img = "/napkins_basic.png"
      , price = 35
      , value = 35
      , tags = [ Napkins ]
      }
    , { name = "Floral plate"
      , img = "/plate_flower.png"
      , price = 75
      , value = 65
      , tags = [ Plate ]
      }
    , { name = "Floral cutlery set"
      , img = "/cutlery_flower.png"
      , price = 100
      , value = 80
      , tags = [ Cutlery ]
      }
    , { name = "Floral cup"
      , img = "/cup_flower.png"
      , price = 40
      , value = 30
      , tags = [ Cup ]
      }
    , { name = "Floral napkins"
      , img = "/napkins_flower.png"
      , price = 50
      , value = 40
      , tags = [ Napkins ]
      }
    , { name = "Gold Star plate"
      , img = "/plate_star.png"
      , price = 150
      , value = 200
      , tags = [ Plate ]
      }
    , { name = "Gold Star cutlery set"
      , img = "/cutlery_star.png"
      , price = 200
      , value = 260
      , tags = [ Cutlery ]
      }
    , { name = "Gold Star cup"
      , img = "/cup_star.png"
      , price = 75
      , value = 100
      , tags = [ Cup ]
      }
    , { name = "Gold Star napkins"
      , img = "/napkins_star.png"
      , price = 100
      , value = 140
      , tags = [ Napkins ]
      }
    , { name = "Books"
      , img = "/decor_books.png"
      , price = 80
      , value = 100
      , tags = [ Decor ]
      }
    , { name = "Cactus"
      , img = "/decor_cactus.png"
      , price = 50
      , value = 60
      , tags = [ Decor ]
      }
    , { name = "Candle"
      , img = "/decor_candle.png"
      , price = 40
      , value = 50
      , tags = [ Decor ]
      }
    , { name = "Charcuterie board"
      , img = "/decor_charcuterie_board.png"
      , price = 100
      , value = 120
      , tags = [ Decor ]
      }

    -- , { name = "Coasters"
    --   , img = "/decor_coasters.png"
    --   , price = 30
    --   , value = 40
    --   , tags = [ Decor ]
    --   }
    , { name = "Flower vase"
      , img = "/decor_flower_vase.png"
      , price = 40
      , value = 50
      , tags = [ Decor ]
      }
    , { name = "Fruit bowl"
      , img = "/decor_fruit_bowl.png"
      , price = 50
      , value = 60
      , tags = [ Decor ]
      }
    , { name = "Painting"
      , img = "/decor_painting.png"
      , price = 80
      , value = 100
      , tags = [ Decor ]
      }
    , { name = "Picture"
      , img = "/decor_picture.png"
      , price = 40
      , value = 50
      , tags = [ Decor ]
      }

    -- , { name = "Place mat"
    --   , img = "/decor_place_mat.png"
    --   , price = 30
    --   , value = 40
    --   , tags = [ Decor ]
    --   }
    -- , { name = "Platter"
    --   , img = "/decor_platter.png"
    --   , price = 50
    --   , value = 60
    --   , tags = [ Decor ]
    --   }
    , { name = "Rubik's cube"
      , img = "/decor_rubiks_cube.png"
      , price = 30
      , value = 40
      , tags = [ Decor ]
      }
    , { name = "Tablecloth"
      , img = "/decor_tablecloth.png"
      , price = 40
      , value = 50
      , tags = [ Decor ]
      }
    ]


type alias Requirement =
    ( Int, Tag )


defaultRequirements : List Requirement
defaultRequirements =
    [ ( 4, Plate ), ( 4, Cutlery ), ( 4, Cup ), ( 1, Napkins ) ]


requirementMet : Model -> Requirement -> Bool
requirementMet model ( num, tag ) =
    let
        numInBag : Int
        numInBag =
            model.purchases
                |> List.filter (\item -> List.member tag item.tags)
                |> List.length
    in
    numInBag >= num


requirementsMet : Model -> List Requirement -> Bool
requirementsMet model requirements =
    List.all (requirementMet model) requirements


getScore : List Item -> Int
getScore items =
    items
        |> List.map .value
        |> List.sum


itemGenerator : Random.Generator Item
itemGenerator =
    Random.uniform item1 otherItems


shelfGenerator : Random.Generator (List ShelfItem)
shelfGenerator =
    Random.list 4 itemGenerator
        |> Random.map (List.map ShelfItem)


init : Int -> ( Model, Cmd Msg )
init randomInt =
    let
        initialSeed : Random.Seed
        initialSeed =
            Random.initialSeed randomInt

        ( initialShelf, seed1 ) =
            Random.step shelfGenerator initialSeed
    in
    ( { seed = seed1, keysDown = [], playerMoney = 1000, shelf = initialShelf, purchases = [], endTimer = dayLength, started = False }, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | HandleAnimationFrameDelta Float
    | HandleKeyUp Key
    | HandleKeyDown Key
    | Purchase Int -- purchase the item at that index on the shelf
    | NextShelf
    | HandleStart


attemptPurchaseItem : { shelf : List ShelfItem, index : Int, playerMoney : Int } -> { shelf : List ShelfItem, playerMoney : Int, purchases : List Item }
attemptPurchaseItem { shelf, index, playerMoney } =
    case List.Extra.getAt index shelf of
        Just (ShelfItem item) ->
            let
                newPlayerMoney : Int
                newPlayerMoney =
                    playerMoney - item.price

                newShelf : List ShelfItem
                newShelf =
                    List.Extra.setAt index EmptyShelfItem shelf
            in
            if newPlayerMoney >= 0 then
                { shelf = newShelf, playerMoney = newPlayerMoney, purchases = [ item ] }

            else
                { shelf = shelf, playerMoney = playerMoney, purchases = [] }

        _ ->
            { shelf = shelf, playerMoney = playerMoney, purchases = [] }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HandleAnimationFrameDelta delta ->
            let
                newEndTimer : Float
                newEndTimer =
                    if model.started then
                        model.endTimer - delta

                    else
                        model.endTimer
            in
            ( { model | endTimer = newEndTimer }, Cmd.none )

        HandleKeyDown key ->
            let
                newKeysDown : List Key
                newKeysDown =
                    model.keysDown
                        ++ [ key ]
                        |> List.Extra.unique

                maybeShortcut : Maybe Shortcut
                maybeShortcut =
                    keysToShortcut (Debug.log "foobar newKeysDown" newKeysDown)
            in
            case maybeShortcut of
                Nothing ->
                    ( { model | keysDown = newKeysDown }, Cmd.none )

                Just shortcut ->
                    ( model, Task.perform (\_ -> keyUpShortcutToMsg model maybeShortcut) (Task.succeed ()) )

        HandleKeyUp key ->
            ( { model
                | keysDown = List.filter ((/=) key) model.keysDown
              }
            , Cmd.none
            )

        Purchase index ->
            let
                purchaseResult =
                    attemptPurchaseItem { shelf = model.shelf, index = index, playerMoney = model.playerMoney }

                newShelf : List ShelfItem
                newShelf =
                    purchaseResult.shelf

                newPlayerMoney : Int
                newPlayerMoney =
                    purchaseResult.playerMoney

                newPurchases : List Item
                newPurchases =
                    purchaseResult.purchases
            in
            ( { model
                | playerMoney = newPlayerMoney
                , purchases = model.purchases ++ newPurchases
                , shelf = newShelf
              }
            , Cmd.none
            )

        NextShelf ->
            let
                ( newShelf, newSeed ) =
                    Random.step shelfGenerator model.seed
            in
            ( { model | seed = newSeed, shelf = newShelf }, Cmd.none )

        HandleStart ->
            ( { model | started = True }, Cmd.none )



-- SUBSCRIPTIONS


type Key
    = Num1
    | Num2
    | Num3
    | Num4
    | Shift
    | Space


type Shortcut
    = ShortcutPurchaseShelfItem Int
    | ShortcutNextShelf


keyCodeToKey : Int -> Maybe Key
keyCodeToKey keyCode =
    case keyCode of
        49 ->
            Just Num1

        50 ->
            Just Num2

        51 ->
            Just Num3

        52 ->
            Just Num4

        16 ->
            Just Shift

        32 ->
            Just Space

        _ ->
            Nothing


keysToShortcut : List Key -> Maybe Shortcut
keysToShortcut keys =
    case keys of
        [ Num1 ] ->
            Just (ShortcutPurchaseShelfItem 0)

        [ Num2 ] ->
            Just (ShortcutPurchaseShelfItem 1)

        [ Num3 ] ->
            Just (ShortcutPurchaseShelfItem 2)

        [ Num4 ] ->
            Just (ShortcutPurchaseShelfItem 3)

        [ Shift, Num1 ] ->
            Just (ShortcutPurchaseShelfItem 4)

        [ Shift, Num2 ] ->
            Just (ShortcutPurchaseShelfItem 5)

        [ Shift, Num3 ] ->
            Just (ShortcutPurchaseShelfItem 6)

        [ Shift, Num4 ] ->
            Just (ShortcutPurchaseShelfItem 7)

        [ Space ] ->
            Just ShortcutNextShelf

        _ ->
            Nothing


keyUpShortcutToMsg : Model -> Maybe Shortcut -> Msg
keyUpShortcutToMsg model maybeShortcut =
    case maybeShortcut of
        Nothing ->
            NoOp

        Just (ShortcutPurchaseShelfItem index) ->
            Purchase index

        Just ShortcutNextShelf ->
            NextShelf


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.endTimer <= 0 then
        Sub.none

    else
        let
            keyUpDecoder : D.Decoder Msg
            keyUpDecoder =
                D.field "keyCode" D.int
                    |> D.map keyCodeToKey
                    |> D.map
                        (\maybeKey ->
                            case maybeKey of
                                Nothing ->
                                    NoOp

                                Just key ->
                                    HandleKeyUp key
                        )

            keyDownDecoder : D.Decoder Msg
            keyDownDecoder =
                D.field "keyCode" D.int
                    |> D.map keyCodeToKey
                    |> D.map
                        (\maybeKey ->
                            case maybeKey of
                                Nothing ->
                                    NoOp

                                Just key ->
                                    HandleKeyDown key
                        )
        in
        Sub.batch
            [ Browser.Events.onAnimationFrameDelta HandleAnimationFrameDelta
            , Browser.Events.onKeyUp keyUpDecoder
            , Browser.Events.onKeyDown keyDownDecoder
            ]



-- VIEW


shopItemImage : String -> List Tag -> Html Msg
shopItemImage itemImageSrc tags =
    div [ class "avatar" ]
        [ div [ class "w-36 rounded-lg border-2 border-primary relative" ]
            [ img [ src itemImageSrc ] []
            , div [ class "absolute bottom-0 right-0 flex p-1 gap-1" ]
                (List.map
                    (\tag ->
                        div
                            [ class "badge"
                            , classList
                                [ ( "badge-accent", List.member tag [ Plate, Napkins, Cutlery, Cup ] )
                                , ( "badge-neutral", tag == Decor )
                                ]
                            ]
                            [ text (tagToString tag) ]
                    )
                    tags
                )
            ]
        ]


renderShopItem : Model -> Int -> ShelfItem -> Html Msg
renderShopItem model index shelfItem =
    let
        shortcutLabel : String
        shortcutLabel =
            case index of
                0 ->
                    "1"

                1 ->
                    "2"

                2 ->
                    "3"

                3 ->
                    "4"

                4 ->
                    "Shift + 1"

                5 ->
                    "Shift + 2"

                6 ->
                    "Shift + 3"

                7 ->
                    "Shift + 4"

                _ ->
                    ""
    in
    div [ class "flex flex-col items-center gap-1" ]
        [ kbd [ class "kbd kbd-sm" ] [ text shortcutLabel ]
        , case shelfItem of
            ShelfItem item ->
                let
                    canAffordItem : Bool
                    canAffordItem =
                        model.playerMoney >= item.price
                in
                div [ class "w-64 border-dashed rounded-xl border border-neutral shadow flex flex-col gap-4 items-center p-4" ]
                    [ shopItemImage item.img item.tags
                    , span [ class "font-bold" ] [ text item.name ]
                    , span [ class "text-4xl font-thin" ] [ text ("$" ++ String.fromInt item.price) ]
                    , button [ class "btn btn-primary btn-sm", onClick (Purchase index), disabled (not canAffordItem) ] [ text "Purchase" ]
                    ]

            EmptyShelfItem ->
                div [ class "w-64 h-[318px] border-dashed rounded-xl border border-neutral shadow flex flex-col gap-4 items-center justify-center p-4" ]
                    [ span [ class "text-sm italic" ] [ text "Sold Out" ]
                    ]
        ]


renderEndTimer : Model -> Html Msg
renderEndTimer model =
    let
        percentComplete : Float
        percentComplete =
            (dayLength - model.endTimer) / dayLength * 100
    in
    div [ class "flex gap-4 items-center w-full", classList [ ( "hidden", model.endTimer <= 0 ) ] ]
        [ div [ class "w-20 flex items-center justify-center font-bold px-2 py-1 rounded text-success" ] [ text "Start" ]
        , progress [ class "progress progress-warning flex-grow", value (String.fromFloat percentComplete), Html.Attributes.max "100" ] []
        , div [ class "w-36 flex items-center justify-center font-bold px-2 py-1 rounded text-error" ] [ text "Shop Closes" ]
        ]


renderRequirements : Model -> List Requirement -> Html Msg
renderRequirements model requirements =
    ul [ class "flex flex-col gap-2" ]
        (List.map
            (\( num, tag ) ->
                let
                    requirementLabel : String
                    requirementLabel =
                        if num == 1 then
                            tagToString tag

                        else
                            String.fromInt num ++ " " ++ tagToStringPlural tag

                    isChecked : Bool
                    isChecked =
                        model.purchases
                            |> List.filter (\item -> List.member tag item.tags)
                            |> List.length
                            |> (\count -> count >= num)
                in
                li [ class "flex gap-2 items-center" ]
                    [ input [ type_ "checkbox", class "checkbox", checked isChecked ] []
                    , span [ class "text-sm" ] [ text requirementLabel ]
                    ]
            )
            requirements
            ++ [ li
                    [ class "flex gap-2 items-center"
                    ]
                    [ FeatherIcons.plus
                        |> FeatherIcons.toHtml []
                    , span [ class "text-sm" ] [ text "Decor!" ]
                    ]
               ]
        )


renderBagContents : Model -> Bool -> Html Msg
renderBagContents model showValue =
    table [ class "table table-sm", classList [ ( "hidden", List.isEmpty model.purchases ) ] ]
        [ tbody []
            (List.concat
                [ [ tr []
                        [ th [] [ text "Purchases" ]
                        , th [] [ text "Value" ]
                        ]
                  ]
                , List.map
                    (\item ->
                        tr []
                            [ td [] [ text item.name ]
                            , td [ class "text-success font-semibold" ]
                                [ if showValue then
                                    text (String.fromInt item.value)

                                  else
                                    text ""
                                ]
                            ]
                    )
                    model.purchases
                ]
            )
        ]


renderBagPreview : Model -> Html Msg
renderBagPreview model =
    let
        itemsInBagString : String
        itemsInBagString =
            case List.length model.purchases of
                0 ->
                    ""

                1 ->
                    "1 item in bag"

                _ ->
                    String.fromInt (List.length model.purchases) ++ " items in bag"
    in
    div [ class "w-full h-full flex items-center justify-center italic" ] [ text itemsInBagString ]


renderBag : Model -> List Requirement -> Html Msg
renderBag model requirements =
    div [ class "h-[1024px] max-h-full overflow-y-auto flex flex-col gap-4 items-center bg-base-300 p-4 w-[25rem] rounded" ]
        [ div [ class "text-xl font-bold" ] [ span [] [ text "Shopping list" ] ]
        , div [ class "flex gap-2 w-full justify-start" ]
            [ div [ class "w-[10rem]" ] [ renderRequirements model requirements ]
            , div [ class "divider divider-horizontal mx-2" ] []
            , renderBagContents model (model.endTimer <= 0)
            ]
        ]


renderScore : Model -> List Requirement -> Html Msg
renderScore model requirements =
    let
        gameOver : Bool
        gameOver =
            model.endTimer
                <= 0
                && model.started

        showScore : Bool
        showScore =
            -- requirementsMet model requirements
            True

        score : Int
        score =
            getScore model.purchases
    in
    -- div [ class "flex flex-col gap-4 items-center w-full", classList [ ( "hidden", not gameOver ) ] ]
    div [ class "flex flex-col gap-4 items-center w-full pt-24" ]
        [ span [ class "text-2xl fade-in-up" ] [ text "Evaluation" ]
        , hr [ class "divider divider-horizontal w-full divider-neutral" ] []
        , div [ classList [ ( "hidden", showScore ) ], class "flex flex-col w-full items-center fade-in-up" ]
            [ span [ class "text-4xl font-bold text-error" ] [ text "Failure" ]
            , span [] [ text "You failed to purchase all required items!" ]
            ]
        , div [ classList [ ( "hidden", not showScore ) ], class "flex flex-col w-full items-center gap-4" ]
            [ span [ class "text-4xl font-bold text-success" ] [ text "Success!" ]
            , span [ class "text-2xl font-bold" ] [ text <| "Score: " ++ String.fromInt score ]
            , span [] [ text "(sum of value of all purchases)" ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        shelfEmpty : Bool
        shelfEmpty =
            List.all (\item -> item == EmptyShelfItem) model.shelf
    in
    div [ class "flex flex-col gap-4 items-start p-6 w-screen h-screen", attribute "data-theme" "corporate" ]
        [ span [ class "text-4xl underline" ] [ text "Gift Card" ]
        , div [ class "flex gap-4 w-full" ]
            [ div [ class "flex flex-col gap-4 items-start relative" ]
                [ div [ class "flex w-full justify-center gap-4 items-center p-4" ]
                    [ div [ class "w-64 h-36 rounded bg-info text-info-content border border-neutral shadow-xl flex flex-col gap-2 items-center p-4" ]
                        [ span [ class "font-thin" ] [ text "Crate and Barrel" ]
                        , span [ class "text-sm font-semibold" ] [ text "remaining value" ]
                        , span [ class "text-4xl" ] [ text ("$" ++ String.fromInt model.playerMoney) ]
                        ]
                    , button [ class "btn btn-primary", onClick HandleStart ] [ text "Start" ]
                    ]
                , div [ class "flex flex-col gap-4 items-center" ]
                    [ div [ class "flex gap-2 items-center" ]
                        [ div [ class "rounded bg-base-200 grid grid-cols-4 grid-rows gap-4 gap-y-16 p-8 pb-12" ]
                            (List.indexedMap (renderShopItem model) model.shelf)
                        , button
                            [ class "btn"
                            , onClick NextShelf
                            , class "relative"
                            ]
                            [ text "Next Shelf"
                            , span
                                [ class " flex h-3 w-3 absolute top-0 right-0", classList [ ( "hidden", not shelfEmpty ) ] ]
                                [ span [ class "animate-ping absolute inline-flex h-full w-full rounded-full bg-info opacity-75" ] []
                                , span [ class "relative inline-flex rounded-full h-3 w-3 bg-info" ] []
                                ]
                            ]
                        ]

                    -- End timer
                    , renderEndTimer model
                    ]
                , div [ class "absolute bg-base-content bg-opacity-70 w-full h-full flex items-center justify-center", classList [ ( "hidden", model.endTimer > 0 ) ] ]
                    [ span [ class "text-error text-8xl font-bold" ] [ text "Store Closed!" ] ]
                ]
            , div [ class "flex flex-col gap-4 items-center " ]
                [ renderBag model defaultRequirements ]
            , div [ class "flex-grow" ] [ renderScore model defaultRequirements ]
            ]
        ]
