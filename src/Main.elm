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
    -- 1000 * 30
    1000 * 30



-- MODEL


type Tag
    = Plate
    | Spoon
    | Fork
    | Knife
    | Cup


tagToString : Tag -> String
tagToString tag =
    case tag of
        Plate ->
            "Plate"

        Spoon ->
            "Spoon"

        Fork ->
            "Fork"

        Knife ->
            "Knife"

        Cup ->
            "Cup"


tagToStringPlural : Tag -> String
tagToStringPlural tag =
    case tag of
        Plate ->
            "Plates"

        Spoon ->
            "Spoons"

        Fork ->
            "Forks"

        Knife ->
            "Knives"

        Cup ->
            "Cups"


type alias Item =
    { name : String
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
    { name = "Plate"
    , price = 100
    , value = 100
    , tags = [ Plate ]
    }


otherItems : List Item
otherItems =
    [ { name = "Spoon"
      , price = 50
      , value = 50
      , tags = [ Spoon ]
      }
    , { name = "Fork"
      , price = 50
      , value = 50
      , tags = [ Fork ]
      }
    , { name = "Knife"
      , price = 50
      , value = 50
      , tags = [ Knife ]
      }
    , { name = "Cup"
      , price = 50
      , value = 50
      , tags = [ Cup ]
      }
    ]


type alias Requirement =
    ( Int, Tag )


defaultRequirements : List Requirement
defaultRequirements =
    [ ( 2, Plate ), ( 1, Spoon ), ( 1, Fork ), ( 1, Knife ) ]


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
    Random.list 8 itemGenerator
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


renderShopItem : Model -> Int -> ShelfItem -> Html Msg
renderShopItem model index shelfItem =
    case shelfItem of
        ShelfItem item ->
            let
                canAffordItem : Bool
                canAffordItem =
                    model.playerMoney >= item.price
            in
            div [ class "w-64 h-48 border-dashed rounded-xl border border-neutral shadow-xl flex flex-col gap-4 items-center p-4" ]
                [ span [ class "text-sm" ] [ text item.name ]
                , span [ class "text-4xl" ] [ text ("$" ++ String.fromInt item.price) ]
                , button [ class "btn btn-primary", onClick (Purchase index), disabled (not canAffordItem) ] [ text "Purchase" ]
                ]

        EmptyShelfItem ->
            div [ class "w-64 h-48 border-dashed rounded-xl border border-neutral shadow-xl flex flex-col gap-4 items-center justify-center p-4" ]
                [ span [ class "text-sm italic" ] [ text "Sold Out" ]
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
        )


renderBagContents : Model -> Html Msg
renderBagContents model =
    table [ class "table table-sm" ]
        [ tbody []
            (List.concat
                [ [ tr []
                        [ th [] [ text "" ]
                        , th [] [ text "Value" ]
                        ]
                  ]
                , List.map
                    (\item ->
                        tr []
                            [ td [] [ text item.name ]
                            , td [ class "text-success font-semibold" ] [ text (String.fromInt item.value) ]
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
    div [ class "flex flex-col gap-4 items-center bg-neutral text-neutral-content p-4 w-[25rem]" ]
        [ div [ class "text-xl font-bold" ] [ span [] [ text "Shopping Bag" ] ]
        , div [ class "flex gap-2 w-full justify-start" ]
            [ div [ class "w-[10rem]" ] [ renderRequirements model requirements ]
            , div [ class "divider divider-horizontal mx-2" ] []
            , if model.endTimer > 0 then
                renderBagPreview model

              else
                renderBagContents model
            ]
        ]


renderScore : Model -> List Requirement -> Html Msg
renderScore model requirements =
    let
        scoreEnabled : Bool
        scoreEnabled =
            requirementsMet model requirements

        score : Int
        score =
            getScore model.purchases
    in
    div [ class "flex flex-col gap-4 items-center w-full" ]
        [ span [] [ text "Score" ]
        , div [ class "flex gap-2" ]
            [ input [ type_ "checkbox", checked (requirementsMet model requirements), class "checkbox checkbox-primary" ] []
            , span [] [ text "All required items in bag" ]
            ]
        , if scoreEnabled then
            div [] [ text (String.fromInt score) ]

          else
            div [] [ text "You must acquire all required items to score points" ]
        ]


view : Model -> Html Msg
view model =
    div [ class "flex flex-col gap-4 items-start p-6 w-full" ]
        [ span [ class "text-4xl underline" ] [ text "Gift Card" ]
        , div [ class "flex gap-4 w-full" ]
            [ div [ class "flex flex-col gap-4 items-start relative" ]
                [ div [ class "w-64 h-36 rounded bg-info text-info-content border border-neutral shadow-xl flex flex-col gap-2 items-center p-4" ]
                    [ span [ class "font-thin" ] [ text "Crate and Barrel" ]
                    , span [ class "text-sm font-semibold" ] [ text "remaining value" ]
                    , span [ class "text-4xl" ] [ text ("$" ++ String.fromInt model.playerMoney) ]
                    ]
                , div [ class "flex flex-col gap-4 items-center" ]
                    [ div [ class "flex gap-2 items-center" ]
                        [ div [ class "rounded bg-base-200 grid grid-cols-4 grid-rows-2 gap-4 p-4" ]
                            (List.indexedMap (renderShopItem model) model.shelf)
                        , button [ class "btn btn-neutral", onClick NextShelf ] [ text "Next Shelf" ]
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
