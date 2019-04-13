module Pagination exposing
    ( Msg(..), Pagination, init
    , setBusy, setOffset
    , view
    , jsonEncode, jsonDecoder
    )

{-| Pagination

Displays a pagination block of html and triggers a message with the offset. Add
Pagination to the model, render it using view, and in the update handle
Pagination.Request with sending a request to the server with pagination data
encoded with Pagination.jsonEncode. Then in the server response decode
Pagination with Pagination.jsonDecoder.

The server will receive offset in the json and is expected to respond with
offset and count.

#Types and constructors

@docs Msg, Pagination, init

#Data manipulation

@docs setBusy, setOffset

#View

@docs view

#Conversions

@docs jsonEncode, jsonDecoder

-}

import Html exposing (Html, a, div, li, nav, span, text, ul)
import Html.Attributes exposing (attribute, class, disabled, href)
import Html.Events exposing (onClick)
import Json.Decode
import Json.Encode



------------------------------------------------------------------------
--                       Types and constructors                       --
------------------------------------------------------------------------


type alias Record =
    { offset : Int
    , count : Int
    , busy : Bool
    }


{-| Opaque type representing a pagination.

Put in your model.

-}
type Pagination
    = Pagination Record


{-| Starting pagination

Call by model init.

-}
init : Pagination
init =
    Pagination { offset = 0, count = 0, busy = False }


{-| triggered with the offset
-}
type Msg
    = Request Int



------------------------------------------------------------------------
--                         Data manipulation                          --
------------------------------------------------------------------------


{-| Sets the offset

We first trigger the message Pagination.Request that should be handled by the
model update. At that point modify pagination in the model with the new offset.

-}
setOffset : Int -> Pagination -> Pagination
setOffset offset (Pagination pagination) =
    Pagination { pagination | offset = offset }


{-| Sets pagination disabled

Show the user that we are waiting for the pagination request to complete. When
the model receives Pagination.Request it can set this to disable Pagination
while the server is responding.

-}
setBusy : Bool -> Pagination -> Pagination
setBusy bool (Pagination pagination) =
    Pagination { pagination | busy = bool }



------------------------------------------------------------------------
--                            Conversions                             --
------------------------------------------------------------------------


{-| decodes Json encoded paginated list

The encoding contains an offset, and a count fileds.

-}
jsonDecoder : Json.Decode.Decoder Pagination
jsonDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "offset" Json.Decode.int)
        (Json.Decode.field "count" Json.Decode.int)
        |> Json.Decode.andThen
            (\( a, b ) ->
                if b < 0 || a < 0 || a >= b then
                    Json.Decode.fail <|
                        "invalid values (offset, count) : ("
                            ++ String.fromInt a
                            ++ ", "
                            ++ String.fromInt b
                            ++ ")"

                else
                    Json.Decode.succeed <|
                        Pagination { offset = a, count = b, busy = False }
            )


{-| encodes a pagination request into JSon

we encode an offset fields sent to the server

-}
jsonEncode : Pagination -> Json.Encode.Value
jsonEncode (Pagination { offset }) =
    Json.Encode.object [ ( "offset", Json.Encode.int offset ) ]



------------------------------------------------------------------------
--                               view                                 --
------------------------------------------------------------------------


disabledClass : Bool -> List (Html.Attribute msg)
disabledClass bool =
    if bool then
        [ class "disabled" ]

    else
        []


viewPaginatedPrevious : Record -> Html Msg
viewPaginatedPrevious paginated =
    let
        disabled =
            isFirstPage paginated || paginated.busy

        destination =
            currentPage paginated - 1
    in
    li
        ([ class "pagination-previous" ] ++ disabledClass disabled)
        (if disabled then
            [ text "Previous" ]

         else
            [ a
                [ attribute "aria-label" "Previous Page"
                , onClick (Request (translateBack destination))
                ]
                [ text "Previous" ]
            ]
        )


viewPaginatedNext : Record -> Html Msg
viewPaginatedNext paginated =
    let
        disabled =
            isLastPage paginated || paginated.busy

        destination =
            currentPage paginated + 1
    in
    li
        ([ class "pagination-next" ] ++ disabledClass disabled)
        (if disabled then
            [ text "Next" ]

         else
            [ a
                [ attribute "aria-label" "Next Page"
                , onClick (Request (translateBack destination))
                ]
                [ text "Next" ]
            ]
        )


guard : Bool -> a -> Maybe a
guard condition a =
    if condition then
        Just a

    else
        Nothing


type Jump
    = Absolute Int
    | Relative Int


viewPaginatedEntry : Record -> Jump -> Html Msg
viewPaginatedEntry paginated jump =
    let
        destination =
            case jump of
                Absolute i ->
                    i

                Relative i ->
                    currentPage paginated + i

        pageText =
            String.fromInt (1 + destination)
    in
    li []
        [ a
            ([ attribute "aria-label" ("Page " ++ pageText)
             , onClick (Request (translateBack destination))
             ]
                ++ disabledClass paginated.busy
            )
            [ text pageText ]
        ]


viewEllipsis : Html msg
viewEllipsis =
    li [ class "ellipsis", attribute "aria-hidden" "true" ] []


{-| html render for pagination
-}
view : Pagination -> Html Msg
view (Pagination paginated) =
    let
        current =
            currentPage paginated

        last =
            lastPage paginated
    in
    div []
        (if numberOfPages paginated <= 1 then
            []

         else
            [ nav
                [ attribute "aria-label" "Pagination" ]
                [ ul [ class "pagination text-center" ]
                    (List.filterMap
                        identity
                        [ Just (viewPaginatedPrevious paginated)
                        , guard (current >= 3) <|
                            viewPaginatedEntry paginated (Absolute 0)
                        , guard (current > 3) viewEllipsis
                        , guard (current > 1) <|
                            viewPaginatedEntry paginated (Relative -1)
                        , Just <|
                            li [ class "current" ]
                                [ span [ class "show-for-sr" ]
                                    [ text "You're on page" ]
                                , text (String.fromInt (1 + current))
                                ]
                        , guard (current < last) <|
                            viewPaginatedEntry paginated (Relative 1)
                        , guard (current < last - 1) <|
                            viewPaginatedEntry paginated (Relative 2)
                        , guard (current < last - 3) viewEllipsis
                        , guard (current < last - 3) <|
                            viewPaginatedEntry paginated (Absolute last)
                        , Just (viewPaginatedNext paginated)
                        ]
                    )
                ]
            ]
        )



------------------------------------------------------------------------
--                           Private stuff                            --
------------------------------------------------------------------------


itemsPerPage : Int
itemsPerPage =
    20


translate : Int -> Int
translate a =
    (a + itemsPerPage - 1) // itemsPerPage


translateBack : Int -> Int
translateBack a =
    a * itemsPerPage


currentPage : Record -> Int
currentPage { offset } =
    translate offset


numberOfPages : Record -> Int
numberOfPages { count } =
    translate count


lastPage : Record -> Int
lastPage record =
    numberOfPages record - 1


isFirstPage : Record -> Bool
isFirstPage paginated =
    currentPage paginated == 0


isLastPage : Record -> Bool
isLastPage paginated =
    currentPage paginated + 1 == numberOfPages paginated
