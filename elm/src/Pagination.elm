module Pagination exposing
    ( Msg(..)
    , Pagination
    , init
    , paginationDecoder
    , viewPaginationHtml
    )

import Html exposing (Html, a, div, li, nav, span, text, ul)
import Html.Attributes exposing (attribute, class, disabled, href)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)


type alias Pagination =
    { offset : Int
    , count : Int
    }


init : Pagination
init =
    { offset = 0, count = 0 }


{-| triggered with the offset
-}
type Msg
    = Request Int


itemsPerPage : Int
itemsPerPage =
    20


translate : Int -> Int
translate a =
    (a + itemsPerPage) // itemsPerPage


translateBack : Int -> Int
translateBack a =
    (a - 1) * itemsPerPage


currentPage : Pagination -> Int
currentPage { offset } =
    translate offset


numberOfPages : Pagination -> Int
numberOfPages { count } =
    translate count


isFirstPage : Pagination -> Bool
isFirstPage paginated =
    currentPage paginated == 1


isLastPage : Pagination -> Bool
isLastPage paginated =
    currentPage paginated == numberOfPages paginated


{-| decodes Json encoded paginated list

The encoding contains an offset, and a count fileds.

-}
paginationDecoder : Decoder Pagination
paginationDecoder =
    Decode.map2 Pagination
        (Decode.field "offset" Decode.int)
        (Decode.field "count" Decode.int)



------------------------------------------------------------------------
--                               view                                 --
------------------------------------------------------------------------


disabledClass : Bool -> List (Html.Attribute msg)
disabledClass bool =
    if bool then
        [ class "disabled" ]

    else
        []


viewPaginatedPrevious : Pagination -> Html Msg
viewPaginatedPrevious paginated =
    let
        disabled =
            isFirstPage paginated

        destination =
            currentPage paginated - 1
    in
    li
        ([ class "pagination-previous" ] ++ disabledClass (isFirstPage paginated))
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


viewPaginatedNext : Pagination -> Html Msg
viewPaginatedNext paginated =
    let
        disabled =
            isLastPage paginated

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


viewPaginatedEntry : Pagination -> Jump -> Html Msg
viewPaginatedEntry paginated jump =
    let
        destination =
            case jump of
                Absolute i ->
                    i

                Relative i ->
                    currentPage paginated + i

        pageText =
            String.fromInt destination
    in
    li []
        [ a
            [ attribute "aria-label" ("Page " ++ pageText)
            , onClick (Request (translateBack destination))
            ]
            [ text pageText ]
        ]


viewEllipsis : Html msg
viewEllipsis =
    li [ class "ellipsis", attribute "aria-hidden" "true" ] []


{-| html render for pagination
-}
viewPaginationHtml : Pagination -> Html Msg
viewPaginationHtml paginated =
    let
        current =
            currentPage paginated

        last =
            numberOfPages paginated
    in
    div []
        (if numberOfPages paginated <= 1 then
            []

         else
            [ nav [ attribute "aria-label" "Pagination" ]
                [ ul [ class "pagination text-center" ]
                    (List.filterMap
                        identity
                        [ Just (viewPaginatedPrevious paginated)
                        , guard (current >= 3) <|
                            viewPaginatedEntry paginated (Absolute 1)
                        , guard (current > 3) viewEllipsis
                        , guard (current > 1) <|
                            viewPaginatedEntry paginated (Relative -1)
                        , Just <|
                            li [ class "current" ]
                                [ span [ class "show-for-sr" ]
                                    [ text "You're on page" ]
                                , text (String.fromInt current)
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
