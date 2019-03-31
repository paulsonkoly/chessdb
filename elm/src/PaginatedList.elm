module PaginatedList exposing
    ( Msg(..)
    , PaginatedList
    , getOffset
    , paginatedListDecoder
    , setOffset
    , viewPaginatedHtml
    )

import Html exposing (Html, a, div, li, nav, span, text, ul)
import Html.Attributes exposing (attribute, class, disabled, href)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)


type PaginatedList a
    = PaginatedList
        { offset : Int
        , count : Int
        , data : List a
        }


getOffset : PaginatedList a -> Int
getOffset (PaginatedList { offset }) =
    offset


setOffset : Int -> PaginatedList a -> PaginatedList a
setOffset offset (PaginatedList record) =
    PaginatedList { record | offset = offset }


{-| triggered with the offset
-}
type Msg
    = PaginationRequest Int


itemsPerPage : Int
itemsPerPage =
    20


paginatedData : PaginatedList a -> List a
paginatedData (PaginatedList { data }) =
    data


translate : Int -> Int
translate a =
    (a + itemsPerPage) // itemsPerPage


translateBack : Int -> Int
translateBack a =
    (a - 1) * itemsPerPage


currentPage : PaginatedList a -> Int
currentPage (PaginatedList { offset }) =
    translate offset


numberOfPages : PaginatedList a -> Int
numberOfPages (PaginatedList { count }) =
    translate count


isFirstPage : PaginatedList a -> Bool
isFirstPage paginated =
    currentPage paginated == 1


isLastPage : PaginatedList a -> Bool
isLastPage paginated =
    currentPage paginated == numberOfPages paginated


isPageValid : PaginatedList a -> Int -> Bool
isPageValid paginated page =
    1 <= page && page <= numberOfPages paginated


{-| decodes Json encoded paginated list

The encoding contains an offset, a count and a data field.

-}
paginatedListDecoder : Decoder a -> Decoder (PaginatedList a)
paginatedListDecoder internDecode =
    Decode.map3 (\a b c -> PaginatedList { offset = a, count = b, data = c })
        (Decode.field "offset" Decode.int)
        (Decode.field "count" Decode.int)
        (Decode.field "data" (Decode.list internDecode))



------------------------------------------------------------------------
--                               view                                 --
------------------------------------------------------------------------


disabledClass : Bool -> List (Html.Attribute msg)
disabledClass bool =
    if bool then
        [ class "disabled" ]

    else
        []


viewPaginatedPrevious :
    PaginatedList a
    -> (Msg -> msg)
    -> Html msg
viewPaginatedPrevious paginated wrapper =
    let
        disabled =
            isFirstPage paginated

        destination =
            currentPage paginated - 1

        click =
            wrapper (PaginationRequest (translateBack destination))
    in
    li
        ([ class "pagination-previous" ] ++ disabledClass (isFirstPage paginated))
        (if disabled then
            [ text "Previous" ]

         else
            [ a
                [ attribute "aria-label" "Previous Page"
                , onClick click
                ]
                [ text "Previous" ]
            ]
        )


viewPaginatedNext :
    PaginatedList a
    -> (Msg -> msg)
    -> Html msg
viewPaginatedNext paginated wrapper =
    let
        disabled =
            isLastPage paginated

        destination =
            currentPage paginated + 1

        click =
            wrapper (PaginationRequest (translateBack destination))
    in
    li
        ([ class "pagination-next" ] ++ disabledClass disabled)
        (if disabled then
            [ text "Next" ]

         else
            [ a
                [ attribute "aria-label" "Next Page"
                , onClick click
                ]
                [ text "Next" ]
            ]
        )


type Jump
    = Absolute Int
    | Relative Int


viewPaginatedEntry :
    PaginatedList a
    -> (Msg -> msg)
    -> Jump
    -> Maybe (Html msg)
viewPaginatedEntry paginated wrapper jump =
    let
        destination =
            case jump of
                Absolute i ->
                    i

                Relative i ->
                    currentPage paginated + i

        pageText =
            String.fromInt destination

        click =
            wrapper (PaginationRequest (translateBack destination))
    in
    if isPageValid paginated destination then
        Just <|
            li []
                [ a
                    [ attribute "aria-label" ("Page " ++ pageText)
                    , onClick click
                    ]
                    [ text pageText ]
                ]

    else
        Nothing


viewEllipsis : PaginatedList a -> Maybe (Html msg)
viewEllipsis paginated =
    if numberOfPages paginated > 5 then
        Just (li [ class "ellipsis", attribute "aria-hidden" "true" ] [])

    else
        Nothing


{-| html render for paginated data
wrapper : Msg -> msg wraps our message type with the apps outer layer message
constructor
innerHtml : List a -> Html msg is the Html to be rendered for the paginated data
-}
viewPaginatedHtml :
    PaginatedList a
    -> (Msg -> msg)
    -> (List a -> Html msg)
    -> Html msg
viewPaginatedHtml paginated wrapper innerHtml =
    if numberOfPages paginated <= 1 then
        innerHtml (paginatedData paginated)

    else
        let
            current =
                currentPage paginated

            last =
                numberOfPages paginated
        in
        div []
            [ innerHtml (paginatedData paginated)
            , nav [ attribute "aria-label" "Pagination" ]
                [ ul [ class "pagination text-center" ]
                    (List.filterMap
                        identity
                        [ Just <|
                            viewPaginatedPrevious paginated wrapper
                        , Just <|
                            li [ class "current" ]
                                [ span [ class "show-for-sr" ]
                                    [ text "You're on page" ]
                                , text (String.fromInt current)
                                ]
                        , viewPaginatedEntry paginated wrapper (Relative 1)
                        , viewPaginatedEntry paginated wrapper (Relative 2)
                        , viewPaginatedEntry paginated wrapper (Relative 3)
                        , viewEllipsis paginated
                        , viewPaginatedEntry paginated wrapper (Absolute (last - 1))
                        , viewPaginatedEntry paginated wrapper (Absolute last)
                        , Just <| viewPaginatedNext paginated wrapper
                        ]
                    )
                ]
            ]
