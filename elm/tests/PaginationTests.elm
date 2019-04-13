module PaginationTests exposing (suite)

import Expect exposing (Expectation)
import Json.Decode as Json
import Pagination
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (class, tag, text)


testLink label query =
    test (label ++ " is a link") <|
        \_ ->
            query
                |> Query.find
                    [ tag "li"
                    , Selector.containing [ text label ]
                    ]
                |> Query.has [ tag "a" ]
                |> Expect.onFail (label ++ " was expected to be a link")


testNotLink label query =
    test (label ++ " is not a link") <|
        \_ ->
            query
                |> Query.find
                    [ tag "li"
                    , Selector.containing [ text label ]
                    ]
                |> Query.hasNot [ tag "a" ]
                |> Expect.onFail (label ++ " was expected not to be a link")


suite : Test
suite =
    describe "Pagination"
        [ test "bad values from server don't parse" <|
            \_ ->
                Expect.err <|
                    Json.decodeString
                        Pagination.jsonDecoder
                        "{\"offset\":10,\"count\":5}"
        , describe "1-1000 @ 1" <|
            let
                pagination =
                    Json.decodeString Pagination.jsonDecoder
                        "{\"offset\":0,\"count\":1000}"

                view =
                    Result.map Pagination.view pagination

                eQuery =
                    Result.map Query.fromHtml view
            in
            case eQuery of
                Ok query ->
                    [ test "has Previous" <|
                        \_ -> Query.has [ text "Previous" ] query
                    , test "has 1" <| \_ -> Query.has [ text "1" ] query
                    , test "has 2" <| \_ -> Query.has [ text "2" ] query
                    , test "has 3" <| \_ -> Query.has [ text "3" ] query
                    , test "doesn't have 4" <|
                        \_ -> Query.hasNot [ text "4" ] query
                    , test "has ..." <|
                        \_ -> Query.has [ class "ellipsis" ] query
                    , test "doesn't have 49" <|
                        \_ -> Query.hasNot [ text "49" ] query
                    , test "has 50" <| \_ -> Query.has [ text "50" ] query
                    , test "doesn't have 51" <|
                        \_ -> Query.hasNot [ text "51" ] query
                    , test "has Next" <| \_ -> Query.has [ text "Next" ] query
                    , testNotLink "Previous" query
                    , testNotLink "1" query
                    , testLink "2" query
                    , testLink "3" query
                    , testLink "50" query
                    , testLink "next" query
                    ]

                Err _ ->
                    [ test "pagination was parsed from json" <|
                        \_ -> Expect.fail "nope"
                    ]
        , describe "1-1000 @ 100" <|
            let
                pagination =
                    Json.decodeString Pagination.jsonDecoder
                        "{\"offset\":999,\"count\":1000}"

                view =
                    Result.map Pagination.view pagination

                eQuery =
                    Result.map Query.fromHtml view
            in
            case eQuery of
                Ok query ->
                    [ test "has Previous" <|
                        \_ -> Query.has [ text "Previous" ] query
                    , test "has 1" <| \_ -> Query.has [ text "1" ] query
                    , test "doesn't have 2" <|
                        \_ -> Query.hasNot [ text "2" ] query
                    , test "has ..." <|
                        \_ -> Query.has [ class "ellipsis" ] query
                    , test "doesn't have 48" <|
                        \_ -> Query.hasNot [ text "48" ] query
                    , test "has 49" <| \_ -> Query.has [ text "49" ] query
                    , test "has 50" <| \_ -> Query.has [ text "50" ] query
                    , test "doesn't have 51" <|
                        \_ -> Query.hasNot [ text "51" ] query
                    , test "has Next" <| \_ -> Query.has [ text "Next" ] query
                    , testLink "Previous" query
                    , testLink "1" query
                    , testLink "49" query
                    , testNotLink "50" query
                    , testNotLink "next" query
                    ]

                Err _ ->
                    [ test "pagination was parsed from json" <|
                        \_ -> Expect.fail "nope"
                    ]
        , describe "1-1000 @ 500" <|
            let
                pagination =
                    Json.decodeString Pagination.jsonDecoder
                        "{\"offset\":500,\"count\":1000}"

                view =
                    Result.map Pagination.view pagination

                eQuery =
                    Result.map Query.fromHtml view
            in
            case eQuery of
                Ok query ->
                    [ test "has Previous" <|
                        \_ -> Query.has [ text "Previous" ] query
                    , test "has 1" <| \_ -> Query.has [ text "1" ] query
                    , test "doesn't have  2" <|
                        \_ -> Query.hasNot [ text "2" ] query
                    , test "has ..." <|
                        \_ -> Query.has [ class "ellipsis" ] query
                    , test "doesn't have 23" <|
                        \_ -> Query.hasNot [ text "23" ] query
                    , test "has 24" <| \_ -> Query.has [ text "24" ] query
                    , test "has 25" <| \_ -> Query.has [ text "25" ] query
                    , test "has 26" <| \_ -> Query.has [ text "26" ] query
                    , test "has 27" <| \_ -> Query.has [ text "27" ] query
                    , test "doesn't have 28" <|
                        \_ -> Query.hasNot [ text "28" ] query
                    , test "doesn't have 49" <|
                        \_ -> Query.hasNot [ text "49" ] query
                    , test "has 50" <| \_ -> Query.has [ text "50" ] query
                    , test "doesn't have 51" <|
                        \_ -> Query.hasNot [ text "51" ] query
                    , test "has Next" <| \_ -> Query.has [ text "Next" ] query
                    , testLink "Previous" query
                    , testLink "1" query
                    , testLink "24" query
                    , testNotLink "25" query
                    , testLink "26" query
                    , testLink "27" query
                    , testLink "50" query
                    , testLink "next" query
                    ]

                Err _ ->
                    [ test "pagination was parsed from json" <|
                        \_ -> Expect.fail "nope"
                    ]
        , describe "1-10 @ 5" <|
            let
                pagination =
                    Json.decodeString Pagination.jsonDecoder
                        "{\"offset\":5,\"count\":10}"

                view =
                    Result.map Pagination.view pagination

                eQuery =
                    Result.map Query.fromHtml view
            in
            case eQuery of
                Ok query ->
                    [ test "doesn't have Previous" <|
                        \_ -> Query.hasNot [ text "Previous" ] query
                    , test "doesn't have -1" <|
                        \_ -> Query.hasNot [ text "-1" ] query
                    , test "doesn't have 0" <|
                        \_ -> Query.hasNot [ text "0" ] query
                    , test "doesn't have 1" <|
                        \_ -> Query.hasNot [ text "1" ] query
                    , test "doesn't have 2" <|
                        \_ -> Query.hasNot [ text "Next" ] query
                    ]

                Err _ ->
                    [ test "pagination was parsed from json" <|
                        \_ -> Expect.fail "nope"
                    ]
        , describe "1-40 @ 20" <|
            let
                pagination =
                    Json.decodeString Pagination.jsonDecoder
                        "{\"offset\":20,\"count\":40}"

                view =
                    Result.map Pagination.view pagination

                eQuery =
                    Result.map Query.fromHtml view
            in
            case eQuery of
                Ok query ->
                    [ test "has Previous" <|
                        \_ -> Query.has [ text "Previous" ] query
                    , test "has 1" <| \_ -> Query.has [ text "1" ] query
                    , test "doesn't have ..." <|
                        \_ -> Query.hasNot [ class "ellipsis" ] query
                    , test "has 2" <| \_ -> Query.has [ text "2" ] query
                    , test "has Next" <| \_ -> Query.has [ text "Next" ] query
                    , testLink "Previous" query
                    , testLink "1" query
                    , testNotLink "2" query
                    , testNotLink "Next" query
                    ]

                Err _ ->
                    [ test "pagination was parsed from json" <|
                        \_ -> Expect.fail "nope"
                    ]
        ]
