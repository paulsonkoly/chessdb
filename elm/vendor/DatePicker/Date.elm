module DatePicker.Date exposing
    ( YearRange(..)
    , changeYear
    , formatDay
    , formatMonth
    , initDate
    , weekdayToInterval
    , yearRange
    )

import Date exposing (Date, Interval(..), Unit(..), day, month, year)
import Time exposing (Month(..), Weekday(..))


type alias Year =
    Int


type alias Day =
    Int


type YearRange
    = Off
    | MoreOrLess Int
    | Between Year Year
    | From Year
    | To Year


initDate : Date
initDate =
    Date.fromCalendarDate 1992 May 31


formatDay : Date.Weekday -> String
formatDay day =
    case day of
        Mon ->
            "Mo"

        Tue ->
            "Tu"

        Wed ->
            "We"

        Thu ->
            "Th"

        Fri ->
            "Fr"

        Sat ->
            "Sa"

        Sun ->
            "Su"


formatMonth : Month -> String
formatMonth month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


weekdayToInterval : Weekday -> Date.Interval
weekdayToInterval weekday =
    case weekday of
        Mon ->
            Monday

        Tue ->
            Tuesday

        Wed ->
            Wednesday

        Thu ->
            Thursday

        Fri ->
            Friday

        Sat ->
            Saturday

        Sun ->
            Sunday


changeYear : Date -> String -> Date
changeYear current newYear =
    case String.toInt newYear of
        Just year ->
            Date.fromCalendarDate year (month current) (day current)

        Nothing ->
            -- We couldn't decode the year to change to... do nothing!
            current


yearRange : { currentMonth : Date, today : Date } -> YearRange -> List Int
yearRange { currentMonth, today } range =
    case range of
        MoreOrLess num ->
            List.range (year currentMonth - num) (year currentMonth + num)

        Between start end ->
            List.range start end

        From year_ ->
            List.range year_ (year today)

        To year_ ->
            List.range (year today) year_

        Off ->
            []
