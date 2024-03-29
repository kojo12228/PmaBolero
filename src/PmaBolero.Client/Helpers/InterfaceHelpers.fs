namespace PmaBolero.Client.Helpers

open Bolero.Html

[<AutoOpen>]
module FantomasHelpers =
    let concat' (unusedList: 'a list) children = concat children

module Forms =
    let inputWithLabel labelText (inputType: string) inputValue setValue =
        div [ attr.``class`` "field" ] [
            label [ attr.``class`` "label" ] [
                text labelText
            ]
            div [ attr.``class`` "control" ] [
                input [ attr.``class`` "input"
                        attr.``type`` inputType
                        bind.input.string inputValue setValue ]
            ]
        ]

module ErrorNotification =
    let private errorNotif (msg: string) hideEvent level =
        div [ attr.classes [ "notification"; level ] ] [
            button [ attr.``class`` "delete"
                     on.click hideEvent ] [
                text msg
            ]
        ]

    let errorNotifWarning msg hideEvent = errorNotif msg hideEvent "is-warning"

    let errorNotifDanger msg hideEvent = errorNotif msg hideEvent "is-danger"

    let errorNotifSuccess msg hideEvent = errorNotif msg hideEvent "is-success"

module ProgressBar =
    let createIndeterminateBar () =
        progress [ attr.classes [ "progress"
                                  "is-medium"
                                  "is-dark" ] ] [
            text "45%"
        ]

    type LoadingStatus =
        | LoadingEmpty
        | LoadingQuarter
        | LoadingHalf
        | LoadingThreeQuarter
        | LoadingComplete

    let private loadingToVal load =
        match load with
        | LoadingEmpty -> "0"
        | LoadingQuarter -> "25"
        | LoadingHalf -> "50"
        | LoadingThreeQuarter -> "75"
        | LoadingComplete -> "100"

    let loadingNextQuarter load =
        match load with
        | LoadingEmpty -> LoadingQuarter
        | LoadingQuarter -> LoadingHalf
        | LoadingHalf -> LoadingThreeQuarter
        | LoadingThreeQuarter -> LoadingComplete
        | LoadingComplete -> LoadingComplete

    let loadingNextHalf load =
        match load with
        | LoadingEmpty -> LoadingHalf
        | LoadingQuarter -> LoadingHalf
        | LoadingHalf -> LoadingComplete
        | LoadingThreeQuarter -> LoadingComplete
        | LoadingComplete -> LoadingComplete

    let createDeterminateBar (value: LoadingStatus) =
        let barValue = loadingToVal value

        progress [ attr.classes [ "progress"
                                  "is-medium"
                                  "is-dark" ]
                   attr.value barValue
                   attr.max "100" ] [
            text $"{barValue}%%"
        ]
