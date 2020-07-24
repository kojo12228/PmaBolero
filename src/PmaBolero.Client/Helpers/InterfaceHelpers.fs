namespace PmaBolero.Client.Helpers

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Microsoft.AspNetCore.Components.Web

type HelperTemplate = Template<"wwwroot/helpers.html">

module ErrorNotification =
    let private errorNotif (msg: string) hideEvent level =
        let event = Action<MouseEventArgs>(hideEvent)
    
        HelperTemplate
            .ErrorNotification()
            .Level(level)
            .Text(msg)
            .Hide(event)
            .Elt()

    let errorNotifWarning msg hideEvent =
        errorNotif msg hideEvent "is-warning"

    let errorNotifDanger msg hideEvent =
        errorNotif msg hideEvent "is-danger"

    let errorNotifSuccess msg hideEvent =
        errorNotif msg hideEvent "is-success"

module ProgressBar =
    let createIndeterminateBar() =
        HelperTemplate
            .IndeterminateProgressBar()
            .Elt()

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
        HelperTemplate
            .DeterminateProgressBar()
            .BarValue(loadingToVal value)
            .Elt()