module PmaBolero.Client.Helpers.ErrorNotification

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Microsoft.AspNetCore.Components.Web

type ErrorNotif = Template<"wwwroot/errornotif.html">

let private errorNotif (msg: string) hideEvent level =
    let event = Action<MouseEventArgs>(hideEvent)
    
    ErrorNotif
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
