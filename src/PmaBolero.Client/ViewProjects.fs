module PmaBolero.Client.ViewProjects

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.Models.EmployeeData

type Model =
    {
        Projects: Project[]
    }

let initModel =
    {
        Projects = [||]
    }

type Message =
    | Placeholder

let update remote message model =
    model, Cmd.none

type ViewProjectsPage = Template<"wwwroot/viewprojects.html">

let view model dispatch =
    ViewProjectsPage().Elt()