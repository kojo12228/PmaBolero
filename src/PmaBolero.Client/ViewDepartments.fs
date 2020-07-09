module PmaBolero.Client.ViewDepartments

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
        Departments: Department[]
    }

let initModel =
    {
        Departments = [||]
    }

type Message =
    | Placeholder

let update remote message model =
    model, Cmd.none

type ViewDepartmentsPage = Template<"wwwroot/viewdepartments.html">

let view model dispatch =
    ViewDepartmentsPage().Elt()