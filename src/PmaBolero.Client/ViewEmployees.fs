module PmaBolero.Client.ViewEmployees

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
        Employees: Employee[]
    }

let initModel =
    {
        Employees = [||]
    }

type Message =
    | Placeholder

let update remote message model =
    model, Cmd.none

type ViewEmployeesPage = Template<"wwwroot/viewemployees.html">

let view model dispatch =
    ViewEmployeesPage().Elt()