module PmaBolero.Client.Pages.ViewGroup.Employee

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Shared.Models

open PmaBolero.Client.Models
open PmaBolero.Client.Models.EmployeeData
open PmaBolero.Client.Helpers

type Model =
    { SignInRole: Role option
      TilesModel: ViewGroup.Model<Employee> }

let initModel: Model =
    { SignInRole = None
      TilesModel =
        { Title = "Employees"
          IsLoading = false
          Data = [||]
          AuthorisationFailure = false
          Error = None } }

type Message =
    | InitMessage of Role option
    | DeleteEmployee of int
    | DeleteReturn of option<option<int>>
    | TilesMessage of ViewGroup.Message<Employee>

let update remote (message: Message) (model: Model) =
    match message with
    | InitMessage role -> { model with SignInRole = role }, Cmd.ofMsg (TilesMessage ViewGroup.InitMessage)
    | DeleteEmployee emplId ->
        model, Cmd.OfAuthorized.either remote.deleteEmployee emplId DeleteReturn (TilesMessage << ViewGroup.Error)
    | DeleteReturn _ -> model, Cmd.ofMsg (TilesMessage ViewGroup.InitMessage)
    | TilesMessage msg ->
        let getDataFunc = remote.getEmployees

        let updatedModel, cmd = ViewGroup.update getDataFunc msg model.TilesModel
        { model with TilesModel = updatedModel }, Cmd.map TilesMessage cmd

type ViewEmployeesPage = Template<"wwwroot/viewemployees.html">

let viewSkills (skills: string []) =
    cond (Array.isEmpty skills)
    <| function
        | false ->
            concat' [] [
                p [] [ text "Skills:" ]
                div [ attr.``class`` "content" ] [
                    forEach skills
                    <| fun skill -> li [] [ text skill ]
                ]
            ]
        | true -> p [] [ text "No skills included." ]

let viewProjects (projects: (int * string) []) =
    cond (Array.isEmpty projects)
    <| function
        | false ->
            concat' [] [
                p [] [ text "Projects:" ]
                div [ attr.``class`` "content" ] [
                    forEach projects
                    <| fun (projId, projName) ->
                        li [] [
                            a [ attr.href $"/project/{projId}" ] [
                                text projName
                            ]
                        ]
                ]
            ]
        | true ->
            p [] [
                text " Not assigned to any projects."
            ]

let generateTile signInRole dispatch (employee: Employee) =
    concat' [] [
        a [ attr.href $"/employee/{employee.Id}"
            attr.``class`` "subtitle" ] [
            strong [] [ text employee.FullName ]
        ]

        p [ attr.``class`` "small" ] [
            strong [] [ text employee.Email ]
        ]
        p [ attr.``class`` "small" ] [
            text $"{employee.Role} | In "
            a [ attr.href $"/department/{employee.DepartmentID |> fst |> string}" ] [
                employee.DepartmentID |> snd |> text
            ]
        ]

        hr []

        div [ attr.``class`` "columns" ] [
            div [ attr.``class`` "column" ] [
                viewSkills employee.Skills
            ]

            div [ attr.``class`` "column" ] [
                viewProjects employee.ProjectIds
            ]
        ]

        div [ attr.``class`` "columns" ] [
            div [ attr.``class`` "column" ] [
                a [ attr.href $"/employee/{employee.Id}"
                    attr.classes [ "button"
                                   "is-primary"
                                   "is-fullwidth" ] ] [
                    text "View"
                ]
            ]

            div [ attr.``class`` "column" ] [
                a [ attr.href $"/employee/{employee.Id}/edit"
                    attr.classes [ "button"
                                   "is-fullwidth" ]

                    match signInRole with
                    | Some Admin -> false
                    | _ -> true
                    |> attr.disabled ] [
                    text "Edit"
                ]
            ]

            div [ attr.``class`` "column" ] [
                button [ attr.classes [ "button"
                                        "is-warning"
                                        "is-fullwidth" ]
                         on.click (fun _ -> dispatch (DeleteEmployee employee.Id))
                         match signInRole with
                         | Some Admin -> false
                         | _ -> true
                         |> attr.disabled ] [
                    text "Delete"
                ]
            ]
        ]
    ]

let view (model: Model) dispatch =
    let mappedDispatch = TilesMessage >> dispatch
    ViewGroup.view (generateTile model.SignInRole dispatch) model.TilesModel mappedDispatch
