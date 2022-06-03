module PmaBolero.Client.Pages.ViewItem.Employee

open Elmish
open Bolero.Html
open Bolero.Remoting.Client

open PmaBolero.Shared.Models

open PmaBolero.Client.Models.EmployeeData
open PmaBolero.Client.Helpers

type Model =
    { SignInRole: Role option
      TileModel: ViewItem.Model<Employee> }

let initModel: Model =
    { SignInRole = None
      TileModel =
        { DataType = "Employees"
          UrlPrefix = "employee"
          IsLoading = true
          Data = None
          AuthorisationFailure = false
          Error = None } }

type Message =
    | InitMessage of int * (Role option)
    | TileMessage of ViewItem.Message<Employee>

let update remote message model =
    let getDataFunc = remote.getEmployee

    match message with
    | InitMessage (emplId, roleOpt) ->
        let tileModel, tileMsg =
            ViewItem.update getDataFunc (ViewItem.InitMessage emplId) model.TileModel

        { model with
            SignInRole = roleOpt
            TileModel = tileModel },
        Cmd.map TileMessage tileMsg
    | TileMessage msg ->
        ViewItem.update getDataFunc msg model.TileModel
        |> fun (tileModel, cmd) -> { model with TileModel = tileModel }, Cmd.map TileMessage cmd

let viewSkills (skills: string []) =
    cond (Array.isEmpty skills)
    <| function
        | false ->
            concat' [] [
                p [] [ text "Skills:" ]
                div [ attr.``class`` "content" ] [
                    ul [] [
                        forEach skills
                        <| fun (skill) -> li [] [ text skill ]
                    ]
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
                    ul [] [
                        forEach projects
                        <| fun (projId, projName) ->
                            li [] [
                                a [ attr.href $"/project/{projId}" ] [
                                    text projName
                                ]
                            ]
                    ]
                ]
            ]
        | true ->
            p [] [
                text "No assigned to any projects."
            ]

let generateTile signInRole (employee: Employee) =
    concat' [] [
        a [ attr.``class`` "subtitle"
            attr.href $"/employee/{employee.Id}" ] [
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

        viewSkills employee.Skills
        viewProjects employee.ProjectIds

        a [ attr.classes [ "button"
                           "is-primary"
                           "is-fullwidth" ]
            attr.href $"/employee/{employee.Id}/edit"

            match signInRole with
            | Some Admin -> false
            | _ -> true
            |> attr.disabled ] [
            text "Edit"
        ]
    ]

let view (model: Model) dispatch =
    let employeeTitle (empl: Employee) = empl.FullName

    let mappedDispatch = TileMessage >> dispatch
    ViewItem.view (generateTile model.SignInRole) employeeTitle model.TileModel mappedDispatch
