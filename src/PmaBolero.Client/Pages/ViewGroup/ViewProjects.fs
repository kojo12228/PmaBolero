module PmaBolero.Client.Pages.ViewGroup.Project

open Elmish
open Bolero.Html
open Bolero.Remoting.Client

open PmaBolero.Shared.Models

open PmaBolero.Client.Models.EmployeeData
open PmaBolero.Client.Helpers

type Model =
    { SignInRole: Role option
      TilesModel: ViewGroup.Model<Project> }

let initModel: Model =
    { SignInRole = None
      TilesModel =
        { Title = "Projects"
          IsLoading = false
          Data = [||]
          AuthorisationFailure = false
          Error = None } }

type Message =
    | InitMessage of Role option
    | DeleteProject of int
    | DeleteReturn of int option option
    | TilesMessage of ViewGroup.Message<Project>

let update remote (message: Message) (model: Model) =
    match message with
    | InitMessage role -> { model with SignInRole = role }, Cmd.ofMsg (TilesMessage ViewGroup.InitMessage)
    | DeleteProject projId ->
        model, Cmd.OfAuthorized.either remote.deleteProject projId DeleteReturn (TilesMessage << ViewGroup.Error)
    | DeleteReturn _ -> model, Cmd.ofMsg (TilesMessage ViewGroup.InitMessage)
    | TilesMessage msg ->
        let getDataFunc = remote.getProjects

        let updatedModel, cmd = ViewGroup.update getDataFunc msg model.TilesModel
        { model with TilesModel = updatedModel }, Cmd.map TilesMessage cmd

let viewDevs (devNames: (int * string) []) =
    cond (Array.isEmpty devNames)
    <| function
        | false ->
            concat' [] [
                p [] [ text "Developers: " ]
                div [ attr.``class`` "content" ] [
                    ul [] [
                        forEach devNames
                        <| fun (devId, devName) ->
                            li [] [
                                a [ attr.href $"/employee/{devId}" ] [
                                    text devName
                                ]
                            ]
                    ]
                ]
            ]
        | true ->
            p [] [
                text "Project has no developers"
            ]

let viewPm (pmOpt: (int * string) option) =
    cond pmOpt
    <| function
        | Some (pmId, name) ->
            concat' [] [
                p [] [ text "Project Manager:" ]
                a [ attr.href $"/employee/{pmId}" ] [
                    text name
                ]
            ]
        | None -> p [] [ text "No PM assigned" ]

let generateTile signInRole dispatch (project: Project) =
    concat' [] [
        a [ attr.``class`` "subtitle"
            attr.href $"/project/{project.Id}" ] [
            strong [] [ text project.Name ]
        ]

        div [ attr.classes [ "field"
                             "is-grouped"
                             "is-grouped-multiline" ] ] [
            div [ attr.``class`` "control" ] [
                div [ attr.classes [ "tags"; "has-addons" ] ] [
                    span [ attr.``class`` "tag" ] [
                        text "Status"
                    ]
                    span [ attr.classes [ "tag"; "is-primary" ] ] [
                        project.Status |> string |> text
                    ]
                ]
            ]

            div [ attr.``class`` "control" ] [
                div [ attr.classes [ "tags"; "has-addons" ] ] [
                    span [ attr.``class`` "tag" ] [
                        text "Department"
                    ]
                    a [ attr.classes [ "tag"
                                       "is-link"
                                       "is-primary" ]
                        attr.href $"/department/{project.DepartmentId |> fst |> string}" ] [
                        project.DepartmentId |> snd |> text
                    ]
                ]
            ]
        ]

        p [ attr.``class`` "small" ] [
            project.SkillRequirements
            |> String.concat ", "
            |> sprintf "Skills Required: %s"
            |> text
        ]

        hr []

        p [] [ text project.Description ]

        hr []

        div [ attr.``class`` "columns" ] [
            div [ attr.``class`` "column" ] [
                viewPm project.ProjectManagerId
            ]

            div [ attr.``class`` "column" ] [
                viewDevs project.DeveloperIds
            ]
        ]

        div [ attr.``class`` "columns" ] [
            div [ attr.``class`` "column" ] [
                a [ attr.classes [ "button"
                                   "is-primary"
                                   "is-fullwidth" ]
                    attr.href $"/project/{project.Id}" ] [
                    text "View"
                ]
            ]
            div [ attr.``class`` "column" ] [
                a [ attr.classes [ "button"
                                   "is-fullwidth" ]
                    attr.href $"/project/{project.Id}/edit"

                    match signInRole with
                    | Some Admin
                    | Some ProjectManager -> false
                    | _ -> true
                    |> attr.disabled ] [
                    text "Edit"
                ]
            ]

            div [ attr.``class`` "column" ] [
                button [ attr.classes [ "button"
                                        "is-warning"
                                        "is-fullwidth" ]
                         on.click (fun _ -> dispatch (DeleteProject project.Id))

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
