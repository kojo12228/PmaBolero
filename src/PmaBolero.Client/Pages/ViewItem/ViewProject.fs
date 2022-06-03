module PmaBolero.Client.Pages.ViewItem.Project

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
      TileModel: ViewItem.Model<Project> }

let initModel: Model =
    { SignInRole = None
      TileModel =
        { DataType = "Projects"
          UrlPrefix = "project"
          IsLoading = true
          Data = None
          AuthorisationFailure = false
          Error = None } }

type Message =
    | InitMessage of int * (Role option)
    | TileMessage of ViewItem.Message<Project>

let update remote message model =
    let getDataFunc = remote.getProject

    match message with
    | InitMessage (projId, roleOpt) ->
        let tileModel, tileCmd =
            ViewItem.update getDataFunc (ViewItem.InitMessage projId) model.TileModel

        { model with
            SignInRole = roleOpt
            TileModel = tileModel },
        Cmd.map TileMessage tileCmd
    | TileMessage msg ->
        let getDataFunc = remote.getProject

        ViewItem.update getDataFunc msg model.TileModel
        |> fun (tileModel, cmd) -> { model with TileModel = tileModel }, Cmd.map TileMessage cmd

type ViewProjectPage = Template<"wwwroot/viewproject.html">

let viewSkills (skills: string []) =
    cond (Array.isEmpty skills)
    <| function
        | false ->
            concat' [] [
                p [] [ text "Skills Required: " ]
                div [ attr.``class`` "content" ] [
                    ul [] [
                        forEach skills
                        <| fun skill -> li [] [ text skill ]
                    ]
                ]
            ]
        | true -> p [] [ text "No skills required" ]

let viewDevs (devs: (int * string) []) =
    cond (Array.isEmpty devs)
    <| function
        | false ->
            concat' [] [
                p [] [
                    strong [] [ text "Developers:" ]
                ]
                div [ attr.``class`` "content" ] [
                    ul [] [
                        forEach devs
                        <| fun (devId, devName) ->
                            li [] [
                                a [ attr.href $"/project/{devId}" ] [
                                    text devName
                                ]
                            ]
                    ]
                ]
            ]
        | true ->
            p [] [
                text "Project has no developers."
            ]

let viewPm (pmOpt: (int * string) option) =
    cond pmOpt
    <| function
        | Some (pmId, pmName) ->
            concat' [] [
                p [] [ text "Project Manager:" ]
                a [ attr.href $"/employee/{pmId}" ] [
                    text pmName
                ]
            ]
        | None -> p [] [ text "No PM assigned" ]

let generateTile signInRole (project: Project) =
    concat' [] [
        div [ attr.classes [ "tags"
                             "are-medium"
                             "has-addons" ] ] [
            span [ attr.``class`` "tag" ] [
                text "Status"
            ]
            span [ attr.classes [ "tag"; "is-primary" ] ] [
                project.Status |> string |> text
            ]
        ]

        div [ attr.classes [ "tags"
                             "are-medium"
                             "has-addons" ] ] [
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

        viewSkills project.SkillRequirements

        hr []

        p [] [ text project.Description ]

        hr []

        viewPm project.ProjectManagerId
        viewDevs project.DeveloperIds

        a [ attr.classes [ "button"
                           "is-primary"
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

let view (model: Model) dispatch =
    let projectTitle (proj: Project) = proj.Name

    let mappedDispatch = TileMessage >> dispatch
    ViewItem.view (generateTile model.SignInRole) projectTitle model.TileModel mappedDispatch
