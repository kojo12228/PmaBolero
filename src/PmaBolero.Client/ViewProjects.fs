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
        IsLoading: bool
        Projects: Project[]
        AuthorisationFailure: bool
        Error: string option
    }

let initModel =
    {
        IsLoading = false
        Projects = [||]
        AuthorisationFailure = false
        Error = None
    }

type Message =
    | InitMessage
    | GetProjects
    | RecvProjects of option<Project[]>
    | Error of exn

let update remote message model =
    match message with
    | InitMessage ->
        { model with IsLoading = true }, Cmd.ofMsg GetProjects
    | GetProjects ->
        model, Cmd.ofAuthorized remote.getProjects () RecvProjects Error
    | RecvProjects (Some projs) ->
        { model with Projects = projs; IsLoading = false }, Cmd.none
    | RecvProjects None ->
        { model with AuthorisationFailure = true }, Cmd.none
    | Error e ->
        { model with Error = Some e.Message }, Cmd.none

type ViewProjectsPage = Template<"wwwroot/viewprojects.html">

let populateDevs (devNames: (int * string) []) =
    ViewProjectsPage
        .DevList()
        .DevItems(
            cond (Array.isEmpty devNames) <| function
            | true ->
                ViewProjectsPage
                    .NoDevs()
                    .Elt()
            | false ->
                forEach devNames (fun (_, name) ->
                    ViewProjectsPage
                        .DevItem()
                        .Name(name)
                        .Elt()
                )
        )
        .Elt()

let viewPm (pmOpt: (int * string) option) =
    cond pmOpt <| function
    | Some (_, name) ->
        ViewProjectsPage
            .PmExists()
            .PmName(name)
            .Elt()
    | None ->
        ViewProjectsPage
            .NoPm()
            .Elt()

let viewProjectTile (project: Project) =
    ViewProjectsPage
        .Tile()
        .Name(project.Name)
        .Status(project.Status |> string)
        .DepartmentName(project.DepartmentId |> snd)
        .Skills(String.Join(", ", project.SkillRequirements))
        .Description(project.Description)
        .ProjectManager(viewPm project.ProjectManagerId)
        .Devs(populateDevs project.DeveloperIds)
        .Elt()

let view model dispatch =
    ViewProjectsPage
        .ProjTemplate()
        .Progress(
            cond model.IsLoading <| function
            | false -> empty
            | true ->
                ViewProjectsPage.DisplayProgress().Elt()
        )
        .Tiles(
            forEach model.Projects viewProjectTile
        )
        .Elt()