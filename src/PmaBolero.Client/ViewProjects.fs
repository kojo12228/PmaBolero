module PmaBolero.Client.ViewProjects

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.Models
open PmaBolero.Client.Models.EmployeeData

type Model = TilesTemplate.Model<Project>

let initModel: Model =
    {
        Title = "Projects"
        IsLoading = false
        Data = [||]
        AuthorisationFailure = false
        Error = None
    }

type Message =
    | InitMessage
    | TilesMessage of TilesTemplate.Message<Project>

let update remote (message: Message) (model: Model) =
    let getDataFunc = remote.getProjects

    let tilesMsg =
        match message with
        | InitMessage -> TilesTemplate.InitMessage
        | TilesMessage msg -> msg

    let updatedModel, cmd = TilesTemplate.update getDataFunc tilesMsg model
    updatedModel, Cmd.map TilesMessage cmd

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

let generateTile (project: Project) =
    ViewProjectsPage
        .ProjectTile()
        .Name(project.Name)
        .Status(project.Status |> string)
        .DepartmentName(project.DepartmentId |> snd)
        .Skills(String.Join(", ", project.SkillRequirements))
        .Description(project.Description)
        .ProjectManager(viewPm project.ProjectManagerId)
        .Devs(populateDevs project.DeveloperIds)
        .Elt()

let view (model: Model) dispatch =
    let mappedDispatch = TilesMessage >> dispatch
    TilesTemplate.view generateTile model mappedDispatch