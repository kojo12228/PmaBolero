module PmaBolero.Client.Pages.ViewGroup.Project

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

type Model =
    { 
        SignInRole: Role option
        TilesModel: ViewGroup.Model<Project>
    }

let initModel: Model =
    {
        SignInRole = None
        TilesModel =
            {
                Title = "Projects"
                IsLoading = false
                Data = [||]
                AuthorisationFailure = false
                Error = None
            }
    }

type Message =
    | InitMessage of Role option
    | DeleteProject of int
    | DeleteReturn of int option option
    | TilesMessage of ViewGroup.Message<Project>

let update remote (message: Message) (model: Model) =
    match message with
    | InitMessage role ->
        { model with SignInRole = role }, Cmd.ofMsg (TilesMessage ViewGroup.InitMessage)
    | DeleteProject projId ->
        model, Cmd.OfAuthorized.either remote.deleteProject projId DeleteReturn (TilesMessage << ViewGroup.Error)
    | DeleteReturn _ ->
        model, Cmd.ofMsg (TilesMessage ViewGroup.InitMessage)
    | TilesMessage msg ->
        let getDataFunc = remote.getProjects

        let updatedModel, cmd = ViewGroup.update getDataFunc msg model.TilesModel
        { model with TilesModel = updatedModel }, Cmd.map TilesMessage cmd

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
                forEach devNames (fun (devId, name) ->
                    ViewProjectsPage
                        .DevItem()
                        .Id(string devId)
                        .Name(name)
                        .Elt()
                )
        )
        .Elt()

let viewPm (pmOpt: (int * string) option) =
    cond pmOpt <| function
    | Some (pmId, name) ->
        ViewProjectsPage
            .PmExists()
            .Id(string pmId)
            .PmName(name)
            .Elt()
    | None ->
        ViewProjectsPage
            .NoPm()
            .Elt()

let generateTile signInRole dispatch (project: Project) =
    ViewProjectsPage
        .ProjectTile()
        .Id(string project.Id)
        .Name(project.Name)
        .Status(project.Status |> string)
        .DeptId(project.DepartmentId |> fst |> string)
        .DepartmentName(project.DepartmentId |> snd)
        .Skills(String.Join(", ", project.SkillRequirements))
        .Description(project.Description)
        .ProjectManager(viewPm project.ProjectManagerId)
        .Devs(populateDevs project.DeveloperIds)
        .EditDisable(
            match signInRole with
            | Some Admin | Some ProjectManager -> false
            | _ -> true
        )
        .DeleteDisable(
            match signInRole with
            | Some Admin -> false
            | _ -> true
        )
        .DeleteClick(fun _ -> dispatch (DeleteProject project.Id))
        .Elt()

let view (model: Model) dispatch =
    let mappedDispatch = TilesMessage >> dispatch
    ViewGroup.view (generateTile model.SignInRole dispatch) model.TilesModel mappedDispatch