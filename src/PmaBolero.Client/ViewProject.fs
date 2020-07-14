module PmaBolero.Client.ViewProject

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.Models.EmployeeData

type Model = ViewSingle.Model<Project>

let initModel: Model =
    {
        DataType = "Projects"
        UrlPrefix = "project"
        IsLoading = true
        Data = None
        AuthorisationFailure = false
        Error = None
    }

type Message =
    | InitMessage of int
    | TileMessage of ViewSingle.Message<Project>

let update remote message model =
    let getDataFunc = remote.getProject

    let tileMsg =
        match message with
        | InitMessage projId -> ViewSingle.InitMessage projId
        | TileMessage msg -> msg

    ViewSingle.update getDataFunc tileMsg model
    |> fun (model, cmd) -> model, Cmd.map TileMessage cmd

type ViewProjectPage = Template<"wwwroot/viewproject.html">

let populateSkills (skills: string []) =
    ViewProjectPage
        .SkillList()
        .Items(
            forEach skills (fun skill ->
                ViewProjectPage
                    .SkillItem()
                    .Skill(skill)
                    .Elt()
                )
        )
        .Elt()

let populateDevs (devs: (int * string) []) =
    ViewProjectPage
        .DevList()
        .DevItems(
            forEach devs (fun (devId, devName) ->
                ViewProjectPage
                    .DevItem()
                    .Id(string devId)
                    .Name(devName)
                    .Elt()
            )
        )
        .Elt()

let generateTile (project: Project) =
    ViewProjectPage
        .Tile()
        .DeptId(project.DepartmentId |> fst |> string)
        .DepartmentName(project.DepartmentId |> snd)
        .Status(string project.Status)
        .Skills(
            cond (Array.isEmpty project.SkillRequirements) <| function
            | false -> populateSkills project.SkillRequirements
            | true -> ViewProjectPage.NoSkills().Elt()
        )
        .Description(project.Description)
        .ProjectManager(
            cond project.ProjectManagerId <| function
            | None -> ViewProjectPage.NoPm().Elt()
            | Some (pmId, pmName) ->
                ViewProjectPage
                    .PmExists()
                    .Id(string pmId)
                    .PmName(pmName)
                    .Elt()
        )
        .Devs(
            cond (Array.isEmpty project.DeveloperIds) <| function
            | false -> populateDevs project.DeveloperIds
            | true -> ViewProjectPage.NoDevs().Elt()
        )
        .Elt()

let view (model: Model) dispatch =
    let projectTitle (proj: Project) = proj.Name

    let mappedDispatch = TileMessage >> dispatch
    ViewSingle.view generateTile projectTitle model mappedDispatch