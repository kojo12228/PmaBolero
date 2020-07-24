module PmaBolero.Client.ViewProject

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.Models
open PmaBolero.Client.Models.EmployeeData

type Model =
    {
        SignInRole: Auth.Role option
        TileModel: ViewSingle.Model<Project>
    }

let initModel: Model =
    {
        SignInRole = None
        TileModel =
            {
                DataType = "Projects"
                UrlPrefix = "project"
                IsLoading = true
                Data = None
                AuthorisationFailure = false
                Error = None
            }
    }

type Message =
    | InitMessage of int * (Auth.Role option)
    | TileMessage of ViewSingle.Message<Project>

let update remote message model =
    let getDataFunc = remote.getProject

    match message with
    | InitMessage (projId, roleOpt) ->
        let tileModel, tileCmd =
            ViewSingle.update
                getDataFunc
                (ViewSingle.InitMessage projId)
                model.TileModel

        { model with SignInRole = roleOpt; TileModel = tileModel }, Cmd.map TileMessage tileCmd
    | TileMessage msg ->
        let getDataFunc = remote.getProject

        ViewSingle.update getDataFunc msg model.TileModel
        |> fun (tileModel, cmd) ->
            { model with TileModel = tileModel },
            Cmd.map TileMessage cmd

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

let generateTile signInRole (project: Project) =
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
        .Id(string project.Id)
        .EditDisabled(
            match signInRole with
            | Some Auth.Admin | Some Auth.ProjectManager -> false
            | _ -> true
        )
        .Elt()

let view (model: Model) dispatch =
    let projectTitle (proj: Project) = proj.Name

    let mappedDispatch = TileMessage >> dispatch
    ViewSingle.view (generateTile model.SignInRole) projectTitle model.TileModel mappedDispatch