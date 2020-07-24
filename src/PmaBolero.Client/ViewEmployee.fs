module PmaBolero.Client.ViewEmployee

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
        TileModel: ViewSingle.Model<Employee>
    }

let initModel: Model =
    {
        SignInRole = None
        TileModel = 
            {
                DataType = "Employees"
                UrlPrefix = "employee"
                IsLoading = true
                Data = None
                AuthorisationFailure = false
                Error = None
            }
    }

type Message =
    | InitMessage of int * (Auth.Role option)
    | TileMessage of ViewSingle.Message<Employee>

let update remote message model =
    let getDataFunc = remote.getEmployee

    match message with
    | InitMessage (emplId, roleOpt) ->
        let tileModel, tileMsg = ViewSingle.update getDataFunc (ViewSingle.InitMessage emplId) model.TileModel
        {
            model with
                SignInRole = roleOpt
                TileModel = tileModel
        }, Cmd.map TileMessage tileMsg
    | TileMessage msg ->
        ViewSingle.update getDataFunc msg model.TileModel
        |> fun (tileModel, cmd) ->
            { model with TileModel = tileModel },
            Cmd.map TileMessage cmd

type ViewEmployeePage = Template<"wwwroot/viewemployee.html">

let populateSkills (skills: string []) =
    ViewEmployeePage
        .SkillList()
        .Items(
            forEach skills (fun skill ->
                ViewEmployeePage
                    .SkillItem()
                    .Skill(skill)
                    .Elt()
            )
        )
        .Elt()

let populateProjects (projects: (int * string) []) =
    ViewEmployeePage
        .ProjectList()
        .Items(
            forEach projects (fun (projId, name) ->
                ViewEmployeePage
                    .ProjectItem()
                    .Id(string projId)
                    .Name(name)
                    .Elt()
            )
        )
        .Elt()

let generateTile signInRole (employee: Employee) =
    ViewEmployeePage
        .Tile()
        .Id(string employee.Id)
        .Name(employee.FullName)
        .Email(employee.Email)
        .Role(string employee.Role)
        .DeptId(employee.DepartmentID |> fst |> string)
        .DepartmentName(employee.DepartmentID |> snd)
        .Skills(
            cond (Array.isEmpty employee.Skills) <| function
            | true ->
                ViewEmployeePage
                    .NoSkills()
                    .Elt()
            | false -> populateSkills employee.Skills)
        .Projects(
            cond (Array.isEmpty employee.ProjectIds) <| function
            | true ->
                ViewEmployeePage
                    .NoProjects()
                    .Elt()
            | false ->
                populateProjects employee.ProjectIds)
        .EditDisable(
            match signInRole with
            | Some Auth.Admin -> false
            | _ -> true
        )
        .Elt()

let view (model: Model) dispatch =
    let employeeTitle (empl: Employee) = empl.FullName

    let mappedDispatch = TileMessage >> dispatch
    ViewSingle.view (generateTile model.SignInRole) employeeTitle model.TileModel mappedDispatch