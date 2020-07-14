module PmaBolero.Client.ViewEmployee

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.Models.EmployeeData

type Model = ViewSingle.Model<Employee>

let initModel: Model =
    {
        DataType = "Employees"
        UrlPrefix = "employee"
        IsLoading = true
        Data = None
        AuthorisationFailure = false
        Error = None
    }

type Message =
    | InitMessage of int
    | TileMessage of ViewSingle.Message<Employee>

let update remote message model =
    let getDataFunc = remote.getEmployee

    let tileMsg =
        match message with
        | InitMessage emplId -> ViewSingle.InitMessage emplId
        | TileMessage msg -> msg

    ViewSingle.update getDataFunc tileMsg model
    |> fun (model, cmd) -> model, Cmd.map TileMessage cmd

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

let generateTile (employee: Employee) =
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
        .Elt()

let view (model: Model) dispatch =
    let employeeTitle (empl: Employee) = empl.FullName

    let mappedDispatch = TileMessage >> dispatch
    ViewSingle.view generateTile employeeTitle model mappedDispatch