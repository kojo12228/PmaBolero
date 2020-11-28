module PmaBolero.Client.Pages.Create.Employee

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open System.Text.RegularExpressions

open PmaBolero.Client.Models
open PmaBolero.Client.Models.EmployeeData
open PmaBolero.Client.Helpers.ErrorNotification
open PmaBolero.Client.Helpers.ProgressBar

type Model =
    {
        Email: string
        FirstName: string
        LastName: string
        Departments: (int * string) []
        SelectedDept: int option
        Roles: string []
        SelectedRole: string
        SkillField: string
        Skills: string Set
        DeptIsLoading: bool
        Error: string option
    }

let initModel =
    {
        Email = ""
        FirstName = ""
        LastName = ""
        Departments = [||]
        SelectedDept = None
        Roles = [| "Developer"; "ProjectManager"; "Admin" |]
        SelectedRole = "Developer"
        SkillField = ""
        Skills = Set.empty
        DeptIsLoading = true
        Error = None
    }

type Message =
    | InitMessage
    | GetDepartments
    | RecvDepartments of (int * string) [] option
    | SetEmail of string
    | SetFirstName of string
    | SetLastName of string
    | SetDepartment of int
    | SetRole of string
    | SetSkillField of string
    | AddSkill
    | RemoveSkill of string
    | SubmitEmployee
    | SubmitEmployeeResponse of int option option
    | Redirect of string
    | Error of exn
    | ClearError

let private roleMap =
    [ 
        "Admin", Auth.Admin
        "Developer", Auth.Developer
        "ProjectManager", Auth.ProjectManager
    ] |> Map.ofList

let update remoteEmployee remoteDepartment message model =
    match message with
    | InitMessage ->
        model, Cmd.ofMsg GetDepartments
    | GetDepartments ->
        model, Cmd.OfAuthorized.either remoteDepartment.getDepartmentIds () RecvDepartments Error
    | RecvDepartments (Some depts) ->
        {
            model with
                Departments = depts
                DeptIsLoading = false
                SelectedDept = Array.tryHead depts |> Option.map fst
        }, Cmd.none
    | RecvDepartments None ->
        { model with Error = Some "Unable to get data due to authentication error" }, Cmd.none

    | SetEmail s ->
        { model with Email = s }, Cmd.none
    | SetFirstName s ->
        { model with FirstName = s }, Cmd.none
    | SetLastName s ->
        { model with LastName = s }, Cmd.none
    | SetDepartment deptId ->
        { model with SelectedDept = Some deptId }, Cmd.none
    | SetRole s ->
        { model with SelectedRole = s }, Cmd.none
    | SetSkillField s ->
        { model with SkillField = s }, Cmd.none

    | AddSkill ->
        {
            model with
                Skills = Set.add model.SkillField model.Skills
                SkillField = ""
        }, Cmd.none
    | RemoveSkill s ->
        { model with Skills = Set.remove s model.Skills }, Cmd.none

    | SubmitEmployee ->
        let role = Map.find model.SelectedRole roleMap

        if Option.isNone model.SelectedDept
        then { model with Error = Some "A new employee must have a department" }, Cmd.none
        else
            let empl =
                {|
                    Email = model.Email
                    FullName = sprintf "%s %s" model.FirstName model.LastName
                    DepartmentId = Option.get model.SelectedDept
                    Role = role
                    Skills = Set.toArray model.Skills
                |}

            model, Cmd.OfAuthorized.either remoteEmployee.createEmployee empl SubmitEmployeeResponse Error
    | SubmitEmployeeResponse (Some (Some emplId)) ->
        model, Cmd.ofMsg (emplId |> sprintf "/employee/%d" |> Redirect)
    | SubmitEmployeeResponse (Some None) ->
        { model with Error = Some "Unfortunately a new employee could not be created" }, Cmd.none
    | SubmitEmployeeResponse None ->
        { model with Error = Some "Unable to send data due to authentication error" }, Cmd.none
    
    // Handled by Main.update
    | Redirect _ -> model, Cmd.none

    | Error e ->
        { model with Error = Some e.Message }, Cmd.none
    | ClearError ->
        { model with Error = None }, Cmd.none

type CreateEmployeeTemplate = Template<"wwwroot/createemployee.html">

let optionIntToString optInt =
    match optInt with
    | Some i -> string i
    | None -> ""

let view model dispatch =
    // fsharplint:disable CanBeReplacedWithComposition

    CreateEmployeeTemplate
        .CreateEmployee()
        .FirstName(model.FirstName, fun fn -> dispatch (SetFirstName fn))
        .LastName(model.LastName, fun ln -> dispatch (SetLastName ln))
        .Email(model.Email, fun em -> dispatch (SetEmail em))
        .Department(
            optionIntToString model.SelectedDept,
            fun deptIdStr -> dispatch (SetDepartment (int deptIdStr))
        )
        .DeptOptions(
            forEach model.Departments <| (fun (deptId, deptName) ->
                CreateEmployeeTemplate
                    .DeptItem()
                    .DeptId(deptId)
                    .Name(deptName)
                    .Elt()
            )
        )
        .DeptIsLoading(if model.DeptIsLoading then "is-loading" else "")
        .Role(model.SelectedRole, fun r -> dispatch (SetRole r))
        .SkillField(model.SkillField,fun sk -> dispatch (SetSkillField sk))
        .AddSkillClick(fun _ -> dispatch AddSkill)
        .SkillTags(
            forEach model.Skills <| (fun skill ->
                CreateEmployeeTemplate
                    .SkillTag()
                    .Skill(skill)
                    .DeleteSkill(fun _ -> dispatch (RemoveSkill skill))
                    .Elt()
            )
        )
        .DisabledSubmit(
            String.IsNullOrEmpty model.FirstName ||
            String.IsNullOrEmpty model.LastName ||
            Regex.IsMatch(model.Email, @"(@)(.+)$") |> not ||
            Option.isNone model.SelectedDept
        )
        .SubmitEmployee(fun _ -> dispatch SubmitEmployee)
        .ErrorNotification(
            cond model.Error <| function
            | None -> empty
            | Some msg -> errorNotifDanger msg (fun _ -> dispatch ClearError)
        )
        .Elt()