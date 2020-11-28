module PmaBolero.Client.Pages.Edit.Employee

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
        OriginalEmployee: Employee option
        // First box field
        Email: string
        Name: string
        SkillField: string
        Skills: string Set
        // Second box field
        Roles: string []
        SelectedRole: string
        // Third box field
        Departments: (int * string) []
        SelectedDept: int option
        LoadingStatus: LoadingStatus
        Error: string option
        Success: string option
    }

let initModel =
    {
        OriginalEmployee = None
        Email = ""
        Name = ""
        SkillField = ""
        Skills = Set.empty
        Roles = [| "Developer"; "ProjectManager"; "Admin" |]
        SelectedRole = "Developer"
        Departments = [||]
        SelectedDept = None
        LoadingStatus = LoadingEmpty
        Error = None
        Success = None
    }

type Message =
    | InitMessage of int

    | GetEmployee of int
    | RecvEmployee of Employee option option
    | GetDepartments
    | RecvDepartments of (int * string) [] option

    | SetEmail of string
    | SetName of string
    | SetSkillField of string
    | AddSkill
    | RemoveSkill of string
    | SetRole of string
    | SetDepartment of int

    | UpdateEmployee
    | UpdateEmployeeResponse of Employee option option
    | UpdateRole
    | UpdateRoleResponse of Employee option option
    | UpdateDepartment
    | UpdateDepartmentResponse of Employee option option

    | Error of exn
    | ClearError
    | ClearSuccess

let private stringToRole str =
    match str with
    | "Admin" -> Auth.Admin
    | "Developer" -> Auth.Developer
    | "ProjectManager" -> Auth.ProjectManager
    | _ -> Auth.Developer

let private roleToString role =
    match role with
    | Auth.Admin -> "Admin"
    | Auth.Developer -> "Developer"
    | Auth.ProjectManager -> "ProjectManager"

let update remoteEmployee remoteDepartment message model =
    match message with
    | InitMessage emplId ->
        model, Cmd.batch [
            Cmd.ofMsg (GetEmployee emplId)
            Cmd.ofMsg (GetDepartments)
        ]

    | GetEmployee emplId ->
        model, Cmd.OfAuthorized.either remoteEmployee.getEmployee emplId RecvEmployee Error
    | RecvEmployee (Some (Some empl)) ->
        {
            model with
                OriginalEmployee = Some empl
                Email = empl.Email
                Name = empl.FullName
                Skills = empl.Skills |> Set.ofArray
                SelectedRole = roleToString empl.Role
                SelectedDept = empl.DepartmentID |> fst |> Some
                LoadingStatus = loadingNextHalf model.LoadingStatus
        }, Cmd.none
    | RecvEmployee (Some None) ->
        {
            model with
                Error = Some "No project exists with this ID"
                LoadingStatus = LoadingComplete
        }, Cmd.none
    | GetDepartments ->
        model, Cmd.OfAuthorized.either remoteDepartment.getDepartmentIds () RecvDepartments Error
    | RecvDepartments (Some depts) ->
        {
            model with
                Departments = depts
                LoadingStatus = loadingNextHalf model.LoadingStatus
        }, Cmd.none
    | RecvEmployee None | RecvDepartments None ->
        { model with Error = Some "Unable to get data due to authentication error" }, Cmd.none

    | SetEmail s ->
        { model with Email = s }, Cmd.none
    | SetName s ->
        { model with Name = s }, Cmd.none
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
    | SetDepartment deptId ->
        { model with SelectedDept = Some deptId }, Cmd.none
    | SetRole s ->
        { model with SelectedRole = s }, Cmd.none

    | UpdateEmployee ->
        let empl =
            {|
                Id = model.OriginalEmployee.Value.Id
                FullName = model.Name
                Skills = model.Skills |> Set.toArray
            |}

        { model with LoadingStatus = LoadingHalf },
        Cmd.OfAuthorized.either remoteEmployee.updateEmployee empl UpdateEmployeeResponse Error
    | UpdateEmployeeResponse (Some (Some empl)) ->
        {
            model with
                OriginalEmployee = Some empl
                Email = empl.Email
                Name = empl.FullName
                Skills = empl.Skills |> Set.ofArray
                LoadingStatus = LoadingComplete
                Success = Some "Employee successfully updated"
        }, Cmd.none
    | UpdateRole ->
        { model with LoadingStatus = LoadingHalf },
        Cmd.OfAuthorized.either
            remoteEmployee.changeRole
            (model.OriginalEmployee.Value.Id, stringToRole model.SelectedRole)
            UpdateRoleResponse
            Error
    | UpdateRoleResponse (Some (Some empl)) ->
        {
            model with
                OriginalEmployee = Some empl
                SelectedRole = empl.Role |> roleToString
                LoadingStatus = LoadingComplete
                Success = Some "Employee successfully updated"
        }, Cmd.none
    | UpdateDepartment ->
        match model.SelectedDept with
        | None -> model, Cmd.none
        | Some deptId ->
            { model with LoadingStatus = LoadingHalf },
            Cmd.OfAuthorized.either
                remoteEmployee.transferToDepartment
                (model.OriginalEmployee.Value.Id, deptId)
                UpdateDepartmentResponse
                Error
    | UpdateDepartmentResponse (Some (Some empl)) ->
        {
            model with
                OriginalEmployee = Some empl
                SelectedDept = empl.DepartmentID |> fst |> Some
                LoadingStatus = LoadingComplete
                Success = Some "Employee successfully updated"
        }, Cmd.none
    | UpdateEmployeeResponse (Some None) | UpdateRoleResponse (Some None)
    | UpdateDepartmentResponse (Some None) ->
        {
            model with Error = Some "Unable to updated project at this time."
        }, Cmd.none
    | UpdateEmployeeResponse None | UpdateRoleResponse None
    | UpdateDepartmentResponse None ->
        {
            model with
                Error = Some "Unable to send data due to authentication error"
                LoadingStatus = LoadingComplete
        }, Cmd.none

    | Error e ->
        { model with Error = Some e.Message }, Cmd.none
    | ClearError ->
        { model with Error = None }, Cmd.none
    | ClearSuccess ->
        { model with Success = None }, Cmd.none

type EditEmployeeTemplate = Template<"wwwroot/editemployee.html">

let optionIntToString optInt =
    match optInt with
    | Some i -> string i
    | None -> ""

// fsharplint:disable CanBeReplacedWithComposition

let viewMainEditBox model dispatch =

    EditEmployeeTemplate
        .MainBox()
        .Name(model.Name, fun nm -> dispatch (SetName nm))
        .Email(model.Email, fun em -> dispatch (SetEmail em))
        .SkillField(model.SkillField, fun sk -> dispatch (SetSkillField sk))
        .AddSkillClick(fun _ -> dispatch AddSkill)
        .SkillTags(
            forEach model.Skills <| (fun skill ->
                EditEmployeeTemplate
                    .SkillTag()
                    .Skill(skill)
                    .DeleteSkill(fun _ -> dispatch (RemoveSkill skill))
                    .Elt()
            )
        )
        .DisabledUpdate(
            String.IsNullOrEmpty model.Name ||
            Regex.IsMatch(model.Email, @"(@)(.+)$") |> not
        )
        .SubmitMain(fun _ -> dispatch UpdateEmployee)
        .Elt()

let viewRoleBox model dispatch =
    EditEmployeeTemplate
        .RoleBox()
        .CurrentDeptName(
            match model.OriginalEmployee with
            | Some empl -> string empl.Role
            | None -> ""
        )
        .Role(model.SelectedRole, fun r -> dispatch (SetRole r))
        .SubmitRole(fun _ -> dispatch UpdateRole)
        .Elt()

let viewDepartmentBox model dispatch =
    EditEmployeeTemplate
        .DepartmentBox()
        .CurrentDeptName(
            match model.OriginalEmployee with
            | Some empl -> empl.DepartmentID |> snd
            | None -> ""
        )
        .Department(
            optionIntToString model.SelectedDept,
            fun deptIdStr -> dispatch (SetDepartment (int deptIdStr))
        )
        .DeptOptions(
            forEach model.Departments <| (fun (deptId, deptName) ->
                EditEmployeeTemplate
                    .DeptItem()
                    .DeptId(deptId)
                    .Name(deptName)
                    .Elt()
            )
        )
        .DisabledSubmit(Option.isNone model.SelectedDept)
        .SubmitDepartment(fun _ -> dispatch UpdateDepartment)
        .Elt()

let view model dispatch =
    EditEmployeeTemplate
        .EditEmployee()
        .EmployeeName(
            match model.OriginalEmployee with
            | Some empl -> empl.FullName
            | None -> ""
        )
        .ProgressBar(
            cond model.LoadingStatus <| function
            | LoadingComplete -> empty
            | load -> createDeterminateBar load
        )
        .MainEditBox(
            cond model.LoadingStatus <| function
            | LoadingComplete ->
                viewMainEditBox model dispatch
            | _ -> empty
        )
        .EditRoleBox(
            cond model.LoadingStatus <| function
            | LoadingComplete ->
                viewRoleBox model dispatch
            | _ -> empty
        )
        .EditDepartmentBox(
            cond model.LoadingStatus <| function
            | LoadingComplete ->
                viewDepartmentBox model dispatch
            | _ -> empty
        )
        .Notification(
            cond model.Error <| function
            | Some errMsg ->
                errorNotifDanger errMsg (fun _ -> dispatch ClearError)
            | None ->
                cond model.Success <| function
                | Some successMsg ->
                    errorNotifSuccess successMsg (fun _ -> dispatch ClearSuccess)
                | None -> empty
        )
        .Elt()