module PmaBolero.Client.Pages.Edit.Employee

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open System.Text.RegularExpressions

open PmaBolero.Shared.Models

open PmaBolero.Client.Models
open PmaBolero.Client.Models.EmployeeData
open PmaBolero.Client.Helpers
open PmaBolero.Client.Helpers.Forms
open PmaBolero.Client.Helpers.ErrorNotification
open PmaBolero.Client.Helpers.ProgressBar

type Model =
    { OriginalEmployee: Employee option
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
      Success: string option }

let initModel =
    { OriginalEmployee = None
      Email = ""
      Name = ""
      SkillField = ""
      Skills = Set.empty
      Roles =
        [| "Developer"
           "ProjectManager"
           "Admin" |]
      SelectedRole = "Developer"
      Departments = [||]
      SelectedDept = None
      LoadingStatus = LoadingEmpty
      Error = None
      Success = None }

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
    | "Admin" -> Admin
    | "Developer" -> Developer
    | "ProjectManager" -> ProjectManager
    | _ -> Developer

let private roleToString role =
    match role with
    | Admin -> "Admin"
    | Developer -> "Developer"
    | ProjectManager -> "ProjectManager"

let update remoteEmployee remoteDepartment message model =
    match message with
    | InitMessage emplId ->
        model,
        Cmd.batch [ Cmd.ofMsg (GetEmployee emplId)
                    Cmd.ofMsg (GetDepartments) ]

    | GetEmployee emplId -> model, Cmd.OfAuthorized.either remoteEmployee.getEmployee emplId RecvEmployee Error
    | RecvEmployee (Some (Some empl)) ->
        { model with
            OriginalEmployee = Some empl
            Email = empl.Email
            Name = empl.FullName
            Skills = empl.Skills |> Set.ofArray
            SelectedRole = roleToString empl.Role
            SelectedDept = empl.DepartmentID |> fst |> Some
            LoadingStatus = loadingNextHalf model.LoadingStatus },
        Cmd.none
    | RecvEmployee (Some None) ->
        { model with
            Error = Some "No project exists with this ID"
            LoadingStatus = LoadingComplete },
        Cmd.none
    | GetDepartments -> model, Cmd.OfAuthorized.either remoteDepartment.getDepartmentIds () RecvDepartments Error
    | RecvDepartments (Some depts) ->
        { model with
            Departments = depts
            LoadingStatus = loadingNextHalf model.LoadingStatus },
        Cmd.none
    | RecvEmployee None
    | RecvDepartments None -> { model with Error = Some "Unable to get data due to authentication error" }, Cmd.none

    | SetEmail s -> { model with Email = s }, Cmd.none
    | SetName s -> { model with Name = s }, Cmd.none
    | SetSkillField s -> { model with SkillField = s }, Cmd.none
    | AddSkill ->
        { model with
            Skills = Set.add model.SkillField model.Skills
            SkillField = "" },
        Cmd.none
    | RemoveSkill s -> { model with Skills = Set.remove s model.Skills }, Cmd.none
    | SetDepartment deptId -> { model with SelectedDept = Some deptId }, Cmd.none
    | SetRole s -> { model with SelectedRole = s }, Cmd.none

    | UpdateEmployee ->
        let empl =
            {| Id = model.OriginalEmployee.Value.Id
               FullName = model.Name
               Skills = model.Skills |> Set.toArray |}

        { model with LoadingStatus = LoadingHalf },
        Cmd.OfAuthorized.either remoteEmployee.updateEmployee empl UpdateEmployeeResponse Error
    | UpdateEmployeeResponse (Some (Some empl)) ->
        { model with
            OriginalEmployee = Some empl
            Email = empl.Email
            Name = empl.FullName
            Skills = empl.Skills |> Set.ofArray
            LoadingStatus = LoadingComplete
            Success = Some "Employee successfully updated" },
        Cmd.none
    | UpdateRole ->
        { model with LoadingStatus = LoadingHalf },
        Cmd.OfAuthorized.either
            remoteEmployee.changeRole
            (model.OriginalEmployee.Value.Id, stringToRole model.SelectedRole)
            UpdateRoleResponse
            Error
    | UpdateRoleResponse (Some (Some empl)) ->
        { model with
            OriginalEmployee = Some empl
            SelectedRole = empl.Role |> roleToString
            LoadingStatus = LoadingComplete
            Success = Some "Employee successfully updated" },
        Cmd.none
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
        { model with
            OriginalEmployee = Some empl
            SelectedDept = empl.DepartmentID |> fst |> Some
            LoadingStatus = LoadingComplete
            Success = Some "Employee successfully updated" },
        Cmd.none
    | UpdateEmployeeResponse (Some None)
    | UpdateRoleResponse (Some None)
    | UpdateDepartmentResponse (Some None) ->
        { model with Error = Some "Unable to updated project at this time." }, Cmd.none
    | UpdateEmployeeResponse None
    | UpdateRoleResponse None
    | UpdateDepartmentResponse None ->
        { model with
            Error = Some "Unable to send data due to authentication error"
            LoadingStatus = LoadingComplete },
        Cmd.none

    | Error e -> { model with Error = Some e.Message }, Cmd.none
    | ClearError -> { model with Error = None }, Cmd.none
    | ClearSuccess -> { model with Success = None }, Cmd.none

type EditEmployeeTemplate = Template<"wwwroot/editemployee.html">

let optionIntToString optInt =
    match optInt with
    | Some i -> string i
    | None -> ""

let mainBoxSkillsTag (skills: Set<string>) dispatch =
    div [ attr.``class`` "tags" ] [
        forEach skills
        <| (fun skill ->
            span [ attr.classes [ "tag"
                                  "is-medium"
                                  "is-rounded" ] ] [
                text skill
                button [ attr.classes [ "delete"; "is-small" ]
                         attr.``type`` "button"
                         on.click (fun _ -> dispatch (RemoveSkill skill)) ] []
            ])
    ]

let mainEditBox model dispatch =
    div [ attr.``class`` "box " ] [
        form [ on.submit (fun _ -> dispatch UpdateEmployee) ] [
            inputWithLabel "Name" "text" model.Name (SetName >> dispatch)

            inputWithLabel "Email" "text" model.Email (SetEmail >> dispatch)

            div [ attr.classes [ "field"; "has-addons" ] ] [
                div [ attr.classes [ "control"
                                     "is-expanded" ] ] [
                    input [ attr.``class`` "input"
                            attr.``type`` "text"
                            on.change (fun e -> e.Value |> unbox |> SetSkillField |> dispatch) ]
                ]

                div [ attr.``class`` "control" ] [
                    button [ attr.``class`` "button"
                             attr.``type`` "button"
                             on.click (fun _ -> dispatch AddSkill) ] [
                        text "Add Skill"
                    ]
                ]
            ]

            mainBoxSkillsTag model.Skills dispatch

            div [ attr.``class`` "field" ] [
                div [ attr.``class`` "control" ] [
                    input [ attr.classes [ "button"
                                           "is-primary"
                                           "is-fullwidth" ]
                            attr.``type`` "submit"
                            attr.value "Update"
                            attr.disabled (
                                String.IsNullOrEmpty model.Name
                                || Regex.IsMatch(model.Email, @"(@)(.+)$") |> not
                            ) ]
                ]
            ]
        ]
    ]

let roleBox model dispatch =
    let currentRole =
        match model.OriginalEmployee with
        | Some empl -> string empl.Role
        | None -> ""

    div [ attr.``class`` "box" ] [
        p [] [
            strong [] [ text "Current Role" ]
            text $": {currentRole}"
        ]

        form [ on.submit (fun _ -> dispatch UpdateRole) ] [
            div [ attr.``class`` "field" ] [
                label [ attr.``class`` "label" ] [
                    text "Role"
                ]
                div [ attr.``class`` "select" ] [
                    select [ attr.``class`` "select"
                             on.change (fun e -> e.Value |> unbox |> SetRole |> dispatch) ] [
                        option [ attr.``default`` true
                                 attr.value "Developer" ] [
                            text "Developer"
                        ]
                        option [ attr.value "ProjectManager" ] [
                            text "Project Manager"
                        ]
                        option [ attr.value "Admin" ] [
                            text "Admin"
                        ]
                    ]
                ]
            ]

            div [ attr.``class`` "field" ] [
                div [ attr.``class`` "control" ] [
                    input [ attr.classes [ "button"
                                           "is-primary"
                                           "is-fullwidth" ]
                            attr.``type`` "submit"
                            attr.value "Update" ]
                ]
            ]
        ]
    ]

let departmentBox model dispatch =
    let currentDepartmentName =
        match model.OriginalEmployee with
        | Some empl -> empl.DepartmentID |> snd
        | None -> ""

    div [ attr.``class`` "box" ] [
        p [] [
            strong [] [ text "Current Department" ]
            text $": {currentDepartmentName}"
        ]

        form [ on.submit (fun _ -> dispatch UpdateDepartment) ] [
            div [ attr.``class`` "field" ] [
                label [ attr.``class`` "label" ] [
                    text "Department"
                ]
                div [ attr.``class`` "control" ] [
                    div [ attr.``class`` "select" ] [
                        select [ on.change (fun e -> e.Value |> unbox |> SetDepartment |> dispatch) ] [
                            forEach model.Departments (fun (id, name) -> option [ attr.value id ] [ text name ])
                        ]
                    ]
                ]
            ]

            div [ attr.``class`` "field" ] [
                div [ attr.``class`` "control" ] [
                    input [ attr.classes [ "button"
                                           "is-primary"
                                           "is-fullwidth" ]
                            attr.``type`` "submit"
                            attr.value "Update"
                            attr.disabled (Option.isNone model.SelectedDept) ]
                ]
            ]
        ]
    ]

let view model dispatch =
    concat' [] [
        p [ attr.``class`` "title" ] [
            text "Edit Employee"
        ]
        p [ attr.``class`` "subtitle" ] [
            match model.OriginalEmployee with
            | Some empl -> empl.FullName
            | None -> ""
            |> text
        ]

        cond model.LoadingStatus
        <| function
            | LoadingComplete ->
                concat' [] [
                    mainEditBox model dispatch

                    div [ attr.``class`` "columns" ] [
                        div [ attr.``class`` "column" ] [
                            roleBox model dispatch
                        ]

                        div [ attr.``class`` "column" ] [
                            departmentBox model dispatch
                        ]
                    ]
                ]
            | load -> createDeterminateBar load

        cond model.Error
        <| function
            | Some errMsg -> errorNotifDanger errMsg (fun _ -> dispatch ClearError)
            | None ->
                cond model.Success
                <| function
                    | Some successMsg -> errorNotifSuccess successMsg (fun _ -> dispatch ClearSuccess)
                    | None -> empty
    ]
