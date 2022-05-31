module PmaBolero.Client.Pages.Create.Employee

open System
open Elmish
open Bolero.Html
open Bolero.Remoting.Client
open System.Text.RegularExpressions

open PmaBolero.Shared.Models

open PmaBolero.Client.Models.EmployeeData
open PmaBolero.Client.Helpers
open PmaBolero.Client.Helpers.ErrorNotification
open PmaBolero.Client.Helpers.Forms

type Model =
    { Email: string
      FirstName: string
      LastName: string
      Departments: (int * string) []
      SelectedDept: int option
      Roles: string []
      SelectedRole: string
      SkillField: string
      Skills: string Set
      DeptIsLoading: bool
      Error: string option }

let initModel =
    { Email = ""
      FirstName = ""
      LastName = ""
      Departments = [||]
      SelectedDept = None
      Roles =
        [| "Developer"
           "ProjectManager"
           "Admin" |]
      SelectedRole = "Developer"
      SkillField = ""
      Skills = Set.empty
      DeptIsLoading = true
      Error = None }

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
    [ "Admin", Admin
      "Developer", Developer
      "ProjectManager", ProjectManager ]
    |> Map.ofList

let update remoteEmployee remoteDepartment message model =
    match message with
    | InitMessage -> model, Cmd.ofMsg GetDepartments
    | GetDepartments -> model, Cmd.OfAuthorized.either remoteDepartment.getDepartmentIds () RecvDepartments Error
    | RecvDepartments (Some depts) ->
        { model with
            Departments = depts
            DeptIsLoading = false
            SelectedDept = Array.tryHead depts |> Option.map fst },
        Cmd.none
    | RecvDepartments None -> { model with Error = Some "Unable to get data due to authentication error" }, Cmd.none

    | SetEmail s -> { model with Email = s }, Cmd.none
    | SetFirstName s -> { model with FirstName = s }, Cmd.none
    | SetLastName s -> { model with LastName = s }, Cmd.none
    | SetDepartment deptId -> { model with SelectedDept = Some deptId }, Cmd.none
    | SetRole s -> { model with SelectedRole = s }, Cmd.none
    | SetSkillField s -> { model with SkillField = s }, Cmd.none

    | AddSkill ->
        { model with
            Skills = Set.add model.SkillField model.Skills
            SkillField = "" },
        Cmd.none
    | RemoveSkill s -> { model with Skills = Set.remove s model.Skills }, Cmd.none

    | SubmitEmployee ->
        let role = Map.find model.SelectedRole roleMap

        if Option.isNone model.SelectedDept then
            { model with Error = Some "A new employee must have a department" }, Cmd.none
        else
            let empl =
                {| Email = model.Email
                   FullName = sprintf "%s %s" model.FirstName model.LastName
                   DepartmentId = Option.get model.SelectedDept
                   Role = role
                   Skills = Set.toArray model.Skills |}

            model, Cmd.OfAuthorized.either remoteEmployee.createEmployee empl SubmitEmployeeResponse Error
    | SubmitEmployeeResponse (Some (Some emplId)) -> model, Cmd.ofMsg (emplId |> sprintf "/employee/%d" |> Redirect)
    | SubmitEmployeeResponse (Some None) ->
        { model with Error = Some "Unfortunately a new employee could not be created" }, Cmd.none
    | SubmitEmployeeResponse None ->
        { model with Error = Some "Unable to send data due to authentication error" }, Cmd.none

    // Handled by Main.update
    | Redirect _ -> model, Cmd.none

    | Error e -> { model with Error = Some e.Message }, Cmd.none
    | ClearError -> { model with Error = None }, Cmd.none

let optionIntToString optInt =
    match optInt with
    | Some i -> string i
    | None -> ""

let nameFields model dispatch =
    div [ attr.``class`` "columns" ] [
        div [ attr.``class`` "column" ] [
            inputWithLabel "First Name" "text" model.FirstName (dispatch << SetFirstName)
        ]
        div [ attr.``class`` "column" ] [
            inputWithLabel "Last Name" "text" model.LastName (dispatch << SetLastName)
        ]
    ]

let departmentFields model dispatch =
    div [ attr.``class`` "field" ] [
        label [ attr.``class`` "label" ] [
            text "Department"
        ]
        div [ attr.``class`` (
                  if model.DeptIsLoading then
                      "select is-loading"
                  else
                      "select"
              ) ] [
            select [ on.change (fun e -> dispatch (SetDepartment(unbox e.Value))) ] [
                forEach model.Departments (fun (id, name) -> option [ attr.value id ] [ text name ])
            ]
        ]
    ]

let roleField dispatch =
    div [ attr.``class`` "field" ] [
        label [ attr.``class`` "label" ] [
            text "Role"
        ]
        div [ attr.``class`` "select" ] [
            select [ on.change (fun e -> dispatch (SetRole(unbox e.Value))) ] [
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

let skillsField dispatch =
    div [ attr.classes [ "field"; "has-addons" ] ] [
        div [ attr.classes [ "control"
                             "is-expanded" ] ] [
            input [ attr.``class`` "input"
                    attr.``type`` "text"
                    attr.placeholder "Skill"
                    on.change (fun e -> dispatch (SetSkillField(unbox e.Value))) ]
        ]
        div [ attr.``class`` "control" ] [
            button [ attr.``class`` "button"
                     attr.``type`` "button"
                     on.click (fun _ -> dispatch AddSkill) ] [
                text "Add Skill"
            ]
        ]
    ]

let skillTags model dispatch =
    div [ attr.``class`` "tags" ] [
        forEach model.Skills (fun skill ->
            span [ attr.classes [ "tag"
                                  "is-medium"
                                  "is-rounded" ] ] [
                text skill
                button [ attr.classes [ "delete"; "is-small" ]
                         attr.``type`` "button"
                         on.click (fun _ -> dispatch <| RemoveSkill skill) ] []
            ])
    ]

let submitButton model =
    div [ attr.``class`` "field" ] [
        div [ attr.``class`` "control" ] [
            input [ attr.classes [ "button"; "is-primary" ]
                    attr.``type`` "submit"
                    attr.value "Create"
                    attr.disabled (
                        if String.IsNullOrEmpty model.FirstName
                           || String.IsNullOrEmpty model.LastName
                           || Regex.IsMatch(model.Email, @"(@)(.+)$") |> not
                           || Option.isNone model.SelectedDept then
                            true
                        else
                            false
                    ) ]
        ]
    ]

let view model dispatch =
    concat' [] [
        p [ attr.``class`` "title" ] []

        div [ attr.``class`` "box" ] [
            form [ on.submit (fun _ -> dispatch SubmitEmployee) ] [
                nameFields model dispatch

                inputWithLabel "Email" "email" model.Email (dispatch << SetEmail)

                departmentFields model dispatch

                roleField dispatch

                skillsField dispatch

                skillTags model dispatch

                submitButton model
            ]
        ]

        cond model.Error
        <| function
            | None -> empty
            | Some msg -> errorNotifDanger msg (fun _ -> dispatch ClearError)
    ]
