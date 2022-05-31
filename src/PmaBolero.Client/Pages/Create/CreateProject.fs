module PmaBolero.Client.Pages.Create.Project

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.Models.EmployeeData
open PmaBolero.Client.Helpers
open PmaBolero.Client.Helpers.ErrorNotification
open PmaBolero.Client.Helpers.ProgressBar
open PmaBolero.Client.Helpers.Forms

type Model =
    { Name: string
      Description: string
      Departments: (int * string) []
      SelectedDept: int option
      ProjectManagers: (int * string) []
      SelectedPm: int option
      Developers: (int * string) []
      SelectedDevs: int Set
      SkillField: string
      SkillsRequired: string Set
      IsLoading: {| Departments: bool
                    Pms: bool
                    Devs: bool |}
      Error: string option }

let initModel =
    { Name = ""
      Description = ""
      Departments = [||]
      SelectedDept = None
      ProjectManagers = [||]
      SelectedPm = None
      Developers = [||]
      SelectedDevs = Set.empty
      SkillField = ""
      SkillsRequired = Set.empty
      IsLoading =
        {| Departments = true
           Pms = true
           Devs = true |}
      Error = None }

type Message =
    | InitMessage
    | GetProjectManagers
    | RecvProjectManagers of (int * string) [] option
    | GetDevelopers
    | RecvDevelopers of (int * string) [] option
    | GetDepartments
    | RecvDepartments of (int * string) [] option
    | SetName of string
    | SetDescription of string
    | SetDepartment of int
    | SetProjectManager of int option
    | AddDev of int
    | RemoveDev of int
    | SetSkillField of string
    | AddSkill
    | RemoveSkill of string
    | SubmitProject
    | SubmitProjectReponse of int option option
    | Redirect of string
    | Error of exn
    | ClearError

let update remoteProject remoteEmployee remoteDepartment message model =
    match message with
    | InitMessage -> model, Cmd.ofMsg GetProjectManagers
    | GetProjectManagers ->
        model, Cmd.OfAuthorized.either remoteEmployee.getProjectManagers () RecvProjectManagers Error
    | RecvProjectManagers (Some pms) ->
        { model with
            ProjectManagers = pms
            IsLoading = {| model.IsLoading with Pms = false |} },
        Cmd.ofMsg GetDevelopers
    | GetDevelopers -> model, Cmd.OfAuthorized.either remoteEmployee.getDevelopers () RecvDevelopers Error
    | RecvDevelopers (Some devs) ->
        { model with
            Developers = devs
            IsLoading = {| model.IsLoading with Devs = false |} },
        Cmd.ofMsg GetDepartments
    | GetDepartments -> model, Cmd.OfAuthorized.either remoteDepartment.getDepartmentIds () RecvDepartments Error
    | RecvDepartments (Some depts) ->
        { model with
            Departments = depts
            SelectedDept = depts |> Array.tryHead |> Option.map fst
            IsLoading =
                {| model.IsLoading with
                    Departments = false |} },
        Cmd.none
    | RecvProjectManagers None
    | RecvDevelopers None
    | RecvDepartments None -> { model with Error = Some "Unable to get data due to authentication error" }, Cmd.none

    | SetName name -> { model with Name = name }, Cmd.none
    | SetDescription desc -> { model with Description = desc }, Cmd.none
    | SetDepartment dept -> { model with SelectedDept = Some dept }, Cmd.none
    | SetProjectManager projId -> { model with SelectedPm = projId }, Cmd.none
    | AddDev devId -> { model with SelectedDevs = Set.add devId model.SelectedDevs }, Cmd.none
    | RemoveDev devId -> { model with SelectedDevs = Set.remove devId model.SelectedDevs }, Cmd.none
    | SetSkillField skillStr -> { model with SkillField = skillStr }, Cmd.none
    | AddSkill ->
        let newSkills = Set.add model.SkillField model.SkillsRequired

        { model with
            SkillsRequired = newSkills
            SkillField = "" },
        Cmd.none
    | RemoveSkill skill -> { model with SkillsRequired = Set.remove skill model.SkillsRequired }, Cmd.none
    | SubmitProject ->
        let project =
            {| ProjectName = model.Name
               DepartmentId = model.SelectedDept.Value
               Description = model.Description
               ProjectManagerId = model.SelectedPm
               DeveloperIds = Set.toArray model.SelectedDevs
               SkillRequirements = Set.toArray model.SkillsRequired |}

        model, Cmd.OfAuthorized.either remoteProject.createProject project SubmitProjectReponse Error
    | SubmitProjectReponse (Some (Some projId)) ->
        initModel, Cmd.ofMsg (string projId |> sprintf "/project/%s" |> Redirect)
    | SubmitProjectReponse (Some None) ->
        { model with Error = Some "Unfortunately a new project could not be created" }, Cmd.none
    | SubmitProjectReponse None ->
        { model with Error = Some "Unable to send data due to authentication error" }, Cmd.none

    | Error e -> { model with Error = Some e.Message }, Cmd.none
    | ClearError -> { model with Error = None }, Cmd.none
    | Redirect _ -> model, Cmd.none

let tryInt iStr =
    match iStr with
    | "" -> None
    | i -> Some(int i)

let optionIntToString optInt =
    match optInt with
    | Some i -> string i
    | None -> ""

let titleField model dispatch =
    inputWithLabel "Project Text" "text" model.Name (dispatch << SetName)

let descriptionField model dispatch =
    div [ attr.``class`` "field" ] [
        label [ attr.``class`` "label" ] [
            text "Description"
        ]

        div [ attr.``class`` "control" ] [
            textarea [ attr.``class`` "textarea"
                       bind.input.string model.Description (dispatch << SetDescription) ] []
        ]
    ]

let departmentField model dispatch =
    div [ attr.``class`` "field" ] [
        label [ attr.``class`` "label" ] [
            text "Department"
        ]

        div [ attr.``class`` "control" ] [
            div [ attr.classes [ "select"
                                 if model.IsLoading.Departments then
                                     "is-loading"
                                 else
                                     "" ] ] [
                select [ on.change (fun e -> dispatch (SetDepartment(unbox e.Value))) ] [
                    forEach model.Departments (fun (id, dept) -> option [ attr.value id ] [ text dept ])
                ]
            ]
        ]
    ]

let projectManagerField model dispatch =
    div [ attr.``class`` "field" ] [
        label [ attr.``class`` "label" ] [
            text "Project Manager"
        ]

        div [ attr.``class`` "control" ] [
            div [ attr.classes [ "select"
                                 if model.IsLoading.Departments then
                                     "is-loading"
                                 else
                                     "" ] ] [
                select [ on.change (fun e -> dispatch (SetProjectManager(unbox e.Value))) ] [
                    option [ attr.``default`` true
                             attr.label " " ] []

                    forEach model.ProjectManagers (fun (id, pm) -> option [ attr.value id ] [ text pm ])
                ]
            ]
        ]
    ]

let developersField model dispatch =
    div [] [
        label [ attr.``class`` "label" ] [
            text "Developers"
        ]

        cond model.IsLoading.Devs
        <| function
            | true -> createIndeterminateBar ()
            | false ->
                forEach model.Developers
                <| fun (id, dev) ->
                    div [ attr.``class`` "field" ] [
                        div [ attr.``class`` "control" ] [
                            label [ attr.``class`` "checkbox" ] [
                                input [ attr.``type`` "checkbox"
                                        bind.checked (Set.contains id model.SelectedDevs) (fun selected ->
                                            match selected with
                                            | true -> dispatch (AddDev id)
                                            | false -> dispatch (RemoveDev id)) ]
                                text dev
                            ]
                        ]
                    ]
    ]

let skillsFields model dispatch =
    div [ attr.classes [ "field"; "has-addons" ] ] [
        div [ attr.classes [ "control"
                             "is-expanded" ] ] [
            input [ attr.``class`` "input"
                    attr.``type`` "text"
                    bind.input.string model.SkillField (dispatch << SetSkillField) ]
        ]

        div [ attr.``class`` "control" ] [
            button [ attr.``class`` "button"
                     attr.``type`` "button"
                     on.click (fun _ -> dispatch AddSkill) ] [
                text "Add Skill"
            ]
        ]
    ]

let skillsTags model dispatch =
    div [ attr.``class`` "tags" ] [
        forEach model.SkillsRequired (fun skill ->
            span [ attr.classes [ "tag"
                                  "is-medium"
                                  "is-rounded" ] ] [
                text skill

                button [ attr.classes [ "delete"; "is-small" ]
                         attr.``type`` "button"
                         on.click (fun _ -> dispatch (RemoveSkill skill)) ] []
            ])
    ]

let submitButton model =
    div [ attr.``class`` "field" ] [
        div [ attr.``class`` "control" ] [
            input [ attr.classes [ "button"; "is-primary" ]
                    attr.``type`` "submit"
                    attr.value "Submit New Project"
                    attr.disabled (String.IsNullOrEmpty model.Name) ]
        ]
    ]

let view model dispatch =
    concat' [] [
        p [ attr.``class`` "title" ] [
            text "Create New Project"
        ]

        div [ attr.``class`` "box" ] [
            form [ on.submit (fun _ -> dispatch SubmitProject) ] [
                titleField model dispatch

                descriptionField model dispatch

                departmentField model dispatch

                projectManagerField model dispatch

                developersField model dispatch

                skillsFields model dispatch

                skillsTags model dispatch

                submitButton model

                cond model.Error
                <| function
                    | None -> empty
                    | Some msg -> errorNotifDanger msg (fun _ -> dispatch ClearError)
            ]
        ]
    ]
