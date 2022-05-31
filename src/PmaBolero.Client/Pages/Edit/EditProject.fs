module PmaBolero.Client.Pages.Edit.Project

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

open PmaBolero.Client.Helpers
open PmaBolero.Client.Helpers.Forms
open PmaBolero.Client.Helpers.ProgressBar

type Model =
    { SignInRole: Role option
      OriginalProject: Project option
      Name: string
      Description: string
      Status: string
      Developers: (int * string) []
      SelectedDevs: int Set
      SkillField: string
      SkillsRequired: string Set
      // Only if the sign in user is admin
      Departments: (int * string) []
      SelectedDept: int option
      ProjectManagers: (int * string) []
      SelectedPm: int option
      LoadingStatus: LoadingStatus
      Error: string option
      Success: string option }

let initModel =
    { SignInRole = None
      OriginalProject = None
      Name = ""
      Description = ""
      Status = ""
      Developers = [||]
      SelectedDevs = Set.empty
      SkillField = ""
      SkillsRequired = Set.empty
      Departments = [||]
      SelectedDept = None
      ProjectManagers = [||]
      SelectedPm = None
      LoadingStatus = LoadingEmpty
      Error = None
      Success = None }

type Message =
    | InitMessage of int * (Role option)

    // Get for both Admin and PM
    | GetProject of int
    | RecvProject of Project option option
    | GetDevelopers
    | RecvDevelopers of (int * string) [] option
    // Get for only Admin
    | GetProjectManagers
    | RecvProjectManagers of (int * string) [] option
    | GetDepartments
    | RecvDepartments of (int * string) [] option

    | SetName of string
    | SetDescription of string
    | SetStatus of string
    | AddDev of int
    | RemoveDev of int
    | SetSkillField of string
    | AddSkill
    | RemoveSkill of string
    // Fields only present for Admin
    | SetDepartment of int
    | SetProjectManager of int option

    | UpdateProject
    | UpdateProjectResponse of Project option option
    | UpdateDepartment
    // If successful, the original object model needs to be updated
    | UpdateDepartmentResponse of Project option option

    | Error of exn
    | ClearError
    | ClearSuccess

let update remoteProject remoteEmployee remoteDepartment message model =
    match message with
    | InitMessage (projId, signInRole) ->
        match signInRole with
        | Some Admin ->
            { model with SignInRole = signInRole },
            Cmd.batch [ Cmd.ofMsg (GetProject projId)
                        Cmd.ofMsg GetDevelopers
                        Cmd.ofMsg GetProjectManagers
                        Cmd.ofMsg GetDepartments ]
        | Some ProjectManager ->
            { model with SignInRole = signInRole },
            Cmd.batch [ Cmd.ofMsg (GetProject projId)
                        Cmd.ofMsg GetDevelopers ]
        | Some Developer -> { model with Error = Some "You do not have permission to edit this project" }, Cmd.none
        | None -> { model with Error = Some "Unexpected error, sign in information unavailable" }, Cmd.none

    | GetProject projId -> model, Cmd.OfAuthorized.either remoteProject.getProject projId RecvProject Error
    | RecvProject (Some (Some proj)) ->
        let updatedModel =
            { model with
                OriginalProject = Some proj
                Name = proj.Name
                Description = proj.Description
                Status = string proj.Status
                SelectedDevs = proj.DeveloperIds |> Array.map fst |> Set.ofArray
                SkillsRequired = proj.SkillRequirements |> Set.ofArray
                SelectedDept = proj.DepartmentId |> fst |> Some
                SelectedPm = proj.ProjectManagerId |> Option.map fst
                LoadingStatus =
                    match model.SignInRole with
                    | Some Admin -> loadingNextQuarter model.LoadingStatus
                    | Some ProjectManager -> loadingNextHalf model.LoadingStatus
                    | _ -> LoadingEmpty }

        updatedModel, Cmd.none
    | RecvProject (Some None) -> { model with Error = Some "No project exists with this ID" }, Cmd.none
    | GetDevelopers -> model, Cmd.OfAuthorized.either remoteEmployee.getDevelopers () RecvDevelopers Error
    | RecvDevelopers (Some devs) ->
        { model with
            Developers = devs
            LoadingStatus =
                match model.SignInRole with
                | Some Admin -> loadingNextQuarter model.LoadingStatus
                | Some ProjectManager -> loadingNextHalf model.LoadingStatus
                | _ -> LoadingEmpty },
        Cmd.none
    | GetProjectManagers ->
        model, Cmd.OfAuthorized.either remoteEmployee.getProjectManagers () RecvProjectManagers Error
    | RecvProjectManagers (Some pms) ->
        { model with
            ProjectManagers = pms
            LoadingStatus = loadingNextQuarter model.LoadingStatus },
        Cmd.none
    | GetDepartments -> model, Cmd.OfAuthorized.either remoteDepartment.getDepartmentIds () RecvDepartments Error
    | RecvDepartments (Some depts) ->
        { model with
            Departments = depts
            LoadingStatus = loadingNextQuarter model.LoadingStatus },
        Cmd.none
    | RecvProject None
    | RecvDevelopers None
    | RecvDepartments None
    | RecvProjectManagers None -> { model with Error = Some "Unable to get data due to authentication error" }, Cmd.none

    | SetName name -> { model with Name = name }, Cmd.none
    | SetDescription desc -> { model with Description = desc }, Cmd.none
    | SetStatus status -> { model with Status = status }, Cmd.none
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
    | SetProjectManager projId -> { model with SelectedPm = projId }, Cmd.none
    | SetDepartment dept -> { model with SelectedDept = Some dept }, Cmd.none

    | UpdateProject ->
        let statusResolver statusStr =
            match statusStr with
            | "Pending" -> Pending
            | "Active" -> Active
            | "Complete" -> Complete
            | _ -> Pending

        match model.SignInRole with
        | Some Admin ->
            let updatedProject =
                {| Id = model.OriginalProject.Value.Id
                   ProjectName = model.Name
                   Description = model.Description
                   Status = statusResolver model.Status
                   ProjectManagerId = model.SelectedPm
                   DeveloperIds = model.SelectedDevs |> Set.toArray
                   SkillRequirements = model.SkillsRequired |> Set.toArray |}

            { model with LoadingStatus = LoadingHalf },
            Cmd.OfAuthorized.either remoteProject.updateProjectElevated updatedProject UpdateProjectResponse Error
        | Some ProjectManager ->
            let updatedProject =
                {| Id = model.OriginalProject.Value.Id
                   ProjectName = model.Name
                   Description = model.Description
                   Status = statusResolver model.Status
                   DeveloperIds = model.SelectedDevs |> Set.toArray
                   SkillRequirements = model.SkillsRequired |> Set.toArray |}

            { model with LoadingStatus = LoadingHalf },
            Cmd.OfAuthorized.either remoteProject.updateProject updatedProject UpdateProjectResponse Error
        | _ -> { model with Error = Some "Unable to update project" }, Cmd.none
    | UpdateProjectResponse (Some (Some proj)) ->
        let updatedModel =
            { model with
                OriginalProject = Some proj
                Name = proj.Name
                Description = proj.Description
                Status = string proj.Status
                SelectedDevs = proj.DeveloperIds |> Array.map fst |> Set.ofArray
                SkillsRequired = proj.SkillRequirements |> Set.ofArray
                SelectedDept = proj.DepartmentId |> fst |> Some
                SelectedPm = proj.ProjectManagerId |> Option.map fst
                LoadingStatus = LoadingComplete
                Success = Some "Project successfully updated" }

        updatedModel, Cmd.none
    | UpdateDepartment ->
        match model.SelectedDept with
        | Some deptId ->
            { model with LoadingStatus = LoadingHalf },
            Cmd.OfAuthorized.either
                remoteProject.assignToDepartment
                (model.OriginalProject.Value.Id, deptId)
                UpdateDepartmentResponse
                Error
        | None -> { model with Error = Some "Required to set a department" }, Cmd.none
    | UpdateDepartmentResponse (Some (Some proj)) ->
        { model with
            OriginalProject = Some proj
            SelectedDept = proj.DepartmentId |> fst |> Some
            Success = Some "Project successfully updated"
            LoadingStatus = LoadingComplete },
        Cmd.none
    | UpdateProjectResponse (Some None)
    | UpdateDepartmentResponse (Some None) ->
        { model with
            Error = Some "Unable to updated project at this time."
            LoadingStatus = LoadingComplete },
        Cmd.none
    | UpdateProjectResponse None
    | UpdateDepartmentResponse (None) ->
        { model with
            Error = Some "Unable to send data due to authentication error"
            LoadingStatus = LoadingComplete

         },
        Cmd.none

    | Error e -> { model with Error = Some e.Message }, Cmd.none
    | ClearError -> { model with Error = None }, Cmd.none
    | ClearSuccess -> { model with Success = None }, Cmd.none

type EditProjectTemplate = Template<"wwwroot/editproject.html">

// Move into separate module to remove duplication
let tryInt iStr =
    match iStr with
    | "" -> None
    | i -> Some(int i)

let optionIntToString optInt =
    match optInt with
    | Some i -> string i
    | None -> ""

// fsharplint:disable CanBeReplacedWithComposition

let projectManagerField model dispatch =
    div [ attr.``class`` "field" ] [
        label [ attr.``class`` "label" ] [ text "Project Manager" ]
        div [ attr.``class`` "control" ] [
            div [ attr.``class`` "select" ] [
                select [ on.change (fun e -> e.Value |> unbox |> tryInt |> SetProjectManager |> dispatch) ] [
                    option [
                        attr.``default`` true
                        attr.label " "
                    ] []

                    forEach model.ProjectManagers (fun (pmId, pmName) ->
                        option [ attr.value pmId ] [ text pmName ]
                    )
                ]
            ]
        ]
    ]

let mainEditBox model dispatch =
    div [ attr.``class`` "box" ] [
        form [ on.submit (fun _ -> dispatch UpdateProject) ] [
            inputWithLabel "Project Title" "text" model.Name (SetName >> dispatch)

            div [ attr.``class`` "field" ] [
                label [ attr.``class`` "label" ] [
                    text "Description"
                ]
                div [ attr.``class`` "control" ] [
                    textarea [
                        attr.``class`` "textarea"
                        on.change (fun e -> e.Value |> unbox |> SetDescription |> dispatch)
                    ] []
                ]
            ]

            div [ attr.``class`` "field" ] [
                label [ attr.``class`` "label" ] [
                    text "Status"
                ]
                div [ attr.``class`` "control" ] [
                    div [ attr.``class`` "select" ] [
                        select [ on.change (fun e -> e.Value |> unbox |> SetStatus |> dispatch) ] [
                            yield!
                                [ "Pending"; "Active"; "Complete"]
                                |> List.map (fun s ->
                                    option [ attr.value s ] [ text s ]
                                )
                        ]
                    ]
                ]
            ]

            projectManagerField model dispatch

            div [] [
                label [ attr.``class`` "label" ] [
                    text "Developers"
                ]

                forEach model.Developers (fun (devId, _) ->
                    div [ attr.``class`` "field" ] [
                        div [ attr.``class`` "control" ] [
                            label [ attr.``class`` "checkbox" ] [
                                input [
                                    attr.``type`` "checkbox"
                                    bind.checked (Set.contains devId model.SelectedDevs) (fun selected -> 
                                        match selected with
                                        | true -> dispatch (AddDev devId)
                                        | false -> dispatch (RemoveDev devId)
                                    )
                                ]
                            ]
                        ]
                    ]
                )
            ]

            div [
                attr.classes [
                    "field"
                    "has-addons"
                ]
            ] [
                div [
                    attr.classes [
                        "control"
                        "is-expande"
                    ]
                ] [
                    input [
                        attr.``class`` "input"
                        attr.``type`` "text"
                        bind.input.string model.SkillField (fun s -> SetSkillField s |> dispatch)
                    ]
                ]

                div [ attr.``class`` "control" ] [
                    button [
                        attr.``class`` "button"
                        attr.``type`` "button"
                        on.click (fun _ -> AddSkill |> dispatch)
                    ] [
                        text "Add Skill"
                    ]
                ]
            ]

            div [ attr.``class`` "tags" ] [
                forEach model.SkillsRequired
                <| fun skill ->
                    span [
                        attr.classes [
                            "tag"
                            "is-medium"
                            "is-rounded"
                        ]
                    ] [
                        text skill
                        button [
                            attr.classes [
                                "delete"
                                "is-small"
                            ]
                            attr.``type`` "button"
                            on.click (fun _ -> RemoveSkill skill |> dispatch)
                        ] []
                    ]
            ]

            div [ attr.``class`` "field" ] [
                div [ attr.``class`` "control"] [
                    input [
                        attr.classes [
                            "button"
                            "is-primary"
                            "is-fullwidth"
                        ]
                        attr.``type`` "submit"
                        attr.value "Update"
                        attr.disabled (String.IsNullOrEmpty model.Name)
                    ]
                ]
            ]
        ]
    ]

let departmentBox model dispatch =
    let currentDepartmentName =
        match model.OriginalProject with
        | Some proj -> proj.DepartmentId |> snd
        | None -> ""

    div [ attr.``class`` "box" ] [
        p [] [
            strong [] [ text "Current Department" ]
            text $": {currentDepartmentName}"
        ]

        form [ on.submit (fun _ -> UpdateDepartment |> dispatch) ] [
            div [ attr.``class`` "field" ] [
                label [ attr.``class`` "label" ] [
                    text "Department"
                ]

                div [ attr.``class`` "control" ] [
                    div [ attr.``class`` "select" ] [
                        select [ on.change (fun e -> e.Value |> unbox |> SetDepartment |> dispatch) ] [
                            forEach model.Departments
                            <| (fun (deptId, deptName) ->
                                option [ attr.value deptId ] [
                                    text deptName
                                ]
                            )
                        ]
                    ]
                ]
            ]

            div [ attr.``class`` "field" ] [
                div [ attr.``class`` "control" ] [
                    input [
                        attr.classes [
                            "button"
                            "is-primary"
                            "is-fullwidth"
                        ]
                        attr.``type`` "submit"
                        attr.value "Update Department"
                        attr.disabled (Option.isNone model.SelectedDept)
                    ]
                ]
            ]
        ]
    ]

let view model dispatch =
    concat [
        p [ attr.``class`` "title" ] [ text "Edit Project" ]
        p [ attr.``class`` "subtitle" ] [
            match model.OriginalProject with
            | Some proj -> proj.Name
            | None -> ""
            |> text
        ]

        cond model.LoadingStatus
        <| function
            | LoadingComplete ->
                concat [
                    mainEditBox model dispatch

                    cond model.SignInRole
                    <| function
                        | Some Admin -> departmentBox model dispatch
                        | _ -> empty
                ]
            | load -> ProgressBar.createDeterminateBar load

        cond model.Error
        <| function
            | Some errMsg -> ErrorNotification.errorNotifDanger errMsg (fun _ -> dispatch ClearError)
            | None ->
                cond model.Success
                <| function
                    | Some successMsg ->
                        ErrorNotification.errorNotifSuccess successMsg (fun _ -> dispatch ClearSuccess)
                    | None -> empty
    ]
