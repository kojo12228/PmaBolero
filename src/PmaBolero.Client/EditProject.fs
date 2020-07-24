module PmaBolero.Client.EditProject

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.Models
open PmaBolero.Client.Models.EmployeeData

open PmaBolero.Client.Helpers

type LoadingTypes =
    | LoadingEmpty
    | LoadingQuarter
    | LoadingHalf
    | LoadingThreeQuarter
    | LoadingComplete

let loadingToVal load =
    match load with
    | LoadingEmpty -> 0
    | LoadingQuarter -> 25
    | LoadingHalf -> 50
    | LoadingThreeQuarter -> 75
    | LoadingComplete -> 100

type Model =
    {
        SignInRole: Auth.Role option
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
        IsLoadingPercentage: LoadingTypes
        Error: string option
        Success: string option
    }

let initModel =
    {
        SignInRole = None
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
        IsLoadingPercentage = LoadingEmpty
        Error = None
        Success = None
    }

type Message =
    | InitMessage of int * (Auth.Role option)

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
        | Some Auth.Admin ->
            { model with SignInRole = signInRole },
            Cmd.batch [
                Cmd.ofMsg (GetProject projId)
                Cmd.ofMsg GetDevelopers
                Cmd.ofMsg GetProjectManagers
                Cmd.ofMsg GetDepartments
            ]
        | Some Auth.ProjectManager ->
            { model with SignInRole = signInRole },
            Cmd.batch [
                Cmd.ofMsg (GetProject projId)
                Cmd.ofMsg GetDevelopers
            ]
        | Some Auth.Developer ->
            { model with Error = Some "You do not have permission to edit this project" }, Cmd.none
        | None ->
            { model with Error = Some "Unexpected error, sign in information unavailable" }, Cmd.none

    | GetProject projId ->
        model, Cmd.ofAuthorized remoteProject.getProject projId RecvProject Error
    | RecvProject (Some (Some proj)) ->
        let updatedModel =
            {
                model with
                    OriginalProject = Some proj
                    Name = proj.Name
                    Description = proj.Description
                    Status = string proj.Status
                    SelectedDevs = proj.DeveloperIds |> Array.map fst |> Set.ofArray
                    SkillsRequired = proj.SkillRequirements |> Set.ofArray
                    SelectedDept = proj.DepartmentId |> fst |> Some
                    SelectedPm = proj.ProjectManagerId |> Option.map fst
                    IsLoadingPercentage =
                        match model.SignInRole with
                        | Some Auth.Admin -> LoadingQuarter
                        | Some Auth.ProjectManager -> LoadingHalf
                        | _ -> LoadingEmpty
            }

        updatedModel, Cmd.none
    | RecvProject (Some None) ->
        { model with Error = Some "No project exists with this ID" }, Cmd.none
    | GetDevelopers ->
        model, Cmd.ofAuthorized remoteEmployee.getDevelopers () RecvDevelopers Error
    | RecvDevelopers (Some devs) ->
        {
            model with
                Developers = devs
                IsLoadingPercentage =
                    match model.SignInRole with
                    | Some Auth.Admin -> LoadingHalf
                    | Some Auth.ProjectManager -> LoadingComplete
                    | _ -> LoadingEmpty
        }, Cmd.none
    | GetProjectManagers ->
        model, Cmd.ofAuthorized remoteEmployee.getProjectManagers () RecvProjectManagers Error
    | RecvProjectManagers (Some pms) ->
        {
            model with
                ProjectManagers = pms
                IsLoadingPercentage = LoadingThreeQuarter
        }, Cmd.none
    | GetDepartments ->
        model, Cmd.ofAuthorized remoteDepartment.getDepartmentIds () RecvDepartments Error
    | RecvDepartments (Some depts) ->
        {
            model with
                Departments = depts
                IsLoadingPercentage = LoadingComplete
        }, Cmd.none
    | RecvProject None | RecvDevelopers None
    | RecvDepartments None | RecvProjectManagers ->
        { model with Error = Some "Unable to get data due to authentication error" }, Cmd.none

    | SetName name ->
        { model with Name = name }, Cmd.none
    | SetDescription desc ->
        { model with Description = desc }, Cmd.none
    | SetStatus status ->
        { model with Status = status }, Cmd.none
    | AddDev devId ->
        { model with SelectedDevs = Set.add devId model.SelectedDevs }, Cmd.none
    | RemoveDev devId ->
        { model with SelectedDevs = Set.remove devId model.SelectedDevs }, Cmd.none
    | SetSkillField skillStr ->
        { model with SkillField = skillStr }, Cmd.none
    | AddSkill ->
        let newSkills = Set.add model.SkillField model.SkillsRequired
        { model with SkillsRequired = newSkills; SkillField = "" }, Cmd.none
    | RemoveSkill skill ->
        { model with SkillsRequired = Set.remove skill model.SkillsRequired }, Cmd.none
    | SetProjectManager projId ->
        { model with SelectedPm = projId }, Cmd.none
    | SetDepartment dept ->
        { model with SelectedDept = Some dept }, Cmd.none

    | UpdateProject ->
        let statusResolver statusStr =
            match statusStr with
            | "Pending" -> Pending
            | "Active" -> Active
            | "Complete" -> Complete
            | _ -> Pending

        match model.SignInRole with
        | Some Auth.Admin ->
            let updatedProject =
                {|
                    Id = model.OriginalProject.Value.Id
                    ProjectName = model.Name
                    Description = model.Description
                    Status = statusResolver model.Status
                    ProjectManagerId = model.SelectedPm
                    DeveloperIds = model.SelectedDevs |> Set.toArray
                    SkillRequirements = model.SkillsRequired |> Set.toArray
                |}

            { model with IsLoadingPercentage = LoadingHalf },
            Cmd.ofAuthorized remoteProject.updateProjectElevated updatedProject UpdateProjectResponse Error
        | Some Auth.ProjectManager ->
            let updatedProject =
                {|
                    Id = model.OriginalProject.Value.Id
                    ProjectName = model.Name
                    Description = model.Description
                    Status = statusResolver model.Status
                    DeveloperIds = model.SelectedDevs |> Set.toArray
                    SkillRequirements = model.SkillsRequired |> Set.toArray
                |}

            { model with IsLoadingPercentage = LoadingHalf },
            Cmd.ofAuthorized remoteProject.updateProject updatedProject UpdateProjectResponse Error
        | _ ->
            { model with Error = Some "Unable to update project" }, Cmd.none
    | UpdateProjectResponse (Some (Some proj)) ->
        let updatedModel =
            {
                model with
                    OriginalProject = Some proj
                    Name = proj.Name
                    Description = proj.Description
                    Status = string proj.Status
                    SelectedDevs = proj.DeveloperIds |> Array.map fst |> Set.ofArray
                    SkillsRequired = proj.SkillRequirements |> Set.ofArray
                    SelectedDept = proj.DepartmentId |> fst |> Some
                    SelectedPm = proj.ProjectManagerId |> Option.map fst
                    IsLoadingPercentage = LoadingComplete
                    Success = Some "Project successfully updated"
            }

        updatedModel, Cmd.none
    | UpdateDepartment ->
        match model.SelectedDept with
        | Some deptId ->
            { model with IsLoadingPercentage = LoadingHalf },
            Cmd.ofAuthorized remoteProject.assignToDepartment (model.OriginalProject.Value.Id, deptId) UpdateDepartmentResponse Error
        | None -> { model with Error = Some "Required to set a department" }, Cmd.none
    | UpdateDepartmentResponse (Some (Some proj)) ->
        {
            model with
                OriginalProject = Some proj
                SelectedDept = proj.DepartmentId |> fst |> Some
                Success = Some "Project successfully updated"
                IsLoadingPercentage = LoadingComplete
        }, Cmd.none
    | UpdateProjectResponse (Some None) | UpdateDepartmentResponse (Some None) ->
        {
            model with
                Error = Some "Unable to updated project at this time."
                IsLoadingPercentage = LoadingComplete
        }, Cmd.none
    | UpdateProjectResponse None | UpdateDepartmentResponse (None) ->
        {
            model with
                Error = Some "Unable to send data due to authentication error"
                IsLoadingPercentage = LoadingComplete

        }, Cmd.none

    | Error e ->
        { model with Error = Some e.Message }, Cmd.none
    | ClearError ->
        { model with Error = None }, Cmd.none
    | ClearSuccess ->
        { model with Success = None }, Cmd.none

type EditProjectTemplate = Template<"wwwroot/editproject.html">

// Move into separate module to remove duplication
let tryInt iStr =
    match iStr with
    | "" -> None
    | i -> Some (int i)

let optionIntToString optInt =
    match optInt with
    | Some i -> string i
    | None -> ""

let viewMainEditBox model dispatch =
    EditProjectTemplate
        .MainEditProjectBox()
        .Name(model.Name, fun name -> dispatch (SetName name))
        .Description(model.Description, fun desc -> dispatch (SetDescription desc))
        .Status(model.Status, fun status -> dispatch (SetStatus status))
        .ProjectManagerField(
            cond model.SignInRole <| function
            | Some Auth.Admin ->
                EditProjectTemplate
                    .PmField()
                    .ProjectManager(
                        optionIntToString model.SelectedPm,
                        fun pmId -> dispatch (SetProjectManager (tryInt pmId))
                    )
                    .PmOptions(
                        forEach model.ProjectManagers <| (fun (pmId, pmName) ->
                            EditProjectTemplate
                                .PmItem()
                                .PmId(pmId)
                                .Name(pmName)
                                .Elt()
                        )
                    )
                    .Elt()
            | _ -> empty
        )
        .DevOptions(
            forEach model.Developers <| (fun (devId, devName) ->
                EditProjectTemplate
                    .DevItem()
                    .DevName(devName)
                    .DevCheckbox(
                        Set.contains devId model.SelectedDevs,
                        fun selected ->
                            match selected with
                            | true -> dispatch (AddDev devId)
                            | false -> dispatch (RemoveDev devId)
                    )
                    .Elt()
            )
        )
        .SkillField(model.SkillField,fun sk -> dispatch (SetSkillField sk))
        .AddSkillClick(fun _ -> dispatch AddSkill)
        .SkillTags(
            forEach model.SkillsRequired <| (fun skill ->
                EditProjectTemplate
                    .SkillTag()
                    .Skill(skill)
                    .DeleteSkill(fun _ -> dispatch (RemoveSkill skill))
                    .Elt()
            )
        )
        .DisabledSubmit(
            String.IsNullOrEmpty model.Name
        )
        .SubmitProject(fun _ -> dispatch UpdateProject)
        .Elt()

let viewDepartmentBox model dispatch =
    EditProjectTemplate
        .EditDepartment()
        .Department(
            optionIntToString model.SelectedDept,
            fun deptIdStr -> dispatch (SetDepartment (int deptIdStr))
        )
        .DeptOptions(
            forEach model.Departments <| (fun (deptId, deptName) ->
                EditProjectTemplate
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
    EditProjectTemplate
        .EditProject()
        .ProjectName(
            match model.OriginalProject with
            | Some proj -> proj.Name
            | None -> ""
        )
        .ProgressBar(
            cond model.IsLoadingPercentage <| function
            | LoadingComplete -> empty
            | load -> ProgressBar.createDeterminateBar (loadingToVal load)
        )
        .MainEditBox(
            cond model.IsLoadingPercentage <| function
            | LoadingComplete ->
                viewMainEditBox model dispatch
            | _ -> empty
        )
        .ChangeDepartmentBox(
            cond model.IsLoadingPercentage <| function
            | LoadingComplete ->
                cond model.SignInRole <| function
                | Some Auth.Admin ->
                    viewDepartmentBox model dispatch
                | _ -> empty
            | _ -> empty
        )
        .Notification(
            cond model.Error <| function
            | Some errMsg ->
                ErrorNotification.errorNotifDanger errMsg (fun _ -> dispatch ClearError)
            | None ->
                cond model.Success <| function
                | Some successMsg ->
                    ErrorNotification.errorNotifSuccess successMsg (fun _ -> dispatch ClearSuccess)
                | None -> empty
        )
        .Elt()