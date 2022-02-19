module PmaBolero.Client.Pages.Create.Project

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

open PmaBolero.Client.Models.EmployeeData
open PmaBolero.Client.Helpers.ErrorNotification
open PmaBolero.Client.Helpers.ProgressBar

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

type CreateProjectTemplate = Template<"wwwroot/createproject.html">

let tryInt iStr =
    match iStr with
    | "" -> None
    | i -> Some(int i)

let optionIntToString optInt =
    match optInt with
    | Some i -> string i
    | None -> ""

let view model dispatch =
    // fsharplint:disable CanBeReplacedWithComposition

    CreateProjectTemplate
        .CreateProject()
        .Name(model.Name, (fun name -> dispatch (SetName name)))
        .Description(model.Description, (fun desc -> dispatch (SetDescription desc)))
        .Department(optionIntToString model.SelectedDept, (fun deptIdStr -> dispatch (SetDepartment(int deptIdStr))))
        .DeptOptions(
            forEach model.Departments
            <| (fun (deptId, deptName) ->
                CreateProjectTemplate
                    .DeptItem()
                    .DeptId(deptId)
                    .Name(deptName)
                    .Elt())
        )
        .DeptIsLoading(
            if model.IsLoading.Departments then
                "is-loading"
            else
                ""
        )
        .ProjectManager(optionIntToString model.SelectedPm, (fun pmId -> dispatch (SetProjectManager(tryInt pmId))))
        .PmOptions(
            forEach model.ProjectManagers
            <| (fun (pmId, pmName) ->
                CreateProjectTemplate
                    .PmItem()
                    .PmId(pmId)
                    .Name(pmName)
                    .Elt())
        )
        .PmIsLoading(
            if model.IsLoading.Pms then
                "is-loading"
            else
                ""
        )
        .DevOptions(
            cond model.IsLoading.Devs
            <| function
                | true -> createIndeterminateBar ()
                | false ->
                    forEach model.Developers
                    <| (fun (devId, devName) ->
                        CreateProjectTemplate
                            .DevItem()
                            .DevName(devName)
                            .DevCheckbox(
                                Set.contains devId model.SelectedDevs,
                                fun selected ->
                                    match selected with
                                    | true -> dispatch (AddDev devId)
                                    | false -> dispatch (RemoveDev devId)
                            )
                            .Elt())
        )
        .SkillField(model.SkillField, (fun sk -> dispatch (SetSkillField sk)))
        .AddSkillClick(fun _ -> dispatch AddSkill)
        .SkillTags(
            forEach model.SkillsRequired
            <| (fun skill ->
                CreateProjectTemplate
                    .SkillTag()
                    .Skill(skill)
                    .DeleteSkill(fun _ -> dispatch (RemoveSkill skill))
                    .Elt())
        )
        .DisabledSubmit(String.IsNullOrEmpty model.Name)
        .SubmitProject(fun _ -> dispatch SubmitProject)
        .ErrorNotification(
            cond model.Error
            <| function
                | None -> empty
                | Some msg -> errorNotifDanger msg (fun _ -> dispatch ClearError)
        )
        .Elt()
