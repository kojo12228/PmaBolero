namespace PmaBolero.Server

open System
open Microsoft.AspNetCore.Hosting
open Bolero.Remoting.Server

open PmaBolero.Server.Models.EmployeeDataInternal
open PmaBolero.Client.Models
open PmaBolero.Server.Repositories.CosmoDb.Backend
open System.Security.Claims
open Microsoft.AspNetCore.Authorization

type ProjectService(ctx: IRemoteContext, env: IWebHostEnvironment) =
    inherit RemoteHandler<EmployeeData.ProjectService>()

    override this.Handler = {
        createProject = ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin, pm")] <| fun newProj -> async {
            let! newProjId = getNextProjectIdAsync()

            let! pmIdIsPm =
                match newProj.ProjectManagerId with
                | None -> async { return true }
                | Some pmId ->
                    async {
                        let! pmOpt = getEmployeeAsync pmId

                        return
                            pmOpt
                            |> Option.filter (fun pm -> pm.Role = Auth.ProjectManager)
                            |> Option.isSome
                    }

            let! allDevIdsAreDevs =
                async {
                    let! devs =
                        Array.map getEmployeeAsync newProj.DeveloperIds
                        |> Async.Parallel

                    return
                        devs
                        |> Array.map (fun devOpt -> Option.filter (fun dev -> dev.Role = Auth.Developer) devOpt)
                        |> Array.forall Option.isSome
                }

            let! validDeptId =
                async {
                    let! deptOpt = getDepartmentAsync newProj.DepartmentId
                    return Option.isSome deptOpt
                }

            if (validDeptId || pmIdIsPm || allDevIdsAreDevs) |> not
            then return None
            else
                let project: Project =
                    {
                        Id = newProjId
                        ProjectName = newProj.ProjectName
                        Description = newProj.Description
                        Status = EmployeeData.Pending
                        SkillRequirements = newProj.SkillRequirements
                    }

                // Add project to department
                let! departmentProjects = getDepartmentProjectsItemAsync newProj.DepartmentId
                let newDeptProjs =
                    departmentProjects.Value // TODO Correctly check for null
                    |> Set.add newProjId
                do! upsertDepartmentProjectsAsync newProj.DepartmentId newDeptProjs |> Async.Ignore

                // Add devs to project
                do! upsertProjectDevelopersAsync newProjId newProj.DeveloperIds |> Async.Ignore

                // Add PM to project
                do! upsertProjectPMAsync newProjId newProj.ProjectManagerId |> Async.Ignore

                // Store remaining data about project
                do! upsertProjectAsync project |> Async.Ignore

                return Some newProjId
        }

        getProjects = ctx.Authorize <| fun () -> async {
            let! projects = getProjectsAsync()

            return!
                projects
                |> Map.toArray
                |> Array.map (snd >> toClientProjectAsync)
                |> Async.Parallel
        }

        getProject = ctx.Authorize <| fun projectId -> async {
            let! projectOpt = getProjectAsync projectId

            match projectOpt with
            | None -> return None
            | Some project ->
                let! clientProject = toClientProjectAsync project
                return Some clientProject
        }

        assignToDepartment =
            ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin, pm")] <| fun (projId, newDeptId) -> async {
                let! projectOpt = getProjectAsync projId
                let! allDepartmentProjects = getDepartmentProjectsAsync()

                let deptOpt =
                    projectOpt
                    |> Option.bind (fun _ ->
                        allDepartmentProjects
                        |> Map.toSeq
                        |> Seq.tryFind (fun (_, projIds) ->
                            Set.contains projId projIds))
                let newDeptProjsOpt =
                    Map.tryFind newDeptId allDepartmentProjects

                match projectOpt, deptOpt, newDeptProjsOpt with
                | Some proj, Some (oldDeptId, oldDeptProjs), Some newDeptProjs ->
                    let updatedOldDeptProjs = Set.remove projId oldDeptProjs
                    let updatedNewDeptProjs = Set.add projId newDeptProjs

                    do! upsertDepartmentProjectsAsync oldDeptId updatedOldDeptProjs |> Async.Ignore
                    do! upsertDepartmentProjectsAsync newDeptId updatedNewDeptProjs |> Async.Ignore

                    let! clientProject = toClientProjectAsync proj
                    return Some clientProject
                | _ -> return None
            }

        updateProject =
            ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin, pm")] <| fun (projDetails) -> async {
                let! projectOpt = getProjectAsync projDetails.Id
                let! employees = getEmployeesAsync()

                let allNewDevIdsAreDevs =
                    projDetails.DeveloperIds
                    |> Array.forall (fun devId ->
                        let devOpt = Map.tryFind devId employees
                        match devOpt with
                        | Some dev -> dev.Role = Auth.Developer
                        | None -> false)

                let newProjDevsSet = 
                    if allNewDevIdsAreDevs
                    then Some <| Set.ofArray projDetails.DeveloperIds
                    else None

                match projectOpt, newProjDevsSet with
                | Some proj, Some devs ->
                    let newProj =
                        {
                            proj with
                                ProjectName = projDetails.ProjectName
                                Description = projDetails.Description
                                Status = projDetails.Status
                                SkillRequirements = projDetails.SkillRequirements
                        }
                    do! upsertProjectDevelopersAsync proj.Id devs |> Async.Ignore
                    do! upsertProjectAsync newProj |> Async.Ignore
                    
                    let! clientProject = toClientProjectAsync newProj
                    return Some clientProject
                | _ -> return None
            }

        updateProjectElevated = ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin")] <| fun (projDetails) -> async {
            let! projectOpt = getProjectAsync projDetails.Id
            let! employees = getEmployeesAsync()

            let allNewDevIdsAreDevs =
                projDetails.DeveloperIds
                |> Array.forall (fun devId ->
                    let devOpt = Map.tryFind devId employees
                    match devOpt with
                    | Some dev -> dev.Role = Auth.Developer
                    | None -> false)

            let newProjDevsSet = 
                if allNewDevIdsAreDevs
                then Some <| Set.ofArray projDetails.DeveloperIds
                else None

            let pmOptOpt =
                match projDetails.ProjectManagerId with
                | Some pmId -> 
                    let pmOpt = Map.tryFind pmId employees
                    match pmOpt with
                    | Some pm ->
                        if pm.Role = Auth.ProjectManager
                        then Some <| Some pmId
                        else None
                    | None -> None
                | None -> Some None

            match projectOpt, newProjDevsSet, pmOptOpt with
            | Some proj, Some devs, Some pmOpt ->
                let newProj =
                    {
                        proj with
                            ProjectName = projDetails.ProjectName
                            Description = projDetails.Description
                            Status = projDetails.Status
                            SkillRequirements = projDetails.SkillRequirements
                    }
                do! upsertProjectDevelopersAsync proj.Id devs |> Async.Ignore
                do! upsertProjectPMAsync proj.Id pmOpt |> Async.Ignore
                do! upsertProjectAsync proj |> Async.Ignore

                let! clientProject = toClientProjectAsync newProj
                return Some clientProject
            | _ -> return None
        }

        deleteProject = ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin")] <| fun projId -> async {
            let! projectOpt = getProjectAsync projId

            match projectOpt with
            | Some _ ->
                // Remove PM from project
                do! deleteProjectPMMappingAsync projId |> Async.Ignore
                // Remove devs from project
                do! deleteProjectDeveloperMappingAsync projId |> Async.Ignore
                // Remove project from department
                let! allDepartmentProjects = getDepartmentProjectsAsync()
                let deptId, departmentProjects =
                    allDepartmentProjects
                    |> Map.pick (fun deptId projIds ->
                        if Set.contains projId projIds
                        then Some (deptId, projIds)
                        else None)
                let updatedDepartmentProjects =
                    Set.remove projId departmentProjects
                do! upsertDepartmentProjectsAsync deptId updatedDepartmentProjects |> Async.Ignore
                // Remove project from stored project
                do! deleteProjectAsync projId |> Async.Ignore
                return Some projId
            | None ->
                return None
        }
    }