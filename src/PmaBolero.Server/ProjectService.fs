namespace PmaBolero.Server

open System
open Microsoft.AspNetCore.Hosting
open Bolero.Remoting.Server

open PmaBolero.Shared.Models

open PmaBolero.Server.Models.EmployeeDataInternal
open PmaBolero.Client.Models
open PmaBolero.Server.Repositories.Mock
open System.Security.Claims
open Microsoft.AspNetCore.Authorization

type ProjectService(ctx: IRemoteContext, env: IWebHostEnvironment) =
    inherit RemoteHandler<EmployeeData.ProjectService>()

    override this.Handler = {
        createProject = ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin, pm")] <| fun newProj -> async {
            let newProjId = Backend.getNextProjectId()

            let validDeptId() = Map.containsKey newProj.DepartmentId Backend.departments

            let pmIdIsPm() =
                match newProj.ProjectManagerId with
                | None -> true
                | Some pmId ->
                    Backend.employees
                    |> Map.tryFind pmId
                    |> Option.filter (fun pm -> pm.Role = ProjectManager)
                    |> Option.isSome

            let allDevIdsAreDevs() =
                newProj.DeveloperIds
                |> Array.map (fun devId -> Map.tryFind devId Backend.employees)
                |> Array.map (fun devOpt -> Option.filter (fun dev -> dev.Role = Developer) devOpt)
                |> Array.forall Option.isSome

            if (validDeptId() || pmIdIsPm() || allDevIdsAreDevs()) |> not
            then return None
            else
                let project: Project =
                    {
                        Id = newProjId
                        ProjectName = newProj.ProjectName
                        Description = newProj.Description
                        Status = Pending
                        SkillRequirements = newProj.SkillRequirements
                    }

                // Add project to department
                let newDeptProjs =
                    Backend.departmentProjects
                    |> Map.find newProj.DepartmentId
                    |> Set.add newProjId
                Backend.departmentProjects <- Map.add newProj.DepartmentId newDeptProjs Backend.departmentProjects

                // Add devs to project
                Backend.projectDevs <-
                    Backend.projectDevs
                    |> Map.add newProjId (Set.ofArray newProj.DeveloperIds)

                // Add PM to project
                Backend.projectPM <- Map.add newProjId newProj.ProjectManagerId Backend.projectPM

                // Store remaining data about project
                Backend.projects <- Map.add newProjId project Backend.projects

                return Some newProjId
        }

        getProjects = ctx.Authorize <| fun () -> async {
            return
                Backend.projects
                |> Map.toArray
                |> Array.map (snd >> Backend.toClientProject)
        }

        getProject = ctx.Authorize <| fun projectId -> async {
            return
                Backend.projects
                |> Map.tryFind projectId
                |> Option.map Backend.toClientProject
        }

        assignToDepartment =
            ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin, pm")] <| fun (projId, newDeptId) -> async {
                let projectOpt = Map.tryFind projId Backend.projects

                let deptOpt =
                    projectOpt
                    |> Option.bind (fun _ ->
                        Backend.departmentProjects
                        |> Map.toSeq
                        |> Seq.tryFind (fun (_, projIds) ->
                            Set.contains projId projIds))
                let newDeptProjsOpt =
                    Map.tryFind newDeptId Backend.departmentProjects

                match projectOpt, deptOpt, newDeptProjsOpt with
                | Some proj, Some (oldDeptId, oldDeptProjs), Some newDeptProjs ->
                    let updatedOldDeptProjs = Set.remove projId oldDeptProjs
                    let updatedNewDeptProjs = Set.add projId newDeptProjs

                    Backend.departmentProjects <- Map.add oldDeptId updatedOldDeptProjs Backend.departmentProjects
                    Backend.departmentProjects <- Map.add newDeptId updatedNewDeptProjs Backend.departmentProjects

                    return Some (Backend.toClientProject proj)
                | _ -> return None
            }

        updateProject =
            ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin, pm")] <| fun (projDetails) -> async {
                let projectOpt = Map.tryFind projDetails.Id Backend.projects

                let allNewDevIdsAreDevs =
                    projDetails.DeveloperIds
                    |> Array.forall (fun devId ->
                        let devOpt = Map.tryFind devId Backend.employees
                        match devOpt with
                        | Some dev -> dev.Role = Developer
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
                    Backend.projectDevs <- Map.add proj.Id devs Backend.projectDevs
                    Backend.projects <- Map.add proj.Id proj Backend.projects
                    return Some (Backend.toClientProject newProj)
                | _ -> return None
            }

        updateProjectElevated = ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin")] <| fun (projDetails) -> async {
            let projectOpt = Map.tryFind projDetails.Id Backend.projects

            let allNewDevIdsAreDevs =
                projDetails.DeveloperIds
                |> Array.forall (fun devId ->
                    let devOpt = Map.tryFind devId Backend.employees
                    match devOpt with
                    | Some dev -> dev.Role = Developer
                    | None -> false)

            let newProjDevsSet = 
                if allNewDevIdsAreDevs
                then Some <| Set.ofArray projDetails.DeveloperIds
                else None

            let pmOptOpt =
                match projDetails.ProjectManagerId with
                | Some pmId -> 
                    let pmOpt = Map.tryFind pmId Backend.employees
                    match pmOpt with
                    | Some pm ->
                        if pm.Role = ProjectManager
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
                Backend.projectDevs <- Map.add proj.Id devs Backend.projectDevs
                Backend.projectPM <- Map.add proj.Id pmOpt Backend.projectPM
                Backend.projects <- Map.add proj.Id proj Backend.projects
                return Some (Backend.toClientProject newProj)
            | _ -> return None
        }

        deleteProject = ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin")] <| fun projId -> async {
            let projectOpt = Map.tryFind projId Backend.projects

            match projectOpt with
            | Some _ ->
                // Remove project from department
                Backend.projectPM <- Map.remove projId Backend.projectPM
                // Remove devs from project
                Backend.projectDevs <- Map.remove projId Backend.projectDevs
                // Remove PM from project
                Backend.departmentProjects <- Map.map (fun _ projIds -> Set.remove projId projIds) Backend.departmentProjects
                // Remove project from stored project
                Backend.projects <- Map.remove projId Backend.projects
                return Some projId
            | None ->
                return None
        }
    }