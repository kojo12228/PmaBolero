namespace PmaBolero.Server

open System
open Microsoft.AspNetCore.Hosting
open Bolero.Remoting.Server

open PmaBolero.Server.Models.EmployeeDataInternal
open PmaBolero.Server.Repositories.CosmoDb.Backend
open PmaBolero.Client.Models
open Microsoft.AspNetCore.Authorization
open Bolero.Remoting

type EmployeeService(ctx: IRemoteContext, env: IWebHostEnvironment) =
    inherit RemoteHandler<EmployeeData.EmployeeService>()

    override this.Handler =
        {
            createEmployee = ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin")] <| fun newEmployee -> async {
                let! departments = getDepartmentsAsync()
                let! departmentEmpls = getDepartmentEmployeesAsync()

                if
                    Map.exists (fun _ dept -> dept.Id = newEmployee.DepartmentId) departments
                then
                    let! newId = getNextEmployeeIdAsync()
                    
                    let employee =
                        {
                            Id = newId
                            Email = newEmployee.Email
                            FullName = newEmployee.FullName
                            Role = newEmployee.Role
                            Skills = newEmployee.Skills
                        }

                    do! upsertEmployeeAsync employee |> Async.Ignore

                    let newDeptEmployees = Set.add newId departmentEmpls.[newEmployee.DepartmentId]
                    do! upsertDepartmentEmployeesAsync newEmployee.DepartmentId newDeptEmployees |> Async.Ignore 

                    return Some newId
                else return None
            }

            getEmployees = ctx.Authorize <| fun () -> async {
                let! employees = getEmployeesAsync()

                return!
                    employees
                    |> Map.toArray
                    |> Array.map snd
                    |> Array.map (toClientEmployeeAsync)
                    |> Async.Parallel
            }

            getProjectManagers = ctx.Authorize <| fun () -> async {
                let! employees = getEmployeesAsync()

                return
                    employees
                    |> Map.toArray
                    |> Array.filter (fun (_, e) -> e.Role = Auth.ProjectManager)
                    |> Array.map (fun (_, e) -> e.Id, e.FullName)
            }

            getDevelopers = ctx.Authorize <| fun () -> async {
                let! employees = getEmployeesAsync()

                return
                    employees
                    |> Map.toArray
                    |> Array.filter (fun (_, e) -> e.Role = Auth.Developer)
                    |> Array.map (fun (_, e) -> e.Id, e.FullName)
            }

            getEmployee = ctx.Authorize <| fun employeeId -> async {
                let! employeeOpt = getEmployeeAsync employeeId

                match employeeOpt with
                | Some e ->
                    let! empl = toClientEmployeeAsync e
                    return Some (empl)
                | None -> return None
            }

            transferToDepartment =
                ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin")] <| fun (employeeId, newDeptId) -> async {
                    let! departmentEmpls = getDepartmentEmployeesAsync()

                    let currentDept = Map.tryFindKey (fun deptId emplIds -> Set.contains employeeId emplIds) departmentEmpls
                    let currentDeptEmpls =
                        currentDept
                        |> Option.bind (fun id -> Map.tryFind id departmentEmpls) 
                        |> Option.bind (fun emplIds -> Some (Set.remove employeeId emplIds))

                    let deptEmployees =
                        Map.tryFind newDeptId departmentEmpls
                        |> Option.bind (fun emplIds -> Some (Set.add employeeId emplIds))

                    match currentDept, currentDeptEmpls, deptEmployees with
                    | Some deptId, Some deptEmpls, Some newDeptEmpls ->
                        do! upsertDepartmentEmployeesAsync deptId deptEmpls |> Async.Ignore
                        do! upsertDepartmentEmployeesAsync newDeptId newDeptEmpls |> Async.Ignore

                        let! employee = getEmployeeAsync employeeId

                        match employee with
                        | Some e ->
                            let! clientEmployee = toClientEmployeeAsync e

                            return Some clientEmployee
                        | None -> return None
                    | _ -> return None
                }

            changeRole =
                ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin")] <| fun (employeeId, newRole) -> async {
                    let! employees = getEmployeesAsync()
                    let! projDevs = getProjectDevelopersAsync()
                    let! projPm = getProjectPMAsync()

                    let employeeOpt = Map.tryFind employeeId employees

                    match employeeOpt with
                    | Some e ->
                        match e.Role, e.Role <> newRole with
                        // If was PM, and not PM anymore, then remove as PM
                        | Auth.ProjectManager, true ->
                            let pmProjects =
                                projPm
                                |> Map.filter (fun _ pmIdOpt ->
                                    match pmIdOpt with
                                    | Some pmId -> pmId = employeeId
                                    | _ -> false)
                                |> Map.toSeq
                                |> Seq.map fst

                            for pmProjId in pmProjects do
                                do! upsertProjectPMAsync pmProjId None |> Async.Ignore
                        // If was dev and not dev anymore, then remove as dev from projects
                        | Auth.Developer, true ->
                            let devProjects =
                                projDevs
                                |> Map.filter (fun _ devIds -> Set.contains employeeId devIds)
                                |> Map.toSeq
                                |> Seq.map fst

                            for projId in devProjects do
                                let newDevs = Set.remove employeeId projDevs.[projId]
                                do! upsertProjectDevelopersAsync projId newDevs |> Async.Ignore
                        | _ -> ()

                        let newEmpl = { e with Role = newRole }

                        do! upsertEmployeeAsync newEmpl |> Async.Ignore

                        let! clientEmployee = toClientEmployeeAsync newEmpl

                        return Some clientEmployee
                    | None -> return None
                }

            updateEmployee = ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin")] <| fun (updatedEmplDetails) -> async {
                let! storedEmpl = getEmployeeAsync updatedEmplDetails.Id
                match storedEmpl with
                | Some empl ->
                    let newEmpl =
                        {
                            empl with
                                FullName = updatedEmplDetails.FullName
                                Skills = updatedEmplDetails.Skills
                        }
                    do! upsertEmployeeAsync newEmpl |> Async.Ignore

                    let! employee = getEmployeeAsync empl.Id
                    
                    match employee with
                    | None -> return None
                    | Some e ->
                        let! clientEmployee = toClientEmployeeAsync e
                        return Some clientEmployee
                | None ->
                    return None
            }

            deleteEmployee = ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin")] <| fun employeeId -> async {
                let! empl = getEmployeeAsync employeeId

                match empl with
                | Some e ->
                   // Remove employee from projects
                    match e.Role with
                    | Auth.Developer ->
                        let! projDevs = getProjectDevelopersAsync()
                        let newProjDev =
                           projDevs
                           |> Map.map (fun _ emplIds -> Set.remove employeeId emplIds)
                        
                        do! 
                            [|
                                for projId, devs in Map.toSeq newProjDev do
                                    if devs <> projDevs.[projId]
                                    then
                                        upsertProjectDevelopersAsync projId devs
                                        |> Async.Ignore
                            |]
                            |> Async.Parallel
                            |> Async.Ignore
                    | Auth.ProjectManager ->
                        let! projPMs = getProjectPMAsync()
                        let newProjPms =
                            projPMs
                            |> Map.map (fun _ pmIdOpt ->
                                match pmIdOpt with
                                | Some pmId when pmId = employeeId ->
                                    None
                                | x -> x)
                        do! 
                            [|
                                for projId, pm in Map.toSeq newProjPms do
                                    if pm <> projPMs.[projId]
                                    then
                                        upsertProjectPMAsync projId pm
                                        |> Async.Ignore
                            |]
                            |> Async.Parallel
                            |> Async.Ignore
                    | _ -> ()

                    let! departmentEmployees = getDepartmentEmployeesAsync()

                    // Remove employee from projects
                    let employeeDeptId =
                        departmentEmployees
                        |> Map.findKey (fun _ emplIds -> Set.contains employeeId emplIds)

                    let newDeptEmployees =
                        departmentEmployees
                        |> Map.find employeeDeptId
                        |> Set.remove employeeId

                    do! upsertDepartmentEmployeesAsync employeeDeptId newDeptEmployees |> Async.Ignore

                    // Remove employee from listed employees
                    do! deleteEmployeeAsync employeeId |> Async.Ignore

                    return Some (employeeId)
                | None -> return None
            }
        }