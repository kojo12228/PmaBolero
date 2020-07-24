namespace PmaBolero.Server

open System
open Microsoft.AspNetCore.Hosting
open Bolero.Remoting.Server

open PmaBolero.Server.Models.EmployeeDataInternal
open PmaBolero.Client.Models
open PmaBolero.Server.Repositories.Mock
open Microsoft.AspNetCore.Authorization
open Bolero.Remoting

type EmployeeService(ctx: IRemoteContext, env: IWebHostEnvironment) =
    inherit RemoteHandler<EmployeeData.EmployeeService>()

    override this.Handler =
        {
            createEmployee = ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin")] <| fun newEmployee -> async {
                if
                    Map.exists (fun _ dept -> dept.Id = newEmployee.DepartmentId) Backend.departments
                then
                    let newId = Backend.getNextEmployeeId()
                    
                    let employee =
                        {
                            Id = Backend.getNextEmployeeId()
                            Email = newEmployee.Email
                            FullName = newEmployee.FullName
                            Role = newEmployee.Role
                            Skills = newEmployee.Skills
                        }

                    Backend.employees <- Map.add newId employee Backend.employees

                    let newDeptEmployees = Set.add newId Backend.departmentEmployees.[newEmployee.DepartmentId]
                    Backend.departmentEmployees <- Map.add newEmployee.DepartmentId newDeptEmployees Backend.departmentEmployees

                    return Some newId
                else return None
            }

            getEmployees = ctx.Authorize <| fun () -> async {
                return
                    Backend.employees
                    |> Map.toArray
                    |> Array.map snd
                    |> Array.map Backend.toClientEmployee
            }

            getProjectManagers = ctx.Authorize <| fun () -> async {
                return
                    Backend.employees
                    |> Map.toArray
                    |> Array.filter (fun (_, e) -> e.Role = Auth.ProjectManager)
                    |> Array.map (fun (_, e) -> e.Id, e.FullName)
            }

            getDevelopers = ctx.Authorize <| fun () -> async {
                return
                    Backend.employees
                    |> Map.toArray
                    |> Array.filter (fun (_, e) -> e.Role = Auth.Developer)
                    |> Array.map (fun (_, e) -> e.Id, e.FullName)
            }

            getEmployee = ctx.Authorize <| fun employeeId -> async {
                let employeeOpt = Map.tryFind employeeId Backend.employees

                match employeeOpt with
                | Some e -> return Some (Backend.toClientEmployee e)
                | None -> return None
            }

            transferToDepartment =
                ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin")] <| fun (employeeId, newDeptId) -> async {
                    let currentDept = Map.tryFindKey (fun deptId emplIds -> Set.contains employeeId emplIds) Backend.departmentEmployees
                    let currentDeptEmpls =
                        currentDept
                        |> Option.bind (fun id -> Map.tryFind id Backend.departmentEmployees) 
                        |> Option.bind (fun emplIds -> Some (Set.remove employeeId emplIds))

                    let deptEmployees =
                        Map.tryFind newDeptId Backend.departmentEmployees
                        |> Option.bind (fun emplIds -> Some (Set.add employeeId emplIds))

                    match currentDept, currentDeptEmpls, deptEmployees with
                    | Some deptId, Some deptEmpls, Some newDeptEmpls ->
                        Backend.departmentEmployees <- Map.add deptId deptEmpls Backend.departmentEmployees
                        Backend.departmentEmployees <- Map.add newDeptId newDeptEmpls Backend.departmentEmployees

                        let employee = Map.find employeeId Backend.employees
                        return Some (Backend.toClientEmployee employee)
                    | _ -> return None
                }

            changeRole =
                ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin")] <| fun (employeeId, newRole) -> async {
                    let employeeOpt = Map.tryFind employeeId Backend.employees

                    match employeeOpt with
                    | Some e ->
                        match e.Role, e.Role <> newRole with
                        // If was PM, and not PM anymore, then remove as PM
                        | Auth.ProjectManager, true ->
                            let pmProjects =
                                Backend.projectPM
                                |> Map.filter (fun _ pmIdOpt ->
                                    match pmIdOpt with
                                    | Some pmId -> pmId = employeeId
                                    | _ -> false)
                                |> Map.toSeq
                                |> Seq.map fst

                            for pmProjId in pmProjects do
                                Backend.projectPM <- Map.remove pmProjId Backend.projectPM
                        // If was dev and not dev anymore, then remove as dev from projects
                        | Auth.Developer, true ->
                            let devProjects =
                                Backend.projectDevs
                                |> Map.filter (fun _ devIds -> Set.contains employeeId devIds)
                                |> Map.toSeq
                                |> Seq.map fst

                            for projId in devProjects do
                                let newDevs = Set.remove employeeId Backend.projectDevs.[projId]
                                Backend.projectDevs <- Map.add projId newDevs Backend.projectDevs
                        | _ -> ()

                        let newEmpl =
                            { e with Role = newRole }
                        Backend.employees <- Map.add employeeId newEmpl Backend.employees
                        return Some (Backend.toClientEmployee e)
                    | None -> return None
                }

            // To ensure a user cannot change their email to an admin email,
            // email cannot be used and email is used to authenticate the request
            updateEmployee = ctx.Authorize <| fun (updatedEmplDetails) -> async {
                let storedEmpl = Map.tryFind updatedEmplDetails.Id Backend.employees
                match storedEmpl with
                | Some empl ->
                    if empl.Email <> ctx.HttpContext.User.Identity.Name
                    then
                        ctx.HttpContext.Response.StatusCode <- 403
                        return None
                    else
                        let newEmpl =
                            {
                                empl with
                                    FullName = updatedEmplDetails.FullName
                                    Skills = updatedEmplDetails.Skills
                            }
                        Backend.employees <- Map.add empl.Id newEmpl Backend.employees

                        return Some ()
                | None ->
                    return None
            }

            deleteEmployee = ctx.AuthorizeWith [AuthorizeAttribute(Roles = "admin")] <| fun employeeId -> async {
                let empl = Map.tryFind employeeId Backend.employees

                match empl with
                | Some e ->
                    // Remove employee from projects
                    match e.Role with
                    | Auth.Developer ->
                        let newProjDev =
                            Backend.projectDevs
                            |> Map.map (fun _ emplIds -> Set.remove employeeId emplIds)
                        Backend.projectDevs <- newProjDev
                    | Auth.ProjectManager ->
                        let newProjPms =
                            Backend.projectPM
                            |> Map.map (fun _ pmIdOpt ->
                                match pmIdOpt with
                                | Some pmId when pmId = employeeId ->
                                    None
                                | x -> x)
                        Backend.projectPM <- newProjPms
                    | _ -> ()

                    // Remove employee from projects
                    let employeeDeptId =
                        Backend.departmentEmployees
                        |> Map.findKey (fun _ emplIds -> Set.contains employeeId emplIds)

                    let newDeptEmployees =
                        Backend.departmentEmployees
                        |> Map.find employeeDeptId
                        |> Set.remove employeeId

                    Backend.departmentEmployees <- Map.add employeeDeptId newDeptEmployees Backend.departmentEmployees

                    // Remove employee from listed employees
                    Backend.employees <- Map.remove employeeId Backend.employees

                    return Some (employeeId)
                | None -> return None
            }
        }