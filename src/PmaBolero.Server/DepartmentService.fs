namespace PmaBolero.Server

open System
open Microsoft.AspNetCore.Hosting
open Bolero.Remoting.Server

open PmaBolero.Server.Models.EmployeeDataInternal
open PmaBolero.Client.Models
open PmaBolero.Server.Repositories.Mock
open System.Security.Claims
open Microsoft.AspNetCore.Authorization

type DepartmentService(ctx: IRemoteContext, env: IWebHostEnvironment) =
    inherit RemoteHandler<EmployeeData.DepartmentService>()

    override this.Handler =
        {
            getDepartments = ctx.Authorize <| fun () -> async {
                return
                    Backend.departments
                    |> Map.toArray
                    |> Array.map (snd >> Backend.toClientDepartment)
            }

            getDepartmentsVis = ctx.Authorize <| fun () -> async {
                let depts =
                    Backend.departments
                    |> Map.toArray
                    |> Array.map (snd >> Backend.toClientDepartment)

                let toVisDepts (dept: EmployeeData.Department): EmployeeData.DepartmentVis =
                    let projs =
                        dept.ProjectIds
                        |> Array.map 
                           ((fun projId -> Map.find projId Backend.projects) >>
                            (fun proj -> proj.Id, proj.ProjectName))
                    let empls =
                        dept.EmployeeIds
                        |> Array.map
                           ((fun emplId -> Map.find emplId Backend.employees) >>
                            (fun empl -> empl.Id, empl.FullName))

                    {
                        Id = dept.Id
                        Name = dept.Name
                        Projects = projs
                        Employees = empls
                    }

                return Array.map toVisDepts depts
            }

            getDepartment = ctx.Authorize <| fun deptId -> async {
                return
                    Backend.departments
                    |> Map.tryFind deptId
                    |> Option.map Backend.toClientDepartment
            }
        }