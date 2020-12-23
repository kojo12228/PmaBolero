namespace PmaBolero.Server

open System
open Microsoft.AspNetCore.Hosting
open Bolero.Remoting.Server

open PmaBolero.Server.Models.EmployeeDataInternal
open PmaBolero.Client.Models
open PmaBolero.Server.Repositories.CosmoDb.Backend
open System.Security.Claims
open Microsoft.AspNetCore.Authorization

type DepartmentService(ctx: IRemoteContext, env: IWebHostEnvironment) =
    inherit RemoteHandler<EmployeeData.DepartmentService>()

    override this.Handler =
        {
            getDepartments = ctx.Authorize <| fun () -> async {
                let! departments = getDepartmentsAsync()
                return!
                    departments
                    |> Map.toArray
                    |> Array.map (snd >> toClientDepartmentAsync)
                    |> Async.Parallel
            }

            getDepartmentIds = ctx.Authorize <| fun () -> async {
                let! departments = getDepartmentsAsync()
                return
                    departments
                    |> Map.toArray
                    |> Array.map (snd >> (fun dept -> dept.Id, dept.Name))
            }

            getDepartment = ctx.Authorize <| fun deptId -> async {
                let! deptOpt = getDepartmentAsync deptId
                match deptOpt with
                | None -> return None
                | Some dept ->
                    let! clientDept = toClientDepartmentAsync dept
                    return Some clientDept
            }
        }