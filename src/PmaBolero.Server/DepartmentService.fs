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

            getDepartment = ctx.Authorize <| fun deptId -> async {
                return
                    Backend.departments
                    |> Map.tryFind deptId
                    |> Option.map Backend.toClientDepartment
            }
        }