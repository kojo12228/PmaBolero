namespace PmaBolero.Server

open System
open System.IO
open Microsoft.AspNetCore.Hosting
open Bolero
open Bolero.Remoting
open Bolero.Remoting.Server
open PmaBolero

open PmaBolero.Server.Models
open PmaBolero.Client.Models
open PmaBolero.Server.Repositories.Mock

type AuthService(ctx: IRemoteContext, env: IWebHostEnvironment) =
    inherit RemoteHandler<Auth.AuthService>()

    let mutable users = UsersRepository.users

    override this.Handler =
        {
            signIn = fun (username, password) -> async {
                if
                    users
                    |> Array.exists (fun u ->
                            u.Username = username &&
                            u.Password = password
                        )
                then
                    do! ctx.HttpContext.AsyncSignIn(username, TimeSpan.FromDays(365.))
                    return Some username
                else
                    return None
            }

            getUsername = ctx.Authorize <| fun () -> async {
                return Some ctx.HttpContext.User.Identity.Name
            }

            getRole = ctx.Authorize <| fun () -> async {
                let user =
                    users
                    |> Array.find (fun u -> u.Username = ctx.HttpContext.User.Identity.Name)
                return Some user.Role
            }

            addUser = fun (username, password) -> async {
                if users |> Array.exists (fun u -> u.Username = username)
                then return None
                else
                    let newUser =
                        [| {
                            Username = username
                            Password = password
                            Role = Auth.Role.Developer
                        } |]
                    users <- Array.append users newUser

                    return Some ()
            }

            signOut = fun () -> async {
                return! ctx.HttpContext.AsyncSignOut()
            }
        }