namespace PmaBolero.Server

open System
open System.IO
open Microsoft.AspNetCore.Hosting
open Bolero
open Bolero.Remoting
open Bolero.Remoting.Server
open PmaBolero

open PmaBolero.Shared.Models
open PmaBolero.Server.Models
open PmaBolero.Client.Models
open PmaBolero.Server.Repositories.Mock
open System.Security.Claims

type AuthService(ctx: IRemoteContext, env: IWebHostEnvironment) =
    inherit RemoteHandler<Auth.AuthService>()

    let mutable users = UsersRepository.users

    override this.Handler =
        { signIn =
            fun (username, password) ->
                async {
                    let matchedUser =
                        users
                        |> Array.tryFind (fun u -> u.Username = username && u.Password = password)

                    match matchedUser with
                    | Some user ->
                        let roleStr = Map.find user.Role Auth.roleToStringMap
                        let claims = [ Claim(ClaimTypes.Role, roleStr) ]
                        do! ctx.HttpContext.AsyncSignIn(user.Username, TimeSpan.FromDays(365.), claims = claims)
                        return Some(user.Username, user.Role)
                    | None -> return None
                }

          getUser =
              ctx.Authorize
              <| fun () ->
                  async {
                      let user =
                          users
                          |> Array.find (fun u -> u.Username = ctx.HttpContext.User.Identity.Name)

                      return (user.Username, user.Role)
                  }

          getUsername =
              ctx.Authorize
              <| fun () -> async { return ctx.HttpContext.User.Identity.Name }

          getRole =
              ctx.Authorize
              <| fun () ->
                  async {
                      let user =
                          users
                          |> Array.find (fun u -> u.Username = ctx.HttpContext.User.Identity.Name)

                      return user.Role
                  }

          addUser =
              fun (username, password) ->
                  async {
                      if users
                         |> Array.exists (fun u -> u.Username = username) then
                          return false
                      else
                          let newUser =
                              [| { Username = username
                                   Password = password
                                   Role = Developer } |]

                          users <- Array.append users newUser

                          return true
                  }

          signOut = fun () -> async { return! ctx.HttpContext.AsyncSignOut() } }
