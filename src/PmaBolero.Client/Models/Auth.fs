module PmaBolero.Client.Models.Auth

open Bolero.Remoting
open Bolero.Remoting.Client

type Role =
    | Admin
    | ProjectManager
    | Developer

type AuthService =
    {
        signIn: string * string -> Async<option<string>>
        getUsername: unit -> Async<option<string>>
        getRole: unit -> Async<option<Role>>
        signOut: unit -> Async<unit>
    }
    interface IRemoteService with
        member this.BasePath = "/auth"