module PmaBolero.Client.Models.Auth

open Bolero.Remoting
open Bolero.Remoting.Client

type Role =
    | Admin
    | ProjectManager
    | Developer

type AuthService =
    {
        signIn: string * string -> Async<option<string * Role>>
        getUsername: unit -> Async<string>
        getRole: unit -> Async<Role>
        getUser: unit -> Async<string * Role>
        addUser: string * string -> Async<option<unit>>
        signOut: unit -> Async<unit>
    }
    interface IRemoteService with
        member this.BasePath = "/auth"