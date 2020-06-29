namespace PmaBolero.Server.Models

open PmaBolero.Client.Models.Auth

type User =
    {
        Username: string
        Password: string
        Role: Role
    }