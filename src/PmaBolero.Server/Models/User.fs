namespace PmaBolero.Server.Models

open PmaBolero.Shared.Models

type User =
    {
        Username: string
        Password: string
        Role: Role
    }