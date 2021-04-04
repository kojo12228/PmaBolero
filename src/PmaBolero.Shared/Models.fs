module PmaBolero.Shared.Models

type Role =
    | Admin
    | ProjectManager
    | Developer
    override this.ToString() =
        match this with
        | Admin -> "Admin"
        | ProjectManager -> "Project Manager"
        | Developer -> "Developer"

type ProjectStatus =
    | Pending
    | Active
    | Complete