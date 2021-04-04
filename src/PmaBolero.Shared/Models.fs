module PmaBolero.Shared.Models

// DUs
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

// Records
type Employee =
    {
        Id: int
        Email: string
        FullName: string
        DepartmentID: (int * string)
        Role: Role
        ProjectIds: (int * string) array
        Skills: string array
    }

type Project =
    {
        Id: int
        Name: string
        DepartmentId: (int * string)
        Description: string
        Status: ProjectStatus
        ProjectManagerId: (int * string) option
        DeveloperIds: (int * string) array
        SkillRequirements: string array
    }

type Department =
    {
        Id: int
        Name: string
        Employees: (int * string) array
        Projects: (int * string) array
    }