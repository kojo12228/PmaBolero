module PmaBolero.Server.Models.EmployeeDataInternal

open PmaBolero.Client.Models

type Employee =
    {
        Id: int
        Email: string
        FullName: string
        Role: Auth.Role
        Skills: string[]
    }

type Project =
    {
        Id: int
        ProjectName: string
        Description: string
        Status: EmployeeData.ProjectStatus
        SkillRequirements: string[]
    }

type Department = { Id: int; Name: string }