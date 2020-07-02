module PmaBolero.Client.Models.EmployeeData

open Bolero.Remoting
open Bolero.Remoting.Client

type Employee =
    {
        Id: int
        Email: string
        FullName: string
        DepartmentID: int
        Role: Auth.Role
        ProjectIds: int array
        Skills: string array
    }

type ProjectStatus =
    | Pending
    | Active
    | Complete

type Project =
    {
        ProjectId: int
        ProjectName: string
        DepartmentId: int
        Description: string
        Status: ProjectStatus
        ProjectManagerId: int
        DeveloperIds: int array
        SkillRequirements: string array
    }

type Department =
    {
        DepartmentId: int
        EmployeeIds: int array
        ProjectIds: string
    }