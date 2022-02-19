module PmaBolero.Server.Models.EmployeeDataInternal

open PmaBolero.Shared.Models

open PmaBolero.Client.Models

type Employee =
    { Id: int
      Email: string
      FullName: string
      Role: Role
      Skills: string [] }

type Project =
    { Id: int
      ProjectName: string
      Description: string
      Status: ProjectStatus
      SkillRequirements: string [] }

type Department = { Id: int; Name: string }
