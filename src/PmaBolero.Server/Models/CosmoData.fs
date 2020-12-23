module PmaBolero.Server.Models.CosmoData

open PmaBolero.Client.Models
open PmaBolero.Server.Models.EmployeeDataInternal

open System

type CosmoEmployee =
    {
        id: string // int stored as string
        email: string
        fullname: string
        role: string
        skills: string[]
    }

type CosmoProject =
    {
        id: string // int stored as string
        projectName: string
        description: string
        status: string
        skillRequirements: string[]
    }

type CosmoDepartment = 
    {
        id: string // int stored as strong
        name: string
    }

type CosmoDepartmentEmployees =
    {
        id: string // int stored as string
        emplIds: int []
    }

type CosmoDepartmentProjects =
    {
        id: string // int stored as string
        projIds: int []
    }

type CosmoProjectDevelopers =
    {
        id: string // int stored as string
        emplIds: int []
    }

type CosmoProjectPM =
    {
        id: string
        emplId: Nullable<int>
    }

let toInternalEmployee (empl: CosmoEmployee) : Employee =
    {
        Id = int empl.id
        Email = empl.email
        FullName = empl.fullname
        Role =
            match empl.role with
            | "Admin" -> Auth.Role.Admin
            | "Developer" -> Auth.Role.Developer
            | "ProjectManager" -> Auth.Role.ProjectManager
            | _ -> failwith $"unexpected data cosmo to internal employee, id {empl.id}"
        Skills = empl.skills
    }

let fromInternalEmployee (empl: Employee) : CosmoEmployee =
    {
        id = string empl.Id
        email = empl.Email
        fullname = empl.FullName
        role =
            match empl.Role with
            | Auth.Role.Admin -> "Admin"
            | Auth.Role.Developer -> "Developer"
            | Auth.Role.ProjectManager -> "ProjectManager"
        skills = empl.Skills
    }

let toInternalProject (proj: CosmoProject) : Project =
    {
        Id = int proj.id
        ProjectName = proj.projectName
        Description = proj.description
        Status =
            match proj.status with
            | "pending" -> EmployeeData.ProjectStatus.Pending
            | "active" -> EmployeeData.ProjectStatus.Active
            | "complete" -> EmployeeData.ProjectStatus.Complete
            | _ -> failwith $"unexpected data cosmo to internal project, id {proj.id}"
        SkillRequirements = proj.skillRequirements
    }

let fromInternalProject (proj: Project) : CosmoProject =
    {
        id = string proj.Id
        projectName = proj.ProjectName
        description = proj.Description
        status =
            match proj.Status with
            | EmployeeData.ProjectStatus.Pending -> "pending" 
            | EmployeeData.ProjectStatus.Active -> "active"
            | EmployeeData.ProjectStatus.Complete -> "complete"
        skillRequirements = proj.SkillRequirements
    }

let toInternalDepartment (dept: CosmoDepartment) : Department =
    {
        Id = int dept.id
        Name = dept.name
    }

let fromInternalDepartment (dept: Department) : CosmoDepartment =
    {
        id = string dept.Id
        name = dept.Name
    }