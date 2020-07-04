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
        Id: int
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
        Id: int
        Name: string
        EmployeeIds: int array
        ProjectIds: string
    }

type EmployeeService =
    {
        createEmployee:
            {| Email: string; FullName: string; DepartmentId: int; Role: Auth.Role; Skills: string[] |}
            -> Async<option<int>>
        getEmployees: unit -> Async<Employee[]>
        getEmployee: int -> Async<option<Employee>>
        transferToDepartment: int * int -> Async<option<Employee>>
        changeRole: int * Auth.Role -> Async<option<Employee>>
        updateEmployee:
            {| Id: int; Email: string; FullName: string; Skills: string[] |}
            -> Async<option<unit>>
        deleteEmployee: int -> Async<option<int>>
    }
    interface IRemoteService with
        member this.BasePath = "/api/employees"

type ProjectService =
    {
        createProject: Project -> Async<option<int>>
        getProjects: unit -> Async<Project[]>
        getProject: int -> Async<option<Project>>
        assignToDepartment: int * int -> Async<option<Project>>
        updateProject:
            {| Id: int; ProjectName: string; Description: string; Status: ProjectStatus; DeveloperIds: int array; SkillRequirements: string array |}
            -> Async<option<Project>>
        updateProjectElevated: Project -> Async<option<Project>>
        deleteProject: int -> Async<option<int>>
    }
    interface IRemoteService with
        member this.BasePath = "/api/projects"

type DepartmentService =
    {
        getDepartments: unit -> Async<Department[]>
        getDepartment: unit -> Async<option<Department>>
    }
    interface IRemoteService with
        member this.BasePath = "/api/departments"

