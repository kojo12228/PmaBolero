module PmaBolero.Client.Models.EmployeeData

open Bolero.Remoting

open PmaBolero.Shared.Models

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

// fsharplint:disable RecordFieldNames

type EmployeeService =
    {
        createEmployee:
            {| Email: string; FullName: string; DepartmentId: int; Role: Role; Skills: string[] |}
            -> Async<option<int>>
        getEmployees: unit -> Async<Employee[]>
        getProjectManagers: unit -> Async<(int * string) []>
        getDevelopers: unit -> Async<(int * string) []>
        getEmployee: int -> Async<option<Employee>>
        transferToDepartment: int * int -> Async<option<Employee>>
        changeRole: int * Role -> Async<option<Employee>>
        updateEmployee:
            {| Id: int; FullName: string; Skills: string[] |}
            -> Async<option<Employee>>
        deleteEmployee: int -> Async<option<int>>
    }
    interface IRemoteService with
        member this.BasePath = "/api/employees"

type ProjectService =
    {
        createProject:
            {| ProjectName: string; DepartmentId: int; Description: string; ProjectManagerId: int option; DeveloperIds: int array; SkillRequirements: string array |}
            -> Async<option<int>>
        getProjects: unit -> Async<Project[]>
        getProject: int -> Async<option<Project>>
        assignToDepartment: int * int -> Async<option<Project>>
        updateProject:
            {| Id: int; ProjectName: string; Description: string; Status: ProjectStatus; DeveloperIds: int array; SkillRequirements: string array |}
            -> Async<option<Project>>
        updateProjectElevated:
            {| Id: int; ProjectName: string; Description: string; Status: ProjectStatus; ProjectManagerId: int option; DeveloperIds: int array; SkillRequirements: string array |}
            -> Async<option<Project>>
        deleteProject: int -> Async<option<int>>
    }
    interface IRemoteService with
        member this.BasePath = "/api/projects"

type DepartmentService =
    {
        getDepartments: unit -> Async<Department[]>
        getDepartmentIds: unit -> Async<(int * string)[]>
        getDepartment: int -> Async<option<Department>>
    }
    interface IRemoteService with
        member this.BasePath = "/api/departments"

