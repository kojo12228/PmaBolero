module PmaBolero.Client.Models.EmployeeData

open Bolero.Remoting

open PmaBolero.Shared.Models

// fsharplint:disable RecordFieldNames

type EmployeeService =
    { createEmployee: {| Email: string
                         FullName: string
                         DepartmentId: int
                         Role: Role
                         Skills: string [] |}
        -> Async<option<int>>
      getEmployees: unit -> Async<Employee []>
      getProjectManagers: unit -> Async<(int * string) []>
      getDevelopers: unit -> Async<(int * string) []>
      getEmployee: int -> Async<option<Employee>>
      transferToDepartment: int * int -> Async<option<Employee>>
      changeRole: int * Role -> Async<option<Employee>>
      updateEmployee: {| Id: int
                         FullName: string
                         Skills: string [] |}
          -> Async<option<Employee>>
      deleteEmployee: int -> Async<option<int>> }
    interface IRemoteService with
        member this.BasePath = "/api/employees"

type ProjectService =
    { createProject: {| ProjectName: string
                        DepartmentId: int
                        Description: string
                        ProjectManagerId: int option
                        DeveloperIds: int array
                        SkillRequirements: string array |}
        -> Async<option<int>>
      getProjects: unit -> Async<Project []>
      getProject: int -> Async<option<Project>>
      assignToDepartment: int * int -> Async<option<Project>>
      updateProject: {| Id: int
                        ProjectName: string
                        Description: string
                        Status: ProjectStatus
                        DeveloperIds: int array
                        SkillRequirements: string array |}
          -> Async<option<Project>>
      updateProjectElevated: {| Id: int
                                ProjectName: string
                                Description: string
                                Status: ProjectStatus
                                ProjectManagerId: int option
                                DeveloperIds: int array
                                SkillRequirements: string array |}
          -> Async<option<Project>>
      deleteProject: int -> Async<option<int>> }
    interface IRemoteService with
        member this.BasePath = "/api/projects"

type DepartmentService =
    { getDepartments: unit -> Async<Department []>
      getDepartmentIds: unit -> Async<(int * string) []>
      getDepartment: int -> Async<option<Department>> }
    interface IRemoteService with
        member this.BasePath = "/api/departments"
