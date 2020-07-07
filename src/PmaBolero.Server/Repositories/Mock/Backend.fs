module PmaBolero.Server.Repositories.Mock.Backend

open PmaBolero.Server.Models.EmployeeDataInternal
open PmaBolero.Client.Models

let mutable employees: Map<int, Employee> =
    [|
        {
            Id = 0
            Email = "adam.admin@pma-bolero.co.uk"
            FullName = "Adam Admin"
            Role = Auth.Admin
            Skills = [||]
        }
        {
            Id = 1
            Email = "chris.lyons@pma-bolero.co.uk"
            FullName = "Chris Lyons"
            Role = Auth.Developer
            Skills = [| "C#"; "SQL" |]
        }
        {
            Id = 2
            Email = "helen.apex@pma-bolero.co.uk"
            FullName = "Helen Apex"
            Role = Auth.ProjectManager
            Skills = [| "C#"; "Java" |]
        }
        {
            Id = 3
            Email = "laura.dawkins@pma-bolero.co.uk"
            FullName = "Laura Dawkins"
            Role = Auth.ProjectManager
            Skills = [| "C#" |]
        }
    |]
    |> Array.zip [| 0 .. 3 |]
    |> Map.ofArray

let mutable projects: Map<int, Project> =
    [|
        {
            Id = 0
            ProjectName = "Migration To Azure"
            Description = "Planning and implementation of move from private cloud to Azure for streaming"
            Status = EmployeeData.Complete
            SkillRequirements = [| "C#" |]
        }
        {
            Id = 1
            ProjectName = "Streaming APAC Launch"
            Description = ""
            Status = EmployeeData.Pending
            SkillRequirements = [| "C#" |]
        }
    |]
    |> Array.zip [| 0; 1 |]
    |> Map.ofArray

let private getNextId dataMap =
    dataMap
    |> Map.toSeq
    |> Seq.maxBy (fst)
    |> fst
    |> ((+) 1)

let getNextEmployeeId() =
    getNextId employees

let getNextProjectId() =
    getNextId projects

let mutable departments: Map<int, Department> =
    [|
        {
            Id = 0
            Name = "Administration"
        }
        {
            Id = 1
            Name = "Streaming"
        }
        {
            Id = 2
            Name = "Data Handling"
        }
        {
            Id = 3
            Name = "Experimental"
        }
        {
            Id = 4
            Name = "Storage"
        }
    |]
    |> Array.zip [| 0 .. 4 |]
    |> Map.ofArray

let mutable departmentEmployees: Map<int, int Set> =
    [|
        0, [ 0 ] |> Set.ofList
        1, [ 1; 2; 3 ] |> Set.ofList
        2, [] |> Set.ofList
        3, [] |> Set.ofList
        4, [] |> Set.ofList
    |] |> Map.ofArray

let mutable departmentProjects: Map<int, int Set> =
    [|
        0, [] |> Set.ofList
        1, [ 0; 1 ] |> Set.ofList
        2, [] |> Set.ofList
        3, [] |> Set.ofList
        4, [] |> Set.ofList
    |] |> Map.ofArray

let mutable projectDevs: Map<int, int Set> =
    [|
        0, [ 1 ] |> Set.ofList
        1, [ 1 ] |> Set.ofList
    |] |> Map.ofArray

let mutable projectPM: Map<int, int option> =
    [|
        0, Some 2
        1, Some 3
    |] |> Map.ofArray

let toClientEmployee (employee: Employee): EmployeeData.Employee =
    let department =
        departmentEmployees
        |> Map.toList
        |> List.find (fun (dept, empls) -> Set.contains employee.Id empls)
        |> fst

    let projects =
        match employee.Role with
        | Auth.Developer ->
            projectDevs
            |> Map.toArray
            |> Array.filter (fun (proj, empls) -> Set.contains employee.Id empls)
            |> Array.map fst
        | Auth.ProjectManager ->
            projectPM
            |> Map.toArray
            |> Array.filter (fun (proj, pm) -> employee.Id = pm.Value)
            |> Array.map fst
        | Auth.Admin ->
            [||]

    {
        Id = employee.Id
        Email = employee.Email
        FullName = employee.FullName
        DepartmentID = department
        Role = employee.Role
        ProjectIds = projects
        Skills = employee.Skills
    }

let toClientProject (project: Project): EmployeeData.Project =
    let department =
        departmentProjects
        |> Map.findKey (fun _ projIds -> Set.contains project.Id projIds)

    let devs =
        projectDevs
        |> Map.find project.Id
        |> Set.toArray

    let pm = Map.find project.Id projectPM

    {
        Id = project.Id
        ProjectName = project.ProjectName
        DepartmentId = department
        Description = project.Description
        Status = project.Status
        ProjectManagerId = pm
        DeveloperIds = devs
        SkillRequirements = project.SkillRequirements
    }

let toClientDepartment (dept: Department): EmployeeData.Department =
    let deptEmployees =
        departmentEmployees
        |> Map.find dept.Id
        |> Set.toArray

    let deptProjects =
        departmentProjects
        |> Map.find dept.Id
        |> Set.toArray

    {
        Id = dept.Id
        Name = dept.Name
        EmployeeIds = deptEmployees
        ProjectIds = deptProjects
    }
