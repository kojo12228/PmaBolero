module PmaBolero.Server.Repositories.Mock.Backend

open PmaBolero.Server.Models.EmployeeDataInternal
open PmaBolero.Client.Models

let employees: Employee[] =
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

let projects: Project[] =
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

let departments: Department[] =
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

let departmentEmployees: Map<int, int Set> =
    [|
        0, [ 0 ] |> Set.ofList
        1, [ 1; 2; 3 ] |> Set.ofList
        2, [] |> Set.ofList
        3, [] |> Set.ofList
        4, [] |> Set.ofList
    |] |> Map.ofArray

let departmentProjects: Map<int, int Set> =
    [|
        0, [] |> Set.ofList
        1, [ 0; 1 ] |> Set.ofList
        2, [] |> Set.ofList
        3, [] |> Set.ofList
        4, [] |> Set.ofList
    |] |> Map.ofArray

let projectDevs: Map<int, int Set> =
    [|
        0, [ 1 ] |> Set.ofList
        1, [ 1 ] |> Set.ofList
    |] |> Map.ofArray

let projectPM: Map<int, int> =
    [|
        0, 2
        1, 3
    |] |> Map.ofArray

