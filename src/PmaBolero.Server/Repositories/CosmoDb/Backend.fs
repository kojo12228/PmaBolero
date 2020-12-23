module PmaBolero.Server.Repositories.CosmoDb.Backend

open System.Collections.Generic
open System.Linq
open System.Net

open Microsoft.Azure.Cosmos
open PmaBolero.Client.Models
open PmaBolero.Server.Models.CosmoData
open PmaBolero.Server.Models.EmployeeDataInternal

open Microsoft.Extensions.Logging
open Microsoft.Extensions.Configuration

type ContainerId<'T> =
    | Id of string
    with
        member this.GetId =
            match this with
            | Id s -> s

let mutable logger: ILogger = null
let mutable config: IConfiguration = null

let private host() = config.["PmaBDB1:host"]
let private auth() = config.["PmaBDB1:auth"]

let private databaseId = "pmabdb1"

let private departmentsContainerId: ContainerId<CosmoDepartment> = Id "departments"
let private employeesContainerId: ContainerId<CosmoEmployee> = Id "employees"
let private projectsContainerId: ContainerId<CosmoProject> = Id "projects"

let private departmentEmployeesContainerId: ContainerId<CosmoDepartmentEmployees> = Id "departmentEmployees"
let private departmentProjectsContainerId: ContainerId<CosmoDepartmentProjects> = Id "departmentProjects"
let private projectDevelopersContainerId: ContainerId<CosmoProjectDevelopers> = Id "projectDevelopers"
let private projectPMsContainerId: ContainerId<CosmoProjectPM> = Id "projectPMs"

let partitionKey = PartitionKey("/id")

let rec private getAllDataAsync<'T> (containerId: ContainerId<'T>): Async<'T list> =
    async {
        logger.LogInformation $"{nameof getAllDataAsync}, containerId {containerId.GetId}"
        use client = new CosmosClient(host(), auth())
        let container = client.GetContainer(databaseId, containerId.GetId)

        let query = "SELECT * FROM c"
        use setIter = container.GetItemQueryIterator<'T>(query)

        logger.LogDebug query

        let arrayList = List<'T>()

        while setIter.HasMoreResults do
            let! response = setIter.ReadNextAsync() |> Async.AwaitTask
            for item in response do
                arrayList.Add(item) |> ignore

        logger.LogDebug (string arrayList)
        logger.LogInformation $"{nameof getAllDataAsync} complete, {arrayList.Count} results"

        return List.ofSeq arrayList
    }

let rec private getItemAsync<'T> (containerId: ContainerId<'T>) (itemId: int): Async<'T option> =
    async {
        logger.LogInformation $"{nameof getItemAsync}, containerId {containerId.GetId}, id {itemId}"
        use client = new CosmosClient(host(), auth())
        let container = client.GetContainer(databaseId, containerId.GetId)

        let query =
            QueryDefinition("SELECT * FROM c WHERE c.id = @id")
                .WithParameter("@id", string itemId)

        logger.LogDebug (string query)
        use setIter = container.GetItemQueryIterator<'T>(query)

        let mutable item: 'T option = None 

        while setIter.HasMoreResults do
            let! response = setIter.ReadNextAsync() |> Async.AwaitTask
            item <- Seq.tryHead response

        logger.LogDebug (string item)
        logger.LogInformation $"{nameof getItemAsync} complete, item found: {item.IsSome}"

        return item
    }

let rec private upsertData<'T> (containerId: ContainerId<'T>) data: Async<Result<HttpStatusCode, HttpStatusCode>> =
    async {
        logger.LogInformation $"{nameof upsertData}, containerId {containerId.GetId}"
        logger.LogDebug $"{data}"

        use client = new CosmosClient(host(), auth())
        let container = client.GetContainer(databaseId, containerId.GetId)

        let! response =
            container.UpsertItemAsync<'T>(data)
            |> Async.AwaitTask

        if int response.StatusCode >= 200 && int response.StatusCode < 300
        then
            logger.LogInformation $"{nameof upsertData} complete, result {int response.StatusCode} {response.StatusCode}"
            return Ok response.StatusCode
        else
            logger.LogError $"{nameof upsertData} failed, result {int response.StatusCode} {response.StatusCode}"
            return Error response.StatusCode
    }

let rec private getNextIdAsync (containerId: ContainerId<_>) =
    async {
        logger.LogInformation $"{nameof getNextIdAsync}, containerId {containerId.GetId}"
        use client = new CosmosClient(host(), auth())
        let container = client.GetContainer(databaseId, containerId.GetId)

        let query = "SELECT c.id FROM c"
        logger.LogInformation query
        use setIter = container.GetItemQueryIterator<{| id: string |}>(query)

        let set = HashSet<int>()

        while setIter.HasMoreResults do
            let! response = setIter.ReadNextAsync() |> Async.AwaitTask
            for item in response do
                set.Add(int item.id) |> ignore

        logger.LogInformation $"{nameof getNextIdAsync} complete, next id: {set.Max() + 1}"

        return set.Max() + 1
    }

let rec private deleteDataAsync<'T> (containerId: ContainerId<'T>) (id: int) =
    async {
        logger.LogInformation $"{nameof deleteDataAsync}, container id {containerId.GetId}, id {id}"

        use client = new CosmosClient(host(), auth())
        let container = client.GetContainer(databaseId, containerId.GetId)

        let! response =
            container.DeleteItemAsync(string id, PartitionKey(string id))
            |> Async.AwaitTask

        if int response.StatusCode >= 200 && int response.StatusCode < 300
        then
            logger.LogInformation $"{nameof deleteDataAsync} complete, result {int response.StatusCode} {response.StatusCode}"
            return Ok response.StatusCode
        else
            logger.LogError $"{nameof deleteDataAsync} failed, result {int response.StatusCode} {response.StatusCode}"
            return Error response.StatusCode
    }

let getEmployeesAsync(): Async<Map<int, Employee>> =
    async {
        let! allEmployees = getAllDataAsync<CosmoEmployee> employeesContainerId

        let mapOfEmpls =
            allEmployees
            |> List.map (toInternalEmployee >> fun e -> e.Id, e)
            |> Map.ofList

        return mapOfEmpls
    }

let getEmployeeAsync emplId: Async<Employee option> =
    async {
        let! emplOpt = getItemAsync<CosmoEmployee> employeesContainerId emplId

        return Option.map toInternalEmployee emplOpt

    }

let upsertEmployeeAsync empl =
    async {
        let cosmoEmpl = fromInternalEmployee empl

        return! upsertData<CosmoEmployee> employeesContainerId cosmoEmpl
    }

let deleteEmployeeAsync emplId =
    deleteDataAsync<CosmoEmployee> employeesContainerId emplId 

let getProjectsAsync(): Async<Map<int, Project>> =
    async {
        let! allProjects = getAllDataAsync<CosmoProject> projectsContainerId

        let mapOfProjs =
            allProjects
            |> List.map (toInternalProject >> fun p -> p.Id, p)
            |> Map.ofList

        return mapOfProjs
    }

let getProjectAsync projId: Async<Project option> =
    async {
        let! projOpt = getItemAsync<CosmoProject> projectsContainerId projId

        return Option.map toInternalProject projOpt
    }

let upsertProjectAsync proj =
    async {
        let cosmoProj = fromInternalProject proj

        return! upsertData<CosmoProject> projectsContainerId cosmoProj
    }

let deleteProjectAsync projId =
    deleteDataAsync<CosmoProject> projectsContainerId projId

let getDepartmentsAsync(): Async<Map<int, Department>> =
    async {
        let! allDepartments = getAllDataAsync<CosmoDepartment> departmentsContainerId

        let mapOfDepartments =
            allDepartments
            |> List.map (toInternalDepartment >> fun d -> d.Id, d)
            |> Map.ofList

        return mapOfDepartments
    }

let getDepartmentAsync deptId: Async<Department option> =
    async {
        let! deptOpt = getItemAsync<CosmoDepartment> departmentsContainerId deptId

        return Option.map toInternalDepartment deptOpt
    }

let upsertDepartmentAsync dept =
    async {
        let cosmoDept = fromInternalDepartment dept

        return! upsertData<CosmoDepartment> departmentsContainerId cosmoDept
    }

let deleteDepartmentAsync deptId =
    deleteDataAsync<CosmoDepartment> departmentsContainerId deptId

let getNextEmployeeIdAsync() = getNextIdAsync employeesContainerId

let getNextProjectIdAsync() = getNextIdAsync projectsContainerId

let getNextDepartmentIdAsync() = getNextIdAsync departmentsContainerId

let getDepartmentEmployeesAsync(): Async<Map<int, int Set>> =
    async {
        let! departmentEmployees = getAllDataAsync<CosmoDepartmentEmployees> departmentEmployeesContainerId

        let map =
            departmentEmployees
            |> List.map (fun x -> int x.id, Set.ofArray x.emplIds)
            |> Map.ofList

        return map
    }

let getDepartmentEmployeesItemAsync deptId: Async<int Set option> =
    async {
        let! deptEmplsOpt = getItemAsync<CosmoDepartmentEmployees> departmentEmployeesContainerId deptId

        return deptEmplsOpt |> Option.map ((fun x -> x.emplIds ) >> Set.ofArray)
    }

let upsertDepartmentEmployeesAsync deptId emplIds =
    async {
        let cosmoDeptEmpls: CosmoDepartmentEmployees =
            {
                id = string deptId
                emplIds = Array.ofSeq emplIds
            }

        return! upsertData<CosmoDepartmentEmployees> departmentEmployeesContainerId cosmoDeptEmpls
    }

let deleteDepartmentEmployeeMappingAsync deptId =
    deleteDataAsync<CosmoDepartmentEmployees> departmentEmployeesContainerId deptId

let getDepartmentProjectsAsync(): Async<Map<int, int Set>> =
    async {
        let! departmentProjects = getAllDataAsync<CosmoDepartmentProjects> departmentProjectsContainerId

        let map =
            departmentProjects
            |> List.map (fun x -> int x.id, Set.ofArray x.projIds)
            |> Map.ofList

        return map
    }

let getDepartmentProjectsItemAsync deptId: Async<int Set option> =
    async {
        let! deptProjsOpt = getItemAsync<CosmoDepartmentProjects> departmentProjectsContainerId deptId

        return deptProjsOpt |> Option.map ((fun x -> x.projIds ) >> Set.ofArray)
    }

let upsertDepartmentProjectsAsync deptId projIds =
    async {
        let cosmoDeptProjs: CosmoDepartmentProjects =
            {
                id = string deptId
                projIds = Array.ofSeq projIds
            }
        
        return! upsertData<CosmoDepartmentProjects> departmentProjectsContainerId cosmoDeptProjs
    }

let deleteDepartmentProjectMappingAsync deptId =
    deleteDataAsync<CosmoDepartmentProjects> departmentProjectsContainerId deptId

let getProjectDevelopersAsync(): Async<Map<int, int Set>> =
    async {
        let! projectDevelopers = getAllDataAsync<CosmoProjectDevelopers> projectDevelopersContainerId

        let map =
            projectDevelopers
            |> List.map (fun x -> int x.id, Set.ofArray x.emplIds)
            |> Map.ofList

        return map
    }

let getProjectDevelopersItemAsync projId: Async<int Set option> =
    async {
        let! projDevsOpt = getItemAsync<CosmoProjectDevelopers> projectDevelopersContainerId projId

        return projDevsOpt |> Option.map ((fun x -> x.emplIds) >> Set.ofArray)
    }

let upsertProjectDevelopersAsync projId emplIds =
    async {
        let cosmoProjDevs: CosmoProjectDevelopers =
            {
                id = string projId
                emplIds = Array.ofSeq emplIds
            }

        return! upsertData<CosmoProjectDevelopers> projectDevelopersContainerId cosmoProjDevs
    }

let deleteProjectDeveloperMappingAsync projId =
    deleteDataAsync<CosmoProjectDevelopers> projectDevelopersContainerId projId

let getProjectPMAsync(): Async<Map<int, int option>> =
    async {
        let! projectPMs = getAllDataAsync<CosmoProjectPM> projectPMsContainerId

        let map =
            projectPMs
            |> List.map (fun x -> int x.id, Option.ofNullable x.emplId)
            |> Map.ofList

        return map
    }

let getProjectPMItemAsync projId: Async<int option option> =
    async {
        let! projPMOpt = getItemAsync<CosmoProjectPM> projectPMsContainerId projId

        return projPMOpt |> Option.map (fun x -> Option.ofNullable x.emplId)
    }

let upsertProjectPMAsync projId emplId =
    async {
        let projPm: CosmoProjectPM =
            {
                id = string projId
                emplId = Option.toNullable emplId
            }

        return! upsertData<CosmoProjectPM> projectPMsContainerId projPm
    }

let deleteProjectPMMappingAsync projId =
    deleteDataAsync<CosmoProjectPM> projectPMsContainerId projId

let toClientEmployeeAsync (employee: Employee): Async<EmployeeData.Employee> =
    async {
        let! departmentEmployees = getDepartmentEmployeesAsync()
        let! projectDevs = getProjectDevelopersAsync()
        let! projectPM = getProjectPMAsync()
        
        let! department =
            departmentEmployees
            |> Map.toList
            |> List.find (fun (_, empls) -> Set.contains employee.Id empls)
            |> (fun (deptId, _) -> 
                async {
                    let! deptOpt = getDepartmentAsync deptId
                    return deptId, deptOpt.Value.Name
                })

        let! projects =
            match employee.Role with
            | Auth.Developer ->
                projectDevs
                |> Map.toArray
                |> Array.filter (fun (_, empls) -> Set.contains employee.Id empls)
                |> Array.map (fun (projId, _) -> 
                    async {
                        let! proj = getProjectAsync projId
                        return projId, proj.Value.ProjectName
                    })
                |> Async.Parallel
            | Auth.ProjectManager ->
                projectPM
                |> Map.toArray
                |> Array.filter (fun (_, pm) ->
                    match pm with
                    | None -> false
                    | Some pmId -> pmId = employee.Id)
                |> Array.map (fun (projId, _) -> 
                    async {
                        let! proj = getProjectAsync projId
                        return projId, proj.Value.ProjectName
                    })
                |> Async.Parallel
            | Auth.Admin ->
                async { return [||] }

        return
            {
                Id = employee.Id
                Email = employee.Email
                FullName = employee.FullName
                DepartmentID = department
                Role = employee.Role
                ProjectIds = projects
                Skills = employee.Skills
            }
    }

let toClientProjectAsync (project: Project): Async<EmployeeData.Project> =
    async {
        let! departmentProjects = getDepartmentProjectsAsync()

        let! department =
            departmentProjects
            |> Map.findKey (fun _ projIds -> Set.contains project.Id projIds)
            |> (fun deptId ->
                async {
                    let! deptOpt = getDepartmentAsync deptId
                    return deptId, deptOpt.Value.Name
                })

        let! projDevs = getProjectDevelopersItemAsync project.Id

        let! devs =
            projDevs.Value // TODO Correctly handle case of null
            |> Set.toArray
            |> Array.map (fun devId ->
                async {
                    let! emplOpt = getEmployeeAsync devId
                    return devId, emplOpt.Value.FullName
                })
            |> Async.Parallel

        let! projPM = getProjectPMItemAsync project.Id

        let! pm =
            projPM.Value // TODO Correctly handle case of null
            |> (fun pmIdOpt ->
                async {
                    match pmIdOpt with
                    | None -> return None
                    | Some pmId ->
                        let! emplOpt = getEmployeeAsync pmId
                        return Some (pmId, emplOpt.Value.FullName)
                })

        return
            {
                Id = project.Id
                Name = project.ProjectName
                DepartmentId = department
                Description = project.Description
                Status = project.Status
                ProjectManagerId = pm
                DeveloperIds = devs
                SkillRequirements = project.SkillRequirements
            }
    }

let toClientDepartmentAsync (dept: Department): Async<EmployeeData.Department> =
    async {
        let! departmentEmployees = getDepartmentEmployeesItemAsync dept.Id

        let! deptEmployees =
            departmentEmployees.Value // TODO Correctly handle case of null
            |> Set.toArray
            |> Array.map (fun emplId ->
                async {
                    let! emplOpt = getEmployeeAsync emplId
                    return emplId, emplOpt.Value.FullName
                })
            |> Async.Parallel

        let! departmentProjects = getDepartmentProjectsItemAsync dept.Id
        
        let! deptProjects =
            departmentProjects.Value // TODO Correctly handle case of null
            |> Set.toArray
            |> Array.map (fun projId ->
                async {
                    let! projOpt = getProjectAsync projId
                    return projId, projOpt.Value.ProjectName
                })
            |> Async.Parallel

        return
            {
                Id = dept.Id
                Name = dept.Name
                Employees = deptEmployees
                Projects = deptProjects
            }
    }