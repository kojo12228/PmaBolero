namespace PmaBolero.Server.Repositories.Mock

open PmaBolero.Shared.Models
open PmaBolero.Server.Models

module UsersRepository =
    let users =
        [| { Username = "adam.admin@pma-bolero.co.uk"
             Password = "ngModel10"
             Role = Admin }
           { Username = "laura.dawkins@pma-bolero.co.uk"
             Password = "am2pm"
             Role = ProjectManager }
           { Username = "chris.lyons@pma-bolero.co.uk"
             Password = "9verParis"
             Role = Developer } |]
