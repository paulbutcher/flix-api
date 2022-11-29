use Html.html
use Api.{Api, parse}
use Api.Api.Api

def generateNs(api: Api, ns: String): Result[Bool, String] \ IO =
    File.write("site/${ns}.html", ns |> mkNamespace(api) |> template |> html)

def generate(): Result[Unit, String] \ IO =
    use Result.flatMap;
    let* api = parse("api.json");
    let* _ = File.mkdir("site");
    match api {
        case Api(_, namespaces, _, _, _, _) =>
            namespaces |> Result.traverseX(generateNs(api))
    }

def main(): Unit \ IO =
    match generate() {
        case Ok(_) => println("Site generated in: site")
        case Err(message) => println(message)
    }
