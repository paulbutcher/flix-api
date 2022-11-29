use Html.html
use Api.{Api, parse}
use Api.Api.Api

def generateNs(api: Api, mdToHtml: String -> String, ns: String): Result[Bool, String] \ IO =
    File.write("site/${nsToUri(ns)}.html",
        ns |> nspace(api, mdToHtml) |> template |> html)

def generate(): Result[Unit, String] \ IO =
    use Result.flatMap;
    let mdToHtml = CommonMark.mkRenderer();
    let* api = parse("api.json");
    let* _ = File.mkdir("site");
    match api {
        case Api(_, nss, _, _, _, _) =>
            nss |> Result.traverseX(generateNs(api, mdToHtml))
    }

def main(): Unit \ IO =
    match generate() {
        case Ok(_) => println("Site generated in: site")
        case Err(message) => println(message)
    }
