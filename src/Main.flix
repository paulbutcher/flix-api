use Html.{html, Element}
use Api.{Api, parse}
use Api.Api.Api
use Namespace.nspace
use Template.template
use Index.index
use Sidebar.sidebar
use HtmlUtils.nsToUri

def writeFile(fname: String, content: Chain[Element]): Result[Bool, String] \ IO =
    content |> template |> html |> File.write(fname)

def generateNs(api: Api, mdToHtml: String -> String, ns: String): Result[Bool, String] \ IO =
    ns |> nspace(api, mdToHtml) |> writeFile("site/${nsToUri(ns)}")

def generate(): Result[Unit, String] \ IO =
    use Result.flatMap;
    let mdToHtml = CommonMark.mkRenderer();
    let* api = parse("api.json");
    let* _ = File.mkdir("site");
    let* _ = File.copyOver(src = "resources/site.css", "site/site.css");
    let* _ = index() |> writeFile("site/index.html");
    let* _ = sidebar(api) |> writeFile("site/sidebar.html");
    match api {
        case Api(_, nss, _, _, _, _) =>
            nss |> Result.traverseX(generateNs(api, mdToHtml))
    }

def main(): Unit \ IO =
    match generate() {
        case Ok(_) => println("Site generated in: site")
        case Err(message) => println(message)
    }
