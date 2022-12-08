use Html.{html, Element}
use Api.{Api, parse}
use Index.index

def writeFile(fname: String, content: Chain[Element]): Result[Bool, String] \ IO =
    content |> html |> File.write(fname)

def generate(): Result[Bool, String] \ IO =
    use Result.flatMap;
    let mdToHtml = CommonMark.mkRenderer();
    let* api = parse("api.json");
    let* _ = File.mkdir("site");
    let* _ = File.copyOver(src = "resources/site.css", "site/site.css");
    let* _ = File.copyOver(src = "resources/sidebar.js", "site/sidebar.js");
    index(api, mdToHtml) |> writeFile("site/index.html")

def main(): Unit \ IO = match generate() {
    case Ok(_) => println("Site generated in: site")
    case Err(message) => println(message)
}
