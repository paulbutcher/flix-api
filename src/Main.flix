use Api.parse

def generate(): Result[Bool, String] \ IO =
    use Result.flatMap;
    let* api = parse("api.json");
    let* _ = File.mkdir("site");
    File.write("site/index.html", index(api))

def main(): Unit \ IO =
    match generate() {
        case Ok(_) => println("Site generated in: site")
        case Err(message) => println(message)
    }
