namespace Api {
    use Json.{JsonElement, JsonError, getAtKey}
    use Json.JsonError.JsonError
    use Json.FromJson
    use Json/FromJson.fromJsonAt
    use Json/Path.{Path, !!, apply}

    pub enum Api(String, List[String], Map[String, List[Class]]) with ToString
    pub enum Class(Sym) with ToString
    pub enum Sym(String) with ToString

    pub def namespaces(api: Api): List[String] = match api {
        case Api(_, nss, _) => nss
    }

    pub def mkErrorString(x: JsonError): String = match x {
        case JsonError(p, ss) => "Error at ${p}:\n${Set.join("\n", ss)}"
    }

    instance FromJson[Api] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[Api, JsonError] = 
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* version = getAtKey(p, "version", obj);
            let* namespaces = getAtKey(p, "namespaces", obj);
            let* classes = getAtKey(p, "classes", obj);
            Ok(Api(version, namespaces, classes))
    }

    instance FromJson[Class] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[Class, JsonError] = 
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* sym = getAtKey(p, "sym", obj);
            Ok(Class(sym))
    }

    instance FromJson[Sym] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[Sym, JsonError] =
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* name = getAtKey(p, "name", obj);
            Ok(Sym(name))
    }

    pub def parse(filename: String): Result[Api, String] \ IO =
        use Result.flatMap;
        let* s = File.read(filename) |> Result.mapErr(s -> "File not found: ${s}");
        let* json = Json/Parse.parse(s) |> Option.toOk("Unable to parse api.json");
        fromJsonAt(Path.Root, json) |> Result.mapErr(mkErrorString)
}
