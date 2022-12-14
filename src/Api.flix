namespace Api {
    use Json.{JsonElement, JsonError, getAtKey, getAtKeyOpt}
    use Json.JsonError.JsonError
    use Json.FromJson
    use Json/FromJson.fromJsonAt
    use Json/Path.Path

    pub enum Api(String, List[String], Map[String, List[Class]], Map[String, List[Enum]], Map[String, List[TypeAlias]], Map[String, List[Def]])
    pub enum Class(Sym, List[String], List[String], List[String], TParam, List[With], List[Sig], List[Def], List[Instance])
    pub enum Enum(Sym, List[String], List[String], List[TParam], List[Case], List[Sym], List[Sym])
    pub enum TypeAlias(Sym, List[String], List[TParam], String, Loc)
    pub enum Def(Sym, List[String], Option[List[String]], List[TParam], List[FParam], String, String, List[With], Loc)
    pub enum Sym(String, Loc)
    pub enum Loc(String, Int32, Int32)
    pub enum TParam(String, String)
    pub enum FParam(String, String)
    pub enum With(Sym, String)
    pub enum Sig(Sym, List[String], List[String], List[TParam], List[FParam], String, String, List[With], Loc)
    pub enum Instance(Sym, List[String], String, List[With], Loc)
    pub enum Case(String, String)

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
            let* enums = getAtKey(p, "enums", obj);
            let* aliases = getAtKey(p, "typeAliases", obj);
            let* defs = getAtKey(p, "defs", obj);
            Ok(Api(version, namespaces, classes, enums, aliases, defs))
    }

    instance FromJson[Class] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[Class, JsonError] = 
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* sym = getAtKey(p, "sym", obj);
            let* doc = getAtKey(p, "doc", obj);
            let* ann = getAtKey(p, "ann", obj);
            let* mod = getAtKey(p, "mod", obj);
            let* tp = getAtKey(p, "tparam", obj);
            let* scs = getAtKey(p, "superClasses", obj);
            let* sigs = getAtKey(p, "sigs", obj);
            let* defs = getAtKey(p, "defs", obj);
            let* instances = getAtKey(p, "instances", obj);
            Ok(Class(sym, doc, ann, mod, tp, scs, sigs, defs, instances))
    }

    instance FromJson[Enum] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[Enum, JsonError] = 
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* sym = getAtKey(p, "sym", obj);
            let* doc = getAtKey(p, "doc", obj);
            let* ann = getAtKey(p, "ann", obj);
            let* tps = getAtKey(p, "tparams", obj);
            let* cases = getAtKey(p, "cases", obj);
            let* derives = getAtKey(p, "derives", obj);
            let* instances = getAtKey(p, "instances", obj);
            Ok(Enum(sym, doc, ann, tps, cases, derives, instances))
    }

    instance FromJson[TypeAlias] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[TypeAlias, JsonError] = 
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* sym = getAtKey(p, "sym", obj);
            let* doc = getAtKey(p, "doc", obj);
            let* tps = getAtKey(p, "tparams", obj);
            let* tpe = getAtKey(p, "tpe", obj);
            let* loc = getAtKey(p, "loc", obj);
            Ok(TypeAlias(sym, doc, tps, tpe, loc))
    }

    instance FromJson[Def] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[Def, JsonError] = 
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* sym = getAtKey(p, "sym", obj);
            let* doc = getAtKey(p, "doc", obj);
            let* ann = getAtKeyOpt(p, "ann", obj);
            let* tps = getAtKey(p, "tparams", obj);
            let* fps = getAtKey(p, "fparams", obj);
            let* tpe = getAtKey(p, "tpe", obj);
            let* eff = getAtKey(p, "eff", obj);
            let* tcs = getAtKey(p, "tcs", obj);
            let* loc = getAtKey(p, "loc", obj);
            Ok(Def(sym, doc, ann, tps, fps, tpe, eff, tcs, loc))
    }

    instance FromJson[Sym] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[Sym, JsonError] =
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* name = getAtKey(p, "name", obj);
            let* loc = getAtKey(p, "loc", obj);
            Ok(Sym(name, loc))
    }

    instance FromJson[Loc] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[Loc, JsonError] =
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* name = getAtKey(p, "name", obj);
            let* begin = getAtKey(p, "beginLine", obj);
            let* end = getAtKey(p, "endLine", obj);
            Ok(Loc(name, begin, end))
    }

    instance FromJson[TParam] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[TParam, JsonError] =
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* name = getAtKey(p, "name", obj);
            let* kind = getAtKey(p, "kind", obj);
            Ok(TParam(name, kind))
    }

    instance FromJson[FParam] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[FParam, JsonError] =
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* name = getAtKey(p, "name", obj);
            let* tpe = getAtKey(p, "tpe", obj);
            Ok(FParam(name, tpe))
    }

    instance FromJson[With] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[With, JsonError] = 
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* sym = getAtKey(p, "sym", obj);
            let* tpe = getAtKey(p, "tpe", obj);
            Ok(With(sym, tpe))
    }

    instance FromJson[Sig] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[Sig, JsonError] = 
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* sym = getAtKey(p, "sym", obj);
            let* doc = getAtKey(p, "doc", obj);
            let* mod = getAtKey(p, "mod", obj);
            let* tps = getAtKey(p, "tparams", obj);
            let* fps = getAtKey(p, "fparams", obj);
            let* tpe = getAtKey(p, "tpe", obj);
            let* eff = getAtKey(p, "eff", obj);
            let* tcs = getAtKey(p, "tcs", obj);
            let* loc = getAtKey(p, "loc", obj);
            Ok(Sig(sym, doc, mod, tps, fps, tpe, eff, tcs, loc))
    }

    instance FromJson[Instance] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[Instance, JsonError] = 
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* sym = getAtKey(p, "sym", obj);
            let* ann = getAtKey(p, "ann", obj);
            let* tpe = getAtKey(p, "tpe", obj);
            let* tcs = getAtKey(p, "tcs", obj);
            let* loc = getAtKey(p, "loc", obj);
            Ok(Instance(sym, ann, tpe, tcs, loc))
    }

    instance FromJson[Case] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[Case, JsonError] =
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* tag = getAtKey(p, "tag", obj);
            let* tpe = getAtKey(p, "tpe", obj);
            Ok(Case(tag, tpe))
    }

    pub def parse(filename: String): Result[Api, String] \ IO =
        use Result.flatMap;
        let* s = File.read(filename) |> Result.mapErr(s -> "File not found: ${s}");
        let* json = Json/Parse.parse(s) |> Option.toOk("Unable to parse api.json");
        fromJsonAt(Path.Root, json) |> Result.mapErr(mkErrorString)
}
