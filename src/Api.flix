namespace Api {
    use Json.{JsonElement, JsonError, getAtKey}
    use Json.JsonError.JsonError
    use Json.FromJson
    use Json/FromJson.fromJsonAt
    use Json/Path.Path

    pub enum Api(String, List[String], Map[String, List[Class]], Map[String, List[Enum]], Map[String, List[TypeAlias]], Map[String, List[Def]])
    pub enum Class(Sym, List[String], List[String], List[String], TParam, List[SuperClass], List[Sig], List[Instance])
    pub enum Enum(Sym, List[String], List[String], List[TParam], List[Case], List[Sym], List[Sym])
    pub enum TypeAlias(Sym, List[String], List[TParam], String, Loc)
    pub enum Def(String, Sym, List[String], List[String], List[TParam], List[FParam], String, String, List[Tc], Loc)
    pub enum Sym(List[String], String, Loc)
    pub enum ClassSym(Sym, String, Loc)
    pub enum Loc(String, Int32, Int32)
    pub enum TParam(String, String)
    pub enum FParam(String, String)
    pub enum SuperClass(Sym, String)
    pub enum Sig(ClassSym, List[String], List[String], List[TParam], List[FParam], String, String, List[Tc], Loc)
    pub enum Instance(Sym, List[String], String, List[Tc], Loc)
    pub enum Case(String, String)
    pub enum Tc(Sym, String)

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
            let* instances = getAtKey(p, "instances", obj);
            Ok(Class(sym, doc, ann, mod, tp, scs, sigs, instances))
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
            let* name = getAtKey(p, "name", obj);
            let* sym = getAtKey(p, "sym", obj);
            let* doc = getAtKey(p, "doc", obj);
            let* ann = getAtKey(p, "ann", obj);
            let* tps = getAtKey(p, "tparams", obj);
            let* fps = getAtKey(p, "fparams", obj);
            let* tpe = getAtKey(p, "tpe", obj);
            let* eff = getAtKey(p, "eff", obj);
            let* tcs = getAtKey(p, "tcs", obj);
            let* loc = getAtKey(p, "loc", obj);
            Ok(Def(name, sym, doc, ann, tps, fps, tpe, eff, tcs, loc))
    }

    instance FromJson[Sym] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[Sym, JsonError] =
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* ns = getAtKey(p, "namespace", obj);
            let* name = getAtKey(p, "name", obj);
            let* loc = getAtKey(p, "loc", obj);
            Ok(Sym(ns, name, loc))
    }

    instance FromJson[ClassSym] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[ClassSym, JsonError] =
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* sym = getAtKey(p, "classSym", obj);
            let* name = getAtKey(p, "name", obj);
            let* loc = getAtKey(p, "loc", obj);
            Ok(ClassSym(sym, name, loc))
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

    instance FromJson[SuperClass] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[SuperClass, JsonError] = 
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* sym = getAtKey(p, "sym", obj);
            let* tpe = getAtKey(p, "tpe", obj);
            Ok(SuperClass(sym, tpe))
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

    instance FromJson[Tc] {
        pub def fromJsonAt(p: Path, json: JsonElement): Result[Tc, JsonError] =
            use Result.flatMap;
            let* obj = fromJsonAt(p, json);
            let* sym = getAtKey(p, "sym", obj);
            let* tpe = getAtKey(p, "tpe", obj);
            Ok(Tc(sym, tpe))
    }

    pub def parse(filename: String): Result[Api, String] \ IO =
        use Result.flatMap;
        let* s = File.read(filename) |> Result.mapErr(s -> "File not found: ${s}");
        let* json = Json/Parse.parse(s) |> Option.toOk("Unable to parse api.json");
        fromJsonAt(Path.Root, json) |> Result.mapErr(mkErrorString)
}
