namespace Namespace {
    use Html.{Element, t, e, +=, +<}
    use Html.Element.Text
    use Api.{Api, Class, Sym, TParam, With, Loc, Sig, FParam, Def, Instance, Enum, Case, TypeAlias}
    use Api.Api.Api
    use Api.Class.Class
    use Api.Sym.Sym
    use Api.TParam.TParam
    use Api.With.With
    use Api.Loc.Loc
    use Api.Sig.Sig
    use Api.FParam.FParam
    use Api.Def.Def
    use Api.Instance.Instance
    use Api.Enum.Enum
    use Api.Case.Case
    use Api.TypeAlias.TypeAlias

    def span(clazz: String): Chain[Element] = e("span") += ("class", clazz)

    def sym(s: String): Chain[Element] = span("sym") +< t(s)

    def clz(s: String): Chain[Element] = span("clz") +< t(s)

    def ann(s: String): Chain[Element] = span("ann") +< t(s)

    def tag(s: String): Chain[Element] = span("tag") +< t(s)

    def h1(s: String): Chain[Element] = e("h1") +< t(s)

    def h2(s: String): Chain[Element] = e("h2") +< t(s)

    def h6(s: String): Chain[Element] = e("h6") +< t(s)

    def a(href: String): Chain[Element] = e("a") += ("href", href)

    def ifNotEmpty(xs: List[a], f: Unit -> Chain[Element]): Chain[Element] = match xs {
        case Nil => Chain.empty()
        case _ => f()
    }

    def mapElem(f: a -> Chain[Element], xs: List[a]): Chain[Element] =
        xs |> List.toChain |> Chain.flatMap(f)

    def mapIntersperse(f: a -> Chain[Element], sep: Chain[Element], xs: List[a]): Chain[Element] =
        xs |> List.map(f) |> List.intersperse(sep) |> List.fold

    def link(v: String, loc: Loc): Chain[Element] = match loc {
        case Loc(f, l1, l2) =>
            e("div") += ("class", "float-end") +< (
                a("https://github.com/flix/flix/blob/${v}/main/src/library/${f}#L${l1}-L${l2}") +<
                    (e("i") += ("class", "bi-link")))
    }

    def doc(mdToHtml: String -> String, doc: List[String]): Chain[Element] =
        e("div") += ("class", "doc") +< (
            t(doc |> List.join("\n") |> mdToHtml))

    def tparam(verbose: {verbose = Bool}, tp: TParam): Chain[Element] = match tp {
        case TParam(n, k) =>
            (span("tparam") +< t(n)) ++ 
                if (verbose.verbose)
                    t(": ") ++ 
                    (span("kind") +< t(k))
                else 
                    Chain.empty()
    }

    def tparams(verbose: {verbose = Bool}, tps: List[TParam]): Chain[Element] = 
        t("[") ++ mapIntersperse(tparam(verbose), t(", "), tps) ++ t("]")

    def wth(sc: With): Chain[Element] = match sc {
        case With(Sym(n, _), tpe) =>
            clz(n) ++ t("[") ++ (span("tparam") +< t(tpe)) ++ t("]")
    }

    def withs(scs: List[With]): Chain[Element] = 
        ifNotEmpty(scs, () ->
            t(" ") ++ t("with ") ++ mapIntersperse(wth, t(", "), scs))

    def derivation(d: Sym): Chain[Element] = match d {
        case Sym(n, _) => clz(n)
    }

    def derivations(ds: List[Sym]): Chain[Element] = 
        ifNotEmpty(ds, () ->
            t(" ") ++ t("with ") ++ mapElem(derivation, ds))

    def tpe(t0: String): Chain[Element] =
        span("tpe") +< t(t0)

    def fparam(fp: FParam): Chain[Element] = match fp {
        case FParam(name, pt) => 
            (span("fparam") +< t(name)) ++ 
            t(": ") ++
            tpe(pt)
    }

    def fparams(fps: List[FParam]): Chain[Element] =
        t("(") ++ mapIntersperse(fparam, t(", "), fps) ++ t(")")

    def effect(ef: String): Chain[Element] =
        if (String.isEmpty(ef))
            Chain.empty()
        else
            t(" & ") ++ span("effect") +< t(ef)

    def annotations(anns: List[String]): Chain[Element] = 
        ifNotEmpty(anns, () ->
            mapIntersperse(ann, t(", "), anns))

    def defOrSig(name: String, anns: List[String], tps: List[TParam], fps: List[FParam], t0: String, ef: String, ws: List[With]): Chain[Element] =
        e("div") +<
            (e("code") +< (
                annotations(anns) ++
                t("def ") ++ sym(name) ++ 
                tparams(verbose = false, tps) ++
                fparams(fps) ++ t(": ") ++ tpe(t0) ++ effect(ef) ++
                withs(ws)))


    def signature(sig: Sig): Chain[Element] = match sig {
        case Sig(Sym(name, _), _, _, tps, fps, t0, ef, ws, _) => 
            defOrSig(name, Nil, tps, fps, t0, ef, ws)
    }

    def signatures(sigs: List[Sig]): Chain[Element] = 
        ifNotEmpty(sigs, () ->
            h6("Signatures") ++
            mapElem(signature, sigs))

    def classDefn(d: Def): Chain[Element] = match d {
        case Def(Sym(name, _), _, anns, tps, fps, t0, ef, ws, _) =>
            defOrSig(name, Option.getWithDefault(Nil, anns), tps, fps, t0, ef, ws)
    }

    def classDefs(ds: List[Def]): Chain[Element] = 
        ifNotEmpty(ds, () ->
            h6("Definitions") ++
            mapElem(classDefn, ds))

    def inst(v: String, i: Instance): Chain[Element] = match i {
        case Instance(_, _, t0, ws, loc) =>
            link(v, loc) ++
            e("div") +< (
                t("instance ") ++ tpe(t0) ++ withs(ws))
    }

    def insts(v: String, is: List[Instance]): Chain[Element] = 
        ifNotEmpty(is, () ->
            h6("Instances") ++
            mapElem(inst(v), is))

    def cse(c: Case): Chain[Element] = match c {
        case Case(tg, t0) =>
            e("div") +< (
                e("code") +< (
                    t("case ") ++ tag(tg) ++ t("(") ++ tpe(t0) ++ t(")")))
    }

    def cases(cs: List[Case]): Chain[Element] =
        mapElem(cse, cs)

    def clazz(mdToHtml: String -> String, v: String, c: Class): Chain[Element] = match c {
        case Class(Sym(name, loc), d, _, _, tp, scs, sigs, ds, is) =>
            e("div") += ("class", "border") +< (
                link(v, loc) ++
                e("code") +< (
                    t("class ") ++ sym(name) ++ tparams(verbose = true, tp :: Nil) ++ withs(scs)) ++
                doc(mdToHtml, d) ++
                signatures(sigs) ++
                classDefs(ds) ++
                insts(v, is))
    }

    def classes(mdToHtml: String -> String, v: String, cs: List[Class]): Chain[Element] =
        ifNotEmpty(cs, () ->
            h2("Classes") ++
            mapElem(clazz(mdToHtml, v), cs))

    def enm(mdToHtml: String -> String, v: String, en: Enum): Chain[Element] = match en {
        case Enum(Sym(name, loc), d, _, tps, cs, ds, _) =>
            e("div") += ("class", "border") +< (
                link(v, loc) ++
                e("code") +< (
                    t("enum ") ++ sym(name) ++ tparams(verbose = true, tps) ++ derivations(ds)) ++
                cases(cs) ++
                doc(mdToHtml, d))
    }

    def enums(mdToHtml: String -> String, v: String, es: List[Enum]): Chain[Element] =
        ifNotEmpty(es, () ->
            h2("Enums") ++
            mapElem(enm(mdToHtml, v), es))

    def aliaz(mdToHtml: String -> String, v: String, a0: TypeAlias): Chain[Element] = match a0 {
        case TypeAlias(Sym(name, loc), d, tps, t0, _) =>
            e("div") += ("class", "border") +< (
                link(v, loc) ++
                e("code") +< (
                    t("type alias ") ++ sym(name) ++ tparams(verbose = true, tps) ++
                    t(" = ") ++ tpe(t0)) ++
                doc(mdToHtml, d))
    }

    def aliases(mdToHtml: String -> String, v: String, as0: List[TypeAlias]): Chain[Element] =
        ifNotEmpty(as0, () ->
            h2("Type Aliases") ++
            mapElem(aliaz(mdToHtml, v), as0))

    def defn(mdToHtml: String -> String, v: String, d0: Def): Chain[Element] = match d0 {
        case Def(Sym(name, loc), d, anns, tps, fps, t0, ef, ws, _) =>
            e("div") += ("class", "border") +< (
                link(v, loc) ++
                defOrSig(name, Option.getWithDefault(Nil, anns), tps, fps, t0, ef, ws) ++
                doc(mdToHtml, d))
    }

    def defs(mdToHtml: String -> String, v: String, d0: List[Def]): Chain[Element] =
        ifNotEmpty(d0, () ->
            h2("Definitions") ++
            mapElem(defn(mdToHtml, v), d0))

    pub def nspace(api: Api, mdToHtml: String -> String, ns: String): Chain[Element] = match api {
        case Api(v, _, cs, es, as0, ds) =>
            h1(ns) ++
            (cs |> Map.getWithDefault(ns, Nil) |> classes(mdToHtml, v)) ++
            (es |> Map.getWithDefault(ns, Nil) |> enums(mdToHtml, v)) ++
            (as0 |> Map.getWithDefault(ns, Nil) |> aliases(mdToHtml, v)) ++
            (ds |> Map.getWithDefault(ns, Nil) |> defs(mdToHtml, v))
    }
}
