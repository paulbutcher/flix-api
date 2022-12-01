use Html.{Element, t, e, +=, +<}
use Html.Element.Text
use Api.{Api, Class, TParam, With, Loc, Sig, FParam, Def, Instance}
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

def nsToUri(ns: String): String =
    ns |> String.replace(from = "/", to = "%2F")

def span(clazz: String): Chain[Element] = e("span") += ("class", clazz)

def sym(s: String): Chain[Element] = span("sym") +< t(s)

def clz(s: String): Chain[Element] = span("clz") +< t(s)

def ann(s: String): Chain[Element] = span("ann") +< t(s)

def h1(s: String): Chain[Element] = e("h1") +< t(s)

def h2(s: String): Chain[Element] = e("h2") +< t(s)

def h6(s: String): Chain[Element] = e("h6") +< t(s)

def a(href: String): Chain[Element] = e("a") += ("href", href)

def ifNotEmpty(xs: List[a], f: Unit -> Chain[Element]): Chain[Element] = match xs {
    case Nil => Chain.empty()
    case _ => f()
}

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
    t("[") ++
    (tps |> List.toChain |> Chain.map(tparam(verbose)) |> Chain.intersperse(t(", ")) |> Chain.flatten) ++
    t("]")

def wth(sc: With): Chain[Element] = match sc {
    case With(Sym(n, _), tpe) =>
        clz(n) ++ t("[") ++ (span("tparam") +< t(tpe)) ++ t("]")
}

def withs(scs: List[With]): Chain[Element] = 
    ifNotEmpty(scs, () ->
        t(" ") ++ t("with ") ++
            (scs |> List.map(wth) |> List.intersperse(t(", ")) |> List.fold))

def tpe(t0: String): Chain[Element] =
    span("tpe") +< t(t0)

def fparam(fp: FParam): Chain[Element] = match fp {
    case FParam(name, pt) => 
        (span("fparam") +< t(name)) ++ 
        t(": ") ++
        tpe(pt)
}

def fparams(fps: List[FParam]): Chain[Element] =
    t("(") ++
    (fps |> List.toChain |> Chain.map(fparam) |> Chain.intersperse(t(", ")) |> Chain.flatten) ++
    t(")")

def effect(ef: String): Chain[Element] =
    if (String.isEmpty(ef))
        Chain.empty()
    else
        t(" & ") ++ span("effect") +< t(ef)

def annotations(anns: List[String]): Chain[Element] = 
    ifNotEmpty(anns, () ->
        (anns |> List.toChain |> Chain.map(ann) |> Chain.intersperse(t(", ")) |> Chain.flatten) ++
        e("br"))

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
        (sigs |> List.toChain |> Chain.flatMap(signature)))

def defn(d: Def): Chain[Element] = match d {
    case Def(Sym(name, _), _, anns, tps, fps, t0, ef, ws, _) =>
        defOrSig(name, Option.getWithDefault(Nil, anns), tps, fps, t0, ef, ws)
}

def defs(ds: List[Def]): Chain[Element] = 
    ifNotEmpty(ds, () ->
        h6("Definitions") ++
        (ds |> List.toChain |> Chain.flatMap(defn)))

def inst(v: String, i: Instance): Chain[Element] = match i {
    case Instance(_, _, t0, ws, loc) =>
        link(v, loc) ++
        e("div") +< (
            t("instance ") ++ tpe(t0) ++ withs(ws))
}

def insts(v: String, is: List[Instance]): Chain[Element] = 
    ifNotEmpty(is, () ->
        h6("Instances") ++
        (is |> List.toChain |> Chain.flatMap(inst(v))))

def clazz(mdToHtml: String -> String, v: String, c: Class): Chain[Element] = match c {
    case Class(Sym(name, loc), d, _, _, tp, scs, sigs, ds, is) =>
        e("div") += ("class", "border") +< (
            link(v, loc) ++
            e("code") +< (
                t("class ") ++ sym(name) ++ tparams(verbose = true, tp :: Nil) ++ withs(scs)) ++
            doc(mdToHtml, d) ++
            signatures(sigs) ++
            defs(ds) ++
            insts(v, is))
}

def classes(mdToHtml: String -> String, v: String, cs: List[Class]): Chain[Element] =
    h2("Classes") ++
    (cs |> List.toChain |> Chain.flatMap(clazz(mdToHtml, v)))

def nspace(api: Api, mdToHtml: String -> String, ns: String): Chain[Element] = match api {
    case Api(v, _, cs, _enums, _aliases, _defs) =>
        h1(ns) ++
        (cs |> Map.getWithDefault(ns, Nil) |> classes(mdToHtml, v))
}
