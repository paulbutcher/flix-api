use Html.{Element, t, e, +=, +<}
use Html.Element.Text
use Api.{Api, Class, TParam, With, Loc, Sig, ClassSym, FParam}
use Api.Api.Api
use Api.Class.Class
use Api.Sym.Sym
use Api.TParam.TParam
use Api.With.With
use Api.Loc.Loc
use Api.Sig.Sig
use Api.ClassSym.ClassSym
use Api.FParam.FParam

def nsToUri(ns: String): String =
    ns |> String.replace(from = "/", to = "%2F")

def span(clazz: String): Chain[Element] = e("span") += ("class", clazz)

def sym(s: String): Chain[Element] = span("sym") +< t(s)

def clz(s: String): Chain[Element] = span("clz") +< t(s)

def h1(s: String): Chain[Element] = e("h1") +< t(s)

def h2(s: String): Chain[Element] = e("h2") +< t(s)

def h6(s: String): Chain[Element] = e("h6") +< t(s)

def a(href: String): Chain[Element] = e("a") += ("href", href)

def gh(v: String, loc: Loc): Chain[Element] = match loc {
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
    case With(Sym(_, n, _), tpe) =>
        clz(n) ++ t("[") ++ (span("tparam") +< t(tpe)) ++ t("]")
}

def withs(scs: List[With]): Chain[Element] = match scs {
    case Nil => Chain.empty()
    case _ =>
        t(" ") ++ t("with ") ++
            (scs |> List.map(wth) |> List.intersperse(t(", ")) |> List.fold)
}

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

def signature(sig: Sig): Chain[Element] = match sig {
    case Sig(ClassSym(_, name, _), _, _, tps, fps, t0, ef, ws, _loc) => 
        e("div") +<
            (e("code") +< (
                t("def ") ++ sym(name) ++ 
                tparams(verbose = false, tps) ++
                fparams(fps) ++ t(": ") ++ tpe(t0) ++ effect(ef) ++
                withs(ws)))
}

def signatures(sigs: List[Sig]): Chain[Element] = 
    sigs |> List.toChain |> Chain.flatMap(signature)

def clazz(mdToHtml: String -> String, v: String, c: Class): Chain[Element] = match c {
    case Class(Sym(_, name, loc), d, _, _, tp, scs, sigs, _instances) =>
        e("div") += ("class", "border") +< (
            gh(v, loc) ++
            e("code") +< (
                t("class ") ++ sym(name) ++ tparams(verbose = true, tp :: Nil) ++ withs(scs)) ++
            doc(mdToHtml, d) ++
            h6("Signatures") ++
            signatures(sigs))
}

def classes(mdToHtml: String -> String, v: String, cs: List[Class]): Chain[Element] =
    h2("Classes") ++
    (cs |> List.toChain |> Chain.flatMap(clazz(mdToHtml, v)))

def nspace(api: Api, mdToHtml: String -> String, ns: String): Chain[Element] = match api {
    case Api(v, _, cs, _enums, _aliases, _defs) =>
        h1(ns) ++
        (cs |> Map.getWithDefault(ns, Nil) |> classes(mdToHtml, v))
}
