use Html.{Element, t, e, +=, +<}
use Html.Element.Text
use Api.{Api, Class, TParam, SuperClass}
use Api.Api.Api
use Api.Class.Class
use Api.Sym.Sym
use Api.TParam.TParam
use Api.SuperClass.SuperClass

def nsToUri(ns: String): String =
    ns |> String.replace(from = "/", to = "%2F")

def span(clazz: String): Chain[Element] = e("span") += ("class", clazz)

def kw(s: String): Chain[Element] = span("keyword") +< t(s)

def sym(s: String): Chain[Element] = span("sym") +< t(s)

def h1(s: String): Chain[Element] = e("h1") +< t(s)

def h2(s: String): Chain[Element] = e("h2") +< t(s)

def h3(s: String): Chain[Element] = e("h3") +< t(s)

def doc(mdToHtml: String -> String, doc: List[String]): Chain[Element] =
    e("div") += ("class", "doc") +< (
        t(doc |> List.join("\n") |> mdToHtml))

def tparam(tp: TParam): Chain[Element] = match tp {
    case TParam(n, k) =>
        t("[") ++ 
        (span("tparam") +< t(n)) ++ 
        t(": ") ++ 
        (span("kind") +< t(k)) ++ 
        t("]")
}

def superclass(sc: SuperClass): Chain[Element] = match sc {
    case SuperClass(Sym(_, n, _), tpe) =>
        sym(n) ++ t("[") ++ (span("tparam") +< t(tpe)) ++ t("]")
}

def superclasses(scs: List[SuperClass]): Chain[Element] = match scs {
    case Nil => Chain.empty()
    case _ =>
        t(" ") ++ kw("with") ++ t(" ") ++
            (scs |> List.map(superclass) |> List.intersperse(t(", ")) |> List.fold)
}

def clazz(mdToHtml: String -> String, c: Class): Chain[Element] = match c {
    case Class(Sym(_, name, _loc), d, _, _, tp, scs, _sigs, _instances) =>
        e("div") += ("class", "border") +< (
            e("code") +< (
                kw("class") ++ t(" ") ++ sym(name) ++ tparam(tp) ++ superclasses(scs)) ++
            doc(mdToHtml, d))
}

def classes(mdToHtml: String -> String, cs: List[Class]): Chain[Element] =
    h2("Classes") ++
    (cs |> List.toChain |> Chain.flatMap(clazz(mdToHtml)))

def nspace(api: Api, mdToHtml: String -> String, ns: String): Chain[Element] = match api {
    case Api(_, _, cs, _enums, _aliases, _defs) =>
        h1(ns) ++
        (cs |> Map.getWithDefault(ns, Nil) |> classes(mdToHtml))
}
