use Html.{Element, t, e, +=, +<}
use Api.{Api, Class}
use Api.Api.Api
use Api.Class.Class
use Api.Sym.Sym

def mkClass(c: Class): Chain[Element] = match c {
    case Class(Sym(_, name, _loc), _doc, _ann, _mod, _tparam, _superClasses, _sigs, _instances) =>
        e("div") +< t(name)
}

def mkClasses(classes: List[Class]): Chain[Element] =
    (e("h2") +< t("Classes")) ++
    (classes |> List.toChain |> Chain.flatMap(mkClass))

def mkNamespace(api: Api, ns: String): Chain[Element] = match api {
    case Api(_, _, classes, _enums, _aliases, _defs) =>
        (e("h1") +< t(ns)) +<
        mkClasses(Map.getWithDefault(ns, Nil, classes))
}
