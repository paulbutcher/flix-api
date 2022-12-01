namespace Sidebar {
    use Html.{t, e, +=, +<, Element}
    use HtmlUtils.{a, mapElem, nsToUri, classAnchor, enumAnchor, aliasAnchor, defnAnchor}
    use Api.{Class, Enum, TypeAlias, Def}
    use Api.Api
    use Api.Api.Api
    use Api.Class.Class
    use Api.Sym.Sym
    use Api.Enum.Enum
    use Api.TypeAlias.TypeAlias
    use Api.Def.Def

    def clazz(uri: String, c: Class): Chain[Element] = match c {
        case Class(Sym(name, _), _, _, _, _, _, _, _, _) =>
            e("div") +< (
                a("${uri}#${classAnchor(name)}") += ("target", "main") +< t(name))
    }

    def enum0(uri: String, e0: Enum): Chain[Element] = match e0 {
        case Enum(Sym(name, _), _, _, _, _, _, _) =>
            e("div") +< (
                a("${uri}#${enumAnchor(name)}") += ("target", "main") +< t(name))
    }

    def alias0(uri: String, a0: TypeAlias): Chain[Element] = match a0 {
        case TypeAlias(Sym(name, _), _, _, _, _) =>
            e("div") +< (
                a("${uri}#${aliasAnchor(name)}") += ("target", "main") +< t(name))
    }

    def defn(uri: String, d: Def): Chain[Element] = match d {
        case Def(Sym(name, _), _, _, _, _, _, _, _, _) =>
            e("div") +< (
                a("${uri}#${defnAnchor(name)}") += ("target", "main") +< t(name))
    }

    def nspace(api: Api, ns: String): Chain[Element] = match api {
        case Api(_, _, cs, es, as0, ds) =>
            let uri = nsToUri(ns);
            e("div") += ("class", "accordion-item") +< (
                e("div") += ("class", "accordion-header") +< (
                    a(uri) += ("class", "accordion-button") += ("target", "main") +< t(ns)) ++
                e("div") += ("class", "accordion-collapse collapse show") +< (
                    e("div") += ("class", "accordion-body") +< (
                        (cs |> Map.getWithDefault(ns, Nil) |> mapElem(clazz(uri))) ++
                        (es |> Map.getWithDefault(ns, Nil) |> mapElem(enum0(uri))) ++
                        (as0 |> Map.getWithDefault(ns, Nil) |> mapElem(alias0(uri)) ++
                        (ds |> Map.getWithDefault(ns, Nil) |> mapElem(defn(uri)))))))
    }

    pub def sidebar(api: Api): Chain[Element] = match api {
        case Api(_, nss, _, _, _, _) =>
            e("div") += ("class", "accordion") +<
                mapElem(nspace(api), nss)
    }
}
