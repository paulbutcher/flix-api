namespace Sidebar {
    use Html.{t, e, +=, +<, Element}
    use HtmlUtils.{a, mapElem, nsToUri}
    use Api.Class
    use Api.Api
    use Api.Api.Api
    use Api.Class.Class
    use Api.Sym.Sym

    def clazz(uri: String, c: Class): Chain[Element] = match c {
        case Class(Sym(name, _), _, _, _, _, _, _, _, _) =>
            e("div") +< (
                a("${uri}#clazz:${name}") += ("target", "main") +< t(name))
    }

    def nspace(api: Api, ns: String): Chain[Element] = match api {
        case Api(_, _, cs, _es, _as0, _ds) =>
            let uri = nsToUri(ns);
            e("div") += ("class", "accordion-item") +< (
                e("div") += ("class", "accordion-header") +< (
                    e("button") += ("class", "accordion-button btn-small") += ("type", "button") +< (
                        a(uri) += ("target", "main") +< t(ns))) ++
                e("div") += ("class", "accordion-collapse collapse show") +< (
                    e("div") += ("class", "accordion-body") +< (
                        cs |> Map.getWithDefault(ns, Nil) |> mapElem(clazz(uri)))))
    }

    pub def sidebar(api: Api): Chain[Element] = match api {
        case Api(_, nss, _, _, _, _) =>
            e("div") += ("class", "accordion") +<
                mapElem(nspace(api), nss)
    }
}
