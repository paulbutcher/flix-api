namespace Sidebar {
    use Html.{t, e, +=, +<, Element}
    use HtmlUtils.{a, mapElem, classAnchor, enumAnchor, aliasAnchor, defnAnchor, span, pill}
    use Api.{Class, Enum, TypeAlias, Def}
    use Api.Api
    use Api.Api.Api
    use Api.Class.Class
    use Api.Sym.Sym
    use Api.Enum.Enum
    use Api.TypeAlias.TypeAlias
    use Api.Def.Def

    def clazz(ns: String, c: Class): Chain[Element] = match c {
        case Class(Sym(name, _), _, _, _, _, _, _, _, _) =>
            e("div") += ("class", "sidebar-link") += ("data-name", name) += ("data-link", classAnchor(ns, name)) +< (
                e("a") +< (
                    pill("c") ++
                    t(name)))
    }

    def enum0(ns: String, e0: Enum): Chain[Element] = match e0 {
        case Enum(Sym(name, _), _, _, _, _, _, _) =>
            e("div") += ("class", "sidebar-link") += ("data-name", name) += ("data-link", enumAnchor(ns, name)) +< (
                e("a") +< (
                    pill("e") ++
                    t(name)))
    }

    def alias0(ns: String, a0: TypeAlias): Chain[Element] = match a0 {
        case TypeAlias(Sym(name, _), _, _, _, _) =>
            e("div") += ("class", "sidebar-link") += ("data-name", name) += ("data-link", aliasAnchor(ns, name)) +< (
                e("a") +< (
                    pill("a") ++
                    t(name)))
    }

    def defn(ns: String, d: Def): Chain[Element] = match d {
        case Def(Sym(name, _), _, _, _, _, _, _, _, _) =>
            e("div") += ("class", "sidebar-link") += ("data-name", name) += ("data-link", defnAnchor(ns, name)) +< (
                e("a") +< (
                    pill("d") ++
                    t(name)))
    }

    def nspace(api: Api, i: Int32, ns: String): Chain[Element] = match api {
        case Api(_, _, cs, es, as0, ds) =>
            let id = "acc${i}";
            e("div") += ("class", "accordion-item") += ("data-ns", ns) +< (
                e("div") += ("class", "accordion-header") +< (
                    e("button") += ("class", "accordion-button collapsed") +=
                        ("data-bs-toggle", "collapse") += ("data-bs-target", "#${id}") +< t(ns)) ++
                e("div") += ("id", id) += ("class", "accordion-collapse collapse") +< (
                    e("div") += ("class", "accordion-body") +< (
                        (cs |> Map.getWithDefault(ns, Nil) |> mapElem(clazz(ns))) ++
                        (es |> Map.getWithDefault(ns, Nil) |> mapElem(enum0(ns))) ++
                        (as0 |> Map.getWithDefault(ns, Nil) |> mapElem(alias0(ns)) ++
                        (ds |> Map.getWithDefault(ns, Nil) |> mapElem(defn(ns)))))))
    }

    def logo(v: String): Chain[Element] = 
        e("div") +< (
            (span("logo") +< t("flix")) ++
            (span("version") +< t(v)))

    def search(): Chain[Element] = 
        e("div") +< (
            e("input") += ("type", "search") += ("class", "search") += 
                ("placeholder", "Filter") += ("oninput", "onSearch(this)")
        )

    pub def sidebar(api: Api): Chain[Element] = match api {
        case Api(v, nss, _, _, _, _) =>
            e("div") += ("class", "sidebar") +< (
                logo(v) ++
                search() ++
                e("div") += ("class", "accordion") += ("id", "accordion") +<
                    (nss |> List.mapWithIndex(nspace(api)) |> List.fold))
    }
}
