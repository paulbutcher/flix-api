namespace HtmlUtils {
    use Html.{Element, +=, +<, e, t}

    pub def span(clazz: String): Chain[Element] = e("span") += ("class", clazz)

    pub def sym(s: String): Chain[Element] = span("sym") +< t(s)

    pub def clz(s: String): Chain[Element] = span("clz") +< t(s)

    pub def ann(s: String): Chain[Element] = span("ann") +< t(s)

    pub def tag(s: String): Chain[Element] = span("tag") +< t(s)

    pub def h1(s: String): Chain[Element] = e("h1") +< t(s)

    pub def h2(s: String): Chain[Element] = e("h2") +< t(s)

    pub def h6(s: String): Chain[Element] = e("h6") +< t(s)

    pub def a(href: String): Chain[Element] = e("a") += ("href", href)

    pub def card(id: String, content: Chain[Element]) : Chain[Element] =
        e("div") += ("class", "card") += ("id", id) +< (
            e("div") += ("class", "card-body") +<
                content)

    pub def pill(s: String): Chain[Element] =
        (e("i") += ("class", "bi-record-circle-fill pill pill-${s}")) ++ t(" ")

    pub def ifNotEmpty(xs: List[a], f: Unit -> Chain[Element]): Chain[Element] = match xs {
        case Nil => Chain.empty()
        case _ => f()
    }

    pub def mapElem(f: a -> Chain[Element], xs: List[a]): Chain[Element] =
        xs |> List.toChain |> Chain.flatMap(f)

    pub def mapIntersperse(f: a -> Chain[Element], sep: Chain[Element], xs: List[a]): Chain[Element] =
        xs |> List.map(f) |> List.intersperse(sep) |> List.fold

    def anchor(tpe: String, ns: String, s: String): String =
        "${ns}:${s}:${tpe}"

    pub def classAnchor(ns: String, s: String): String = anchor("clazz", ns, s)

    pub def enumAnchor(ns: String, s: String): String = anchor("enum", ns, s)

    pub def aliasAnchor(ns: String, s: String): String = anchor("alias", ns, s)

    pub def defnAnchor(ns: String, s: String): String = anchor("defn", ns, s)

    pub def nsAnchor(ns: String): String = ns
}
