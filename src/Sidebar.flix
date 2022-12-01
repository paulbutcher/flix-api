namespace Sidebar {
    use Html.{t, e, +=, +<, Element}
    use HtmlUtils.{a, mapElem, nsToUri}
    use Api.Api
    use Api.Api.Api

    def nspace(ns: String): Chain[Element] =
        e("div") += ("class", "accordion-item") +< (
            e("div") += ("class", "accordion-header") +< (
                e("button") += ("class", "accordion-button btn-small") += ("type", "button") +< (
                    a(nsToUri(ns)) += ("target", "main") +<
                        t(ns))) ++
            e("div") += ("class", "accordion-collapse collapse show") +< (
                e("div") += ("class", "accordion-body") +< (
                    t("Body goes here"))))

    pub def sidebar(api: Api): Chain[Element] = match api {
        case Api(_, nss, _, _, _, _) =>
            e("div") += ("class", "accordion") +<
                mapElem(nspace, nss)
    }
}
