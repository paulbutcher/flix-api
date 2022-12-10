namespace Index {
    use Html.{t, e, +=, +<, Element, doctype}
    use Api.Api
    use Sidebar.sidebar
    use Namespace.namespaces

    pub def index(api: Api, mdToHtml: String -> String): Chain[Element] =
        doctype("html") ++
        e("html") += ("lang", "en") +< (
            e("head") +< (
                e("meta") += ("charset", "utf-8") ++
                e("meta") += ("name", "viewport") += ("content", "width=device-width, initial-scale=1") ++
                (e("title") +< t("Flix API Documentation")) ++
                e("link") +=
                    ("href", "https://cdn.jsdelivr.net/npm/bootstrap@5.2.3/dist/css/bootstrap.min.css") +=
                    ("rel", "stylesheet") += 
                    ("integrity", "sha384-rbsA2VBKQhggwzxH7pPCaAqO46MgnOM80zW1RWuH61DGLwZJEdK2Kadq2F9CUG65") +=
                    ("crossorigin", "anonymous") ++
                e("link") +=
                    ("href", "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.2/font/bootstrap-icons.css") +=
                    ("rel", "stylesheet") ++
                e("link") +=
                    ("href", "https://fonts.googleapis.com/css?family=Fira+Code&display=swap") +=
                    ("rel", "stylesheet") ++
                e("link") +=
                    ("href", "https://fonts.googleapis.com/css?family=Oswald&display=swap") +=
                    ("rel", "stylesheet") ++
                e("link") += ("href", "site.css") += ("rel", "stylesheet")) ++
            e("body") +< (
                sidebar(api) ++
                namespaces(api, mdToHtml)) ++
            e("script") +=
                ("src", "https://cdn.jsdelivr.net/npm/bootstrap@5.2.3/dist/js/bootstrap.bundle.min.js") +=
                ("integrity", "sha384-kenU1KFdBIe4zVF0s0G1M5b4hcpxyD9F7jL+jjXkk+Q2h455rYXK/7HAuoJl+0I4") +=
                ("crossorigin", "anonymous")) ++
            e("script") += ("src", "sidebar.js")
}
