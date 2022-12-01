namespace Index {
    use Html.{t, e, +=, +<, Element}

    pub def index(): Chain[Element] =
        e("div") += ("class", "row") +< (
            e("div") += ("class", "col col-auto vh-100") +< (
                e("iframe") += ("src", "sidebar.html") += ("height", "100%") += ("width", "200px")) ++
            e("div") += ("class", "col vh-100 w-100") +< (
                e("iframe") += ("name", "main") += ("src", "Prelude.html") += ("height", "100%") += ("width", "100%")))
}
