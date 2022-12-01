namespace Sidebar {
    use Html.{t, e, +=, +<, Element}
    use Api.Api
    use Api.Api.Api

    pub def sidebar(api: Api): Chain[Element] = match api {
        case Api(_, namespaces, _, _, _, _) =>
            e("ul") +< 
                (namespaces |> List.toChain |> Chain.flatMap(ns -> e("li") +< t(ns)))
    }
}
