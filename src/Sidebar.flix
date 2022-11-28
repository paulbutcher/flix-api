use Html.{doctype, t, e, +=, +<, Element}
use Api.{Api, namespaces}

def sidebar(api: Api): Chain[Html.Element] =
    e("ul") +< 
        (api |> namespaces |> List.toChain |> Chain.flatMap(ns -> e("li") +< e("p") +< t(ns)))
