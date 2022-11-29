use Html.{doctype, t, e, +=, +<, Element}
use Api.Api
use Api.Api.Api

def sidebar(api: Api): Chain[Html.Element] = match api {
    case Api(_, namespaces, _, _, _, _) =>
        e("ul") +< 
            (namespaces |> List.toChain |> Chain.flatMap(ns -> e("li") +< e("p") +< t(ns)))
}
