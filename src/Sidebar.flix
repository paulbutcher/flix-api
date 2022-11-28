use Html.{doctype, t, e, +=, +<, Element}
use Api.{Api, namespaces}

def sidebar(api: Api): Nec[Html.Element] =
    match api |> namespaces |> List.toNec {
        case None => e("ul")
        case Some(nss) =>
            e("ul") +< 
                (nss |> Nec.flatMap(ns -> e("li") +< e("p") +< t(ns)))
    }
