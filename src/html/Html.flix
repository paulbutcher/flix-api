namespace Html {

    pub enum Element {
        case DocType(String)
        case Tag(String, Map[String, String], Option[Nec[Element]])
        case Text(String)
    }

    pub def html(es: Nec[Element]): String =
        es |> Nec.joinWith(fmtElement, "")

    def fmtElement(e: Element): String = match e {
        case DocType(name) => "<!doctype ${name}>"
        case Tag(name, attrs, None) =>
            "<${name} ${fmtAttrs(attrs)}/>"
        case Tag(name, attrs, Some(children)) =>
            "<${name} ${fmtAttrs(attrs)}>${children |> Nec.joinWith(fmtElement, "")}</${name}>"
        case Text(s) => s
    }

    def fmtAttrs(attrs: Map[String, String]): String =
        attrs |> Map.joinWith(fmtAttr, " ")

    def fmtAttr(k: String, v: String): String = "${k} = \"${v}\""

    pub def doctype(x: String): Nec[Element] = Nec.singleton(DocType(x))

    pub def e(x: String): Nec[Element] = Nec.singleton(Tag(x, Map#{}, None))

    pub def t(x: String): Nec[Element] = Nec.singleton(Text(x))

    pub def addAttribute(attr: (String, String), e: Element): Element =
        let (k, v) = attr;
        match e {
            case Tag(name, attrs, children) => Tag(name, attrs |> Map.insert(k, v), children)
            case Text(_) => bug!("Can't add attributes to text")
            case DocType(_) => bug!("Can't add attributes to doctype")
        }

    pub def addChildren(newChildren: Nec[Element], e: Element): Element = match e {
        case Tag(name, attrs, None) => Tag(name, attrs, Some(newChildren))
        case Tag(name, attrs, Some(children)) => Tag(name, attrs, Some(children ++ newChildren))
        case Text(_) => bug!("Can't add children to text")
        case DocType(_) => bug!("Can't add attributes to text")
    }

    pub def +=(es: Nec[Element], attr: (String, String)): Nec[Element] =
        match Nec.viewRight(es) {
            case Nec/ViewRight.OneRight(last) => Nec.singleton(addAttribute(attr, last))
            case Nec/ViewRight.SomeRight(rest, last) => Nec.snoc(rest, addAttribute(attr, last))
        }

    pub def +<(es: Nec[Element], children: Nec[Element]): Nec[Element] = 
        match Nec.viewRight(es) {
            case Nec/ViewRight.OneRight(last) => Nec.singleton(addChildren(children, last))
            case Nec/ViewRight.SomeRight(rest, last) => Nec.snoc(rest, addChildren(children, last))
        }
}
