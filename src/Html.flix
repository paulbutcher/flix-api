namespace Html {

    pub enum Element {
        case DocType(String)
        case Tag(String, Map[String, String], Chain[Element])
        case Text(String)
    }

    pub def html(es: Chain[Element]): String =
        es |> Chain.joinWith(fmtElement, "")

    def fmtElement(e: Element): String = match e {
        case DocType(name) => "<!doctype ${name}>"
        case Tag(name, attrs, children) =>
            "<${name} ${fmtAttrs(attrs)}>${children |> Chain.joinWith(fmtElement, "")}</${name}>"
        case Text(s) => s
    }

    def fmtAttrs(attrs: Map[String, String]): String =
        attrs |> Map.joinWith(fmtAttr, " ")

    def fmtAttr(k: String, v: String): String = "${k}=\"${v}\""

    pub def doctype(x: String): Chain[Element] = Chain.singleton(DocType(x))

    pub def e(x: String): Chain[Element] = Chain.singleton(Tag(x, Map#{}, Chain.empty()))

    pub def t(x: String): Chain[Element] = Chain.singleton(Text(x))

    pub def addAttribute(attr: (String, String), e: Element): Element =
        let (k, v) = attr;
        match e {
            case Tag(name, attrs, children) => Tag(name, attrs |> Map.insert(k, v), children)
            case Text(_) => bug!("Can't add attributes to text")
            case DocType(_) => bug!("Can't add attributes to doctype")
        }

    pub def addChildren(newChildren: Chain[Element], e: Element): Element = match e {
        case Tag(name, attrs, children) => Tag(name, attrs, children ++ newChildren)
        case Text(_) => bug!("Can't add children to text")
        case DocType(_) => bug!("Can't add attributes to text")
    }

    pub def +=(es: Chain[Element], attr: (String, String)): Chain[Element] =
        match Chain.viewRight(es) {
            case Chain/ViewRight.NoneRight => bug!("Can't add attributes to empty chain")
            case Chain/ViewRight.SomeRight(rest, last) => Chain.snoc(rest, addAttribute(attr, last))
        }

    pub def +<(es: Chain[Element], children: Chain[Element]): Chain[Element] = 
        match Chain.viewRight(es) {
            case Chain/ViewRight.NoneRight => bug!("Can't add children to empty chain")
            case Chain/ViewRight.SomeRight(rest, last) => Chain.snoc(rest, addChildren(children, last))
        }
}
