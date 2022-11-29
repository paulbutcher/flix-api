namespace CommonMark {

    import org.commonmark.parser.Parser;
    import org.commonmark.parser.{Parser$Builder => ParserBuilder};
    import org.commonmark.node.Node;
    import org.commonmark.renderer.html.HtmlRenderer;
    import org.commonmark.renderer.html.{HtmlRenderer$Builder => HtmlRendererBuilder};

    pub def mkRenderer(): (String -> String) \ IO = 
        import static org.commonmark.parser.Parser.builder(): ParserBuilder \ IO as parserBuilder;
        import org.commonmark.parser.Parser$Builder.build(): Parser \ IO as buildParser;
        import static org.commonmark.renderer.html.HtmlRenderer.builder(): HtmlRendererBuilder \ IO as rendererBuilder;
        import org.commonmark.renderer.html.HtmlRenderer$Builder.build(): HtmlRenderer \ IO as buildRenderer;
        import org.commonmark.parser.Parser.parse(String): Node \ {};
        import org.commonmark.renderer.html.HtmlRenderer.render(Node): String \ {};

        let parser = parserBuilder() |> buildParser;
        let renderer = rendererBuilder() |> buildRenderer;

        s -> parse(parser, s) |> render(renderer)
}
