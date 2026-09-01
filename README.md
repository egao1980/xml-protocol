# xml-protocol

CLOS **XML Infoset** + pull events + streaming writer for [cl-stack](https://github.com/egao1980/cl-stack). Hard-implements [`serdes-protocol`](https://github.com/egao1980/serdes-protocol) `:xml`.

| System | Role | OCI |
|--------|------|-----|
| `xml-protocol` (`stack-xml`) | Nodes, events, `encode` / `decode`, writer GFs | **0.1.0** |
| `xml-backend-native` | Default — well-formed XML 1.0 + Namespaces | **0.1.0** |

XSD lives in [`schema-protocol-xsd`](https://github.com/egao1980/schema-protocol-xsd). This package is well-formed syntax only.

```lisp
(asdf:load-system "xml-backend-native")

(let ((doc (stack-xml:decode "<root xmlns='urn:x'><n a='1'>hi</n></root>")))
  (stack-xml:xml-element-text (stack-xml:xml-child (stack-xml:document-root-element doc) "n")))

(stack-serdes:encode doc :format :xml)
(stack-serdes:make-event-parser "<a/>" :format :xml)
```

XXE-safe: no external subset / general entities (predefined + numeric refs only).

## License

MIT — see [LICENSE](LICENSE).
