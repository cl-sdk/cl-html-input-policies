# io.github.cl-sdk.html-input-policies

`io.github.cl-sdk.html-input-policies` is a Common Lisp library for sanitizing HTML input.
It helps applications accept user-provided markup more safely by removing denied tags,
optionally stripping the content of dangerous elements, and returning a sanitized fragment
built on top of `io.github.cl-sdk.xml`.

## Examples

### Sanitize while parsing

```common-lisp
(io.github.cl-sdk.html-input-policies:parse-and-sanitize-html-input
 "<p>Hello<script>alert(1)</script>World</p>"
 '("script"))
```

Result:

```common-lisp
;; => sanitized fragment equivalent to: <p>HelloWorld</p>
```

### Sanitize an existing XML document

```common-lisp
(io.github.cl-sdk.html-input-policies:sanitize-html-input
 xml-document
 '("script" "iframe")
 :strip-content-tags '("script" "iframe"))
```

This removes denied tags from the document and fully drops the content of tags listed in
`strip-content-tags`.

## License

This project is released under the terms described in [LICENSE](./LICENSE).
