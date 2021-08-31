# OCaml-CSS-Validator

This Project is CSS Validator written by OCaml.

Follow CSS Version is CSS22. ( https://www.w3.org/TR/CSS22/ )

## how to use parser

### install dune and libs

```bash
$ opam install dune sedlex menhir
```

### use parser

```bash
$ cat test/css/at_rule.css | dune exec css_validator
```
