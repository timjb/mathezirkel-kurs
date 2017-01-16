## Dependencies

Install `diagrams-binomialcoefficients`:

```bash
$ cd diagrams-binomialcoefficients
$ cabal install
```

Install `diagrams-builder` with PostScript backend:

```bash
$ cabal install -fcairo diagrams-builder
```

## Compilation

To compile, call LaTeX with

```bash
$ pdflatex --enable-write18 uebung.tex
```
