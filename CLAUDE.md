# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

3SF Web Browser: an educational desktop web browser in Haskell (wxHaskell GUI) that renders a simple HTML/XML-like markup plus a subset of CSS. Single executable `3sfwbrowser`, entry point `src/MainWebBrowserGui.hs`. Code identifiers and comments are a mix of English and Spanish (e.g. `Propiedades`, `ProcesarEstilo`, `ObtenerHojasEstilo`).

## Build / run

There is no test suite or linter configured. Build with Cabal (`Build-Type: Custom`):

```
cabal install            # builds and installs; also copies config/ to ~/.3SFWebBrowser/
```

- `Setup.hs` hooks in **uuagc** (UU Attribute Grammar Compiler, `uuagc` / `uuagc-cabal`), so it must be installed for `.ag` files to compile. Per-file uuagc flags live in `uuagc_options`.
- The custom install hook creates `~/.3SFWebBrowser/` from `config/`. After install, edit `~/.3SFWebBrowser/dbf` so `User_Agent_Stylesheet` (and `User_Stylesheet`) point to real files; the checked-in `config/dbf` contains developer-specific paths.
- `examples/` holds HTML/CSS pages (`test*.html`, `ex*.html`) used for manual testing in the GUI; `doc/screenshoots/` shows expected output.
- GHC is run with `-Wall`.

## Architecture

The core is a rendering pipeline where most of the logic is written as **attribute grammars** (`src-ag/*.ag`, plus `src/Data/DataTree{HTML,CSS}.ag`), compiled by uuagc into Haskell modules. Plain Haskell in `src/` supplies parsing, helpers, and the GUI. Do not hand-edit generated Haskell; edit the `.ag` source. (`src-derived/` was committed once as generated output in an older layout and is no longer tracked.)

Pipeline, driven by `renderPage` / `genFormattingEstructure` in `src/MainWebBrowserGui.hs`:

1. **Fetch** — `Process/DownloadProcess.hs` (curl), `Utils/Url.hs`; local files and URLs both work.
2. **Parse** — `Parser/ParserHTML.hs` (tagsoup + uu-parsinglib) builds the `DataTreeHTML` AST; `Parser/ParserCSS.hs`, `BasicCssParser.hs`, `PropertyParser.hs`, `Propiedades.hs` parse stylesheets into `DataTreeCSS` / property values. `ProcesarEstilo.ag` processes parsed stylesheets. Two default stylesheets are parsed at startup (user-agent `html4.css` and user CSS) and kept in GUI variables.
3. **`NTree.ag`** (module `NTree`) — styling phase over the document tree: gathers stylesheets (`ObtenerHojasEstilo`), selector matching (`SelectorProcess`, `Process/MatchSelector.hs`), specified → computed values (`DoSpecifiedValue`, `DoComputedValue`), CSS counters (`CounterProcess`), generated content (`GeneratedContent`). Output is the input to phase 1 of the formatting structure.
4. **`FSTreeFase1.ag`** — "used values" (`DoUsedValue`), line building (`BuildLines`), list markers (`ListMarker`); produces the first formatting-structure tree (box model from `FSBox.ag` / `Process/CssBox.hs`).
5. **`FSTreeFase2.ag`** — sizes and positions (`WindowSize`, `WindowPosition`), then window tree and box tree (`GenWindowTree`, `GenBoxTree`) for drawing. `updateInitialContainer` re-runs this phase on window resize without re-parsing.
6. **Render** — wxHaskell painting in `MainWebBrowserGui.hs`; history navigation uses `Utils/ZipperList.hs`; settings in `Settings/Settings.hs`.

Notes:
- `.ag` files compose via `INCLUDE`; top-level files (`NTree`, `FSTreeFase1`, `FSTreeFase2`, `FSBox`, `ProcesarEstilo`) are the ones listed in `uuagc_options`, so a new `.ag` fragment must be `INCLUDE`d from one of them.
- `3SFunctionalWebBrowser.cabal`'s `other-modules` lists only some modules and its `Extra-source-files` mentions `tmp/*` while images live in `config/tmp/`; `uuagc_options` also references `FormatNTree.ag`, which doesn't exist (the file is `FormatTree.ag`, and is not wired into the executable). Check these when a build fails on missing modules or files.
- Branch `haskell-gi-boxer`: migration from wxHaskell to haskell-gi (GTK 4). The `3sfwbrowser` executable is commented out in the cabal file; the built executable is `3sfboxer` (`src/MainBoxerGui.hs`, box-tree viewer). Run it from the repo root (`cabal run 3sfboxer`), since it reads `./config/html4.css`.
- The AGs no longer depend on a GUI toolkit: the `cb` attribute is a `Utils.TextMeasure.TextMeasurer` (Pango implementation in `Utils/PangoMeasure.hs`). wx-only code is parked, unbuilt, in `Process/CssBoxWx.hs`, `Utils/TextExtend.hs` and `src-ag/GenWindowTree.ag` (its `INCLUDE` in `FSTreeFase2.ag` is commented out).
- Keep `default-language: Haskell2010`: it enables `DeepSubsumption`, which the uu-parsinglib `Parser` type synonym code relies on under GHC 9.x.
