# rebar3_edoc_extensions
[![Hex.pm Version][hexpm version]][hexpm]
[![Hex.pm Downloads][hexpm downloads]][hexpm]
[![Hex.pm Documentation][hexdocs documentation]][hexdocs]
[![Build Status][gh badge]][gh]
[![Erlang Versions][erlang version badge]][gh]

A pugin for extensions of [EDoc](https://www.erlang.org/doc/apps/edoc/chapter.html)

## Build

```sh
$ rebar3 compile
```

## Use

Add the plugin to your rebar config:

```erlang
{project_plugins, [{rebar3_edoc_extensions, "~> 0.2.5"}]}.
```

For generate `doc` with new style, run:
```sh
$ rebar3 edoc_extensions
```

## Behavior
Use `rebar3 edoc_extensions` instead of `rebar3 edoc` for generate documentation.
Current plugin will redefine second options of `edoc_opts`: `stylesheet`, `xml_export`, `layout`, `doclet`.
Make sure that you don't use some config options which are described above - otherwise they will be overwritten by plugin.
Plugin will use `overview.edoc` by default which placed in `priv` folder of current source code.
If need to use own custom `overview.edoc`, just create `overview.edoc` in `doc` folder of root application.

## 3d party of `CSS`/`JavaScript`
* Used [https://prismjs.com](https://prismjs.com)
* Used [github-markdown-css](https://github.com/sindresorhus/github-markdown-css)

<!-- Badges -->
[hexpm]: https://hex.pm/packages/rebar3_edoc_extensions
[hexpm version]: https://img.shields.io/hexpm/v/rebar3_edoc_extensions.svg?style=flat-square
[hexpm downloads]: https://img.shields.io/hexpm/dt/rebar3_edoc_extensions.svg?style=flat-square
[hexdocs documentation]: https://img.shields.io/badge/hex-docs-purple.svg?style=flat-square
[hexdocs]: https://hexdocs.pm/rebar3_edoc_extensions
[gh]: https://github.com/vkatsuba/rebar3_edoc_extensions/actions/workflows/ci.yml
[gh badge]: https://img.shields.io/github/workflow/status/vkatsuba/rebar3_edoc_extensions/CI?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-23.0%20to%2024.1-blue.svg?style=flat-square
