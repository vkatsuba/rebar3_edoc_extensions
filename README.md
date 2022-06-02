# rebar3_edoc_extensions

[![Hex.pm Version][hexpm version]][hexpm]
[![Hex.pm Downloads][hexpm downloads]][hexpm]
[![Hex.pm Documentation][hexdocs documentation]][hexdocs]
[![Erlang Versions][erlang version badge]][gh]
[![Build Status][gh badge]][gh]

`rebar3_edoc_extensions` is a Rebar 3 plugin bringing improvements to
[EDoc](https://www.erlang.org/doc/apps/edoc/chapter.html)-based documentation.

## Getting started

To use this plugin for your project, simply add the plugin to the
`project_plugins` list in your `rebar.config` configuration:

```erlang
%% In `rebar.config`.
{project_plugins, [rebar3_edoc_extensions]}.
```

It overrides the defaut behavior of the `rebar edoc` command. Therefore, the
next time you generate the documentation, you will benefit from the
improvements brought by this plugin.

## EDoc improvements

Here is a list of changes this plugin makes to EDoc-generated documentation:

1. A table of content of the Overview page is added to the left sidebar.
2. The [GitHub Markdown stylesheet](https://github.com/sindresorhus/github-markdown-css)
   is used to style the entire documentation.
3. [PrismJS](https://prismjs.com/) is used to enable syntax highlighting to
   literals and code snippets.

## Configuration

The plugin supports a few options to configure the **PrismJS syntax
highlighting** library:

*   Select the version of PrismJS to download:
    ```erlang
    {prismjs_version, "v1.26.0"}.
    ```

    The default version is `"v1.26.0"`.

*   Select the PrismJS theme:
    ```erlang
    {prismjs_theme, "twilight"}.
    ```

    The default theme is `"default"`.

*   Select the list of languages to support:
    ```erlang
    {prismjs_languages, ["erlang", "javascript"]}.
    ```

    The default languages are `"erlang"` and `"elixir"`.

## Build

```sh
$ rebar3 compile
```

## Use

Add the plugin to your rebar config:

For generate `doc` with new style, run:
```sh
$ rebar3 edoc
```

<!-- Badges -->
[hexpm]: https://hex.pm/packages/rebar3_edoc_extensions
[hexpm version]: https://img.shields.io/hexpm/v/rebar3_edoc_extensions.svg?style=flat-square
[hexpm downloads]: https://img.shields.io/hexpm/dt/rebar3_edoc_extensions.svg?style=flat-square
[hexdocs documentation]: https://img.shields.io/badge/hex-docs-purple.svg?style=flat-square
[hexdocs]: https://hexdocs.pm/rebar3_edoc_extensions
[gh]: https://github.com/vkatsuba/rebar3_edoc_extensions/actions/workflows/ci.yaml
[gh badge]: https://img.shields.io/github/workflow/status/vkatsuba/rebar3_edoc_extensions/CI?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-23.X%20to%2025.X-blue.svg?style=flat-square
