# rebar3_edoc_extensions

A pugin for extensions of [EDoc](https://www.erlang.org/doc/apps/edoc/chapter.html)

## Build

```sh
$ rebar3 compile
```

## Use

Add the plugin to your rebar config:

```erlang
{project_plugins, [{rebar3_edoc_extensions, "~> 0.1.0"}]}.
```

For generate `doc` with new style, run:
```sh
$ rebar3 edoc_style
```
