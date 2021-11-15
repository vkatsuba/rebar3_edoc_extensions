# rebar3_edoc_style

A rebar plugin description

## Build

```sh
$ rebar3 compile
```

## Use

Add the plugin to your rebar config:

```erlang
{project_plugins, [{rebar3_edoc_style, "~> 0.0.0"}]}.
```

Then just call your plugin directly in an existing application:
```sh
$ rebar3 rebar3_edoc_style
===> Fetching rebar3_edoc_style
===> Compiling rebar3_edoc_style
<Plugin Output>
```
