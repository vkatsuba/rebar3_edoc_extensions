%%% @doc Plugin provider for rebar3 rebar3_edoc_extensions.
-module(rebar3_edoc_extensions_prv).

-export([init/1, do/1, format_error/1]).

-ignore_xref([do/1,
              format_error/1,
              {providers, create, 1},
              {rebar_state, add_provider, 2},
              {rebar_state, command_parsed_args, 1}]).

-define(PROVIDER, edoc_extensions).
-define(DEPS, [compile]).
-define(OPTS,
        [{boolean, $b, "boolean", {boolean, false}, "Boolean example"},
         {string, $s, "rebar-config", {string, "some.txt"}, "String example"},
         {atom, $a, "ignore", atom, "Atom example"},
         {atom_none, $n, "atom-none", {atom, none}, "Atom example, with default"}]).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([{name, ?PROVIDER}, % The 'user friendly' name of the task
                          {module, ?MODULE}, % The module implementation of the task
                          {bare, true},      % The task can be run by the user, always true
                          {deps, ?DEPS},     % The list of dependencies
                          {example, "rebar3 rebar3_plugin"}, % How to use the plugin
                          {opts, ?OPTS},     % list of options understood by the plugin
                          {short_desc, "A rebar plugin"},
                          {desc, "A rebar plugin"}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    EdocOpts = rebar_state:get(State, edoc_opts, []),
    NewOpts =
        [{stylesheet, [code:priv_dir(rebar3_edoc_extensions), "/github-markdown.css"]},
         {xml_export, rebar3_edoc_style_export},
         {layout, rebar3_edoc_extensions_wrapper},
         {doclet, rebar3_edoc_extensions_wrapper}
         | EdocOpts],
    NesState = rebar_state:set(State, edoc_opts, NewOpts),
    rebar_prv_edoc:do(NesState).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================
