-module(rebar3_edoc_extensions_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl"). % Assertion macros for convenience

-export([all/0, groups/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_app/1, command/1]).

-type rebar_dict() :: dict:dict().

-record(state_t, {dir                               :: file:name(),
                  opts                = dict:new()  :: rebar_dict(),
                  code_paths          = dict:new()  :: rebar_dict(),
                  default             = dict:new()  :: rebar_dict(),
                  escript_path                      :: undefined | file:filename_all(),

                  lock                = [],
                  current_profiles    = [default]   :: [atom()],
                  namespace           = default     :: atom(),

                  command_args        = [],
                  command_parsed_args = {[], []},

                  current_app                       :: undefined | rebar_app_info:t(),
                  project_apps        = []          :: [rebar_app_info:t()],
                  deps_to_build       = []          :: [rebar_app_info:t()],
                  all_plugin_deps     = []          :: [rebar_app_info:t()],
                  all_deps            = []          :: [rebar_app_info:t()],

                  compilers           = []          :: [module()],
                  project_builders    = []          :: [{rebar_app_info:project_type(), module()}],
                  resources           = [],
                  providers           = [],
                  allow_provider_overrides = false  :: boolean()}).

-spec all() -> list().
all() ->
    [test_app,
     command].

-spec groups() -> list().
groups() ->
    [].

-spec init_per_testcase(atom(), list()) ->
                           list() | {fail, term()} | {skip, term()}.
init_per_testcase(_Name, Config) ->
    Config.

-spec end_per_testcase(atom(), list()) ->
                          term() | {fail, term()} | {save_config, list()}.
end_per_testcase(_Name, _Config) ->
    ok.

%% =============================================================================
%% Test Cases
%% =============================================================================

-spec test_app(list()) -> ok | no_return().
test_app(_Config) ->
    ok = file:set_cwd("../../../../test"),
    State = init_test_app(),
    {Res, _} = edoc(State),
    ?assertEqual(ok, Res),

    {ok, Bin} = file:read_file("test_app/doc/test.html"),
    ?assertEqual(true, filelib:is_file("test_app/doc/edoc-extensions.css")),
    ?assertEqual(true, filelib:is_file("test_app/doc/prism.js")),
    ?assertEqual(true, filelib:is_file("test_app/doc/prism.css")),
    ?assertEqual(true, filelib:is_file("test_app/doc/github-markdown.css")),
    ?assertEqual(true, nomatch /= re:run(Bin, "language-erlang")),
    ?assertEqual(true, nomatch /= re:run(Bin, "edoc-extensions.css")),

    {ok, Overview} = file:read_file("test_app/doc/overview-summary.html"),
    ?assertEqual(true, nomatch /= re:run(Overview, "Added to head element")),
    ?assertEqual(true, nomatch /= re:run(Overview, "Some Unicode")).

-spec command(list()) -> ok | no_return().
command(_Config) ->
    ok = file:set_cwd("../../../../test"),
    {ok, State1} = init(),
    State2 = rebar_state:set(State1, edoc, []),
    {Res, _} = command(State2, [compile, edoc]),
    ?assertEqual(ok, Res).

-spec command(any(), any()) -> any().
command(State, Commands) ->
    rebar3_edoc_extensions_prv:do(
        rebar_state:command_parsed_args(State, {Commands, something})).

%% =============================================================================
%% Helpers
%% =============================================================================

-spec edoc(any()) -> any().
edoc(State) ->
    rebar3_edoc_extensions_prv:do(State).

-spec init() -> any().
init() ->
    rebar3_edoc_extensions:init(rebar_state:new()).

-spec init_test_app() -> any().
init_test_app() ->
    {ok, State0} = init(),
    State1 = rebar_state:set(State0, prismjs_version, "v1.26.0"),
    State2 = rebar_state:set(State1, prismjs_theme, "twilight"),
    State3 = rebar_state:set(State2, prismjs_languages, ["erlang", "elixir"]),
    {ok, TestAppInfo} = rebar_app_info:new(test_app, git, "test_app"),
    State3#state_t{current_app = TestAppInfo, project_apps = [TestAppInfo]}.
