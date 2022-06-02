%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2021-2022 VMware, Inc. or its affiliates.  All rights reserved.
%% Copyright (c) 2021-2022 Viacheslav Katsuba.
%%

%%% @doc Plugin provider for rebar3 rebar3_edoc_extensions.
-module(rebar3_edoc_extensions_prv).

-include("rebar3_edoc_extensions.hrl").

-export([init/1, do/1, format_error/1]).

-ignore_xref([do/1,
              format_error/1,
              {providers, create, 1},
              {rebar_state, add_provider, 2},
              {rebar_state, command_parsed_args, 1}]).

-if(?OTP_RELEASE >= 24).
-dialyzer({no_underspecs, [chain_edoc_backends/1,
                           chain_edoc_backends/2]}).
-else.
-dialyzer({nowarn_function, [chain_edoc_backends/1,
                             chain_edoc_backends/2]}).
-endif.

-define(PROVIDER, edoc).
-define(DEPS, [compile]).
-define(GITHUB_URL_BASE, "https://raw.githubusercontent.com").
%% The `++` is useless but it works around a bug in `rebar3 _format`.
-define(GITHUB_MARKDOWN_CSS_URL,
        ?GITHUB_URL_BASE ++ "/sindresorhus/github-markdown-css/gh-pages/github-markdown.css").
-define(PRISMJS_CORE_URL,
        ?GITHUB_URL_BASE ++ "/PrismJS/prism/$VERSION/components/prism-core.min.js").
-define(PRISMJS_THEME_URL,
        ?GITHUB_URL_BASE ++ "/PrismJS/prism/$VERSION/themes/prism.min.css").
-define(PRISMJS_DEFAULT_VERSION, "v1.26.0").
-define(PRISMJS_DEFAULT_THEME, "default").
-define(PRISMJS_DEFAULT_LANGS, ["erlang", "elixir"]).
-define(GENERATED_CSS, "edoc-extensions.css").
-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(INFO(Str, Args), rebar_log:log(info, Str, Args)).
-define(WARN(Str, Args), rebar_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), rebar_log:log(error, Str, Args)).

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
                          {example, "rebar3 edoc"}, % How to use the plugin
                          {opts, []},        % list of options understood by the plugin
                          {short_desc, "Override \"edoc\" command to improve EDoc documentation"},
                          {desc,
                           "The EDoc-generated documentation is improved "
                           "with overview ToC, more modern style and syntax highlighting"}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    EdocOpts = rebar_state:get(State, edoc_opts, []),
    DirOpt = proplists:get_value(dir, EdocOpts, "doc"),
    EdocOptsWithCSS = lists:keystore(
                        stylesheet, 1, EdocOpts,
                        {stylesheet, ?GENERATED_CSS}),

    ProjectApps = rebar_state:project_apps(State),
    PrismVersion = rebar_state:get(State, prismjs_version, ?PRISMJS_DEFAULT_VERSION),
    PrismTheme = rebar_state:get(State, prismjs_theme, ?PRISMJS_DEFAULT_THEME),
    PrismLangs = rebar_state:get(State, prismjs_languages, ?PRISMJS_DEFAULT_LANGS),
    Ret = lists:foldl(fun (AppInfo, ok) ->
                              AppName =
                                  rebar_utils:to_list(
                                      rebar_app_info:name(AppInfo)),
                              AppDir = rebar_app_info:dir(AppInfo),
                              DocDir = filename:join(AppDir, DirOpt),
                              try
                                  case file:make_dir(DocDir) of
                                      ok ->
                                          ok;
                                      {error, eexist} ->
                                          ok
                                  end,
                                  {ok, _} = application:ensure_all_started(ssl),
                                  {ok, _} = application:ensure_all_started(inets),
                                  download_github_markdown_css(AppName, DocDir),
                                  download_prismjs(AppName,
                                                   DocDir,
                                                   PrismVersion,
                                                   PrismTheme,
                                                   PrismLangs),

                                  ok = prepare_stylesheets(DocDir, EdocOpts),

                                  ok
                              catch
                                  _Class:Reason:_Stacktrace ->
                                      {app_failed, AppName, Reason}
                              end;
                          (_, Error) ->
                              Error
                      end,
                      ok,
                      ProjectApps),
    State1 =
        case Ret of
            ok ->
                EdocOpts1 = chain_edoc_backends(EdocOptsWithCSS),
                ?DEBUG("Overriden edoc options: ~p", [EdocOpts1]),
                rebar_state:set(State, edoc_opts, EdocOpts1);
            {app_failed, AppName, Reason} ->
                ?ERROR("Failed to fetch JS+CSS resources for ~ts: ~p", [AppName, Reason]),
                State
        end,
    rebar_prv_edoc:do(State1).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec download_github_markdown_css(AppName, DocDir) -> Ret
    when AppName :: string(),
         DocDir :: file:filename(),
         Ret :: ok.
download_github_markdown_css(AppName, DocDir) ->
    ?INFO("Downloading GitHub Markdown CSS for ~ts", [AppName]),
    Filename = filename:join(DocDir, "github-markdown.css"),
    case filelib:is_regular(Filename) of
        true ->
            ok;
        false ->
            ok = download(?GITHUB_MARKDOWN_CSS_URL, Filename)
    end.

-spec download_prismjs(AppName, DocDir, Version, Theme, Languages) -> Ret
    when AppName :: string(),
         DocDir :: file:filename(),
         Version :: string(),
         Theme :: string(),
         Languages :: [string()],
         Ret :: ok.
download_prismjs(AppName, DocDir, Version, Theme, Languages) when is_list(Languages) ->
    Urls0 =
        [prismjs_core_url(Version), prismjs_theme_url(Theme, Version)
         | [prismjs_language_url(Language, Version) || Language <- Languages]],
    Urls =
        [{Url,
          filename:join(DocDir, filename:basename(Url)) ++ "." ++ Version,
          filename:extension(Url)}
         || Url <- Urls0],
    ?INFO("Downloading PrismJS (~ts) for ~ts", [Version, AppName]),
    lists:foreach(fun({Url, Filename, _}) ->
                     case filelib:is_regular(Filename) of
                         true ->
                             ok;
                         false ->
                             ok = download(Url, Filename)
                     end
                  end,
                  Urls),

    ?DEBUG("Creating final prism.css", []),
    CssContent =
        [begin
             {ok, Binary} = file:read_file(Filename),
             Binary
         end
         || {_, Filename, ".css"} <- Urls],
    ConcatCssFile = filename:join(DocDir, "prism.css"),
    ok = file:write_file(ConcatCssFile, CssContent),

    ?DEBUG("Creating final prism.js", []),
    JsContent =
        [begin
             {ok, Binary} = file:read_file(Filename),
             Binary
         end
         || {_, Filename, ".js"} <- Urls],
    ConcatJsFile = filename:join(DocDir, "prism.js"),
    ok = file:write_file(ConcatJsFile, JsContent),
    ok.

-spec prismjs_core_url(Version) -> Url
    when Version :: string(),
         Url :: string().
prismjs_core_url(Version) ->
    prismjs_versioned_url(?PRISMJS_CORE_URL, Version).

-spec prismjs_theme_url(Theme, Version) -> Url
    when Theme :: string(),
         Version :: string(),
         Url :: string().
prismjs_theme_url("default", Version) ->
    prismjs_versioned_url(?PRISMJS_THEME_URL, Version);
prismjs_theme_url(Theme, Version) ->
    ReOpts = [{return, list}],
    Url = re:replace(?PRISMJS_THEME_URL,
                     "prism\\.min\\.css$",
                     "prism-" ++ Theme ++ ".min.css",
                     ReOpts),
    prismjs_versioned_url(Url, Version).

-spec prismjs_language_url(Language, Version) -> Url
    when Language :: string(),
         Version :: string(),
         Url :: string().
prismjs_language_url(Language, Version) ->
    ReOpts = [{return, list}],
    Url = re:replace(?PRISMJS_CORE_URL,
                     "prism-core\\.min\\.js$",
                     "prism-" ++ Language ++ ".min.js",
                     ReOpts),
    prismjs_versioned_url(Url, Version).

-spec prismjs_versioned_url(Url, Version) -> Url
    when Url :: string(),
         Version :: string().
prismjs_versioned_url(Url, Version) ->
    ReOpts = [{return, list}],
    re:replace(Url, "\\$VERSION", Version, ReOpts).

-spec download(Url, Filename) -> Ret
    when Url :: string(),
         Filename :: file:filename(),
         Ret :: ok.
download(Url, Filename) ->
    ?DEBUG("Downloading \"~ts\" and saving to \"~ts\"", [Url, Filename]),
    HTTPOptions = [{ssl, [{verify, verify_none}]}],
    Options = [{stream, Filename}],
    {ok, saved_to_file} = httpc:request(get, {Url, []}, HTTPOptions, Options, default),
    ok.

-spec prepare_stylesheets(DocDir, EdocOpts) -> ok when
      DocDir :: file:filename(),
      EdocOpts :: [tuple()].
prepare_stylesheets(DocDir, EdocOpts) ->
    %% We first want to import the two CSS files we downloaded. We then import
    %% the user-provided CSS file, if any.
    Stylesheets0 = ["github-markdown.css",
                    "prism.css"],
    Stylesheets = case proplists:get_value(stylesheet, EdocOpts) of
                      undefined  -> Stylesheets0;
                      Stylesheet -> Stylesheets0 ++ [Stylesheet]
                  end,
    generate_wrapping_css(DocDir, Stylesheets).

-spec generate_wrapping_css(DocDir, Stylesheets) -> ok when
      DocDir :: file:filename(),
      Stylesheets :: [string()].
generate_wrapping_css(DocDir, Stylesheets) ->
    %% We generate a CSS file to import everything (including the
    %% user-specified stylesheet) and write it to the doc directory. This
    %% generated CSS file is the one passed in EDoc options.
    Imports = [io_lib:format("@import url(\"~ts\");~n", [Url])
               || Url <- Stylesheets],
    Content = Imports ++ "\n" ?ADDITIONAL_STYLE "\n",
    Filename = filename:join(DocDir, ?GENERATED_CSS),
    ok = file:write_file(Filename, Content),
    ok.

-spec chain_edoc_backends(EdocOpts) -> EdocOpts when
      EdocOpts :: [tuple()].
chain_edoc_backends(EdocOpts) ->
    Options = [xml_export,
               layout,
               doclet],
    chain_edoc_backends(Options, EdocOpts).

-spec chain_edoc_backends(Options, EdocOpts) -> EdocOpts when
      Options :: [xml_export | doclet | layout],
      EdocOpts :: [tuple()].
chain_edoc_backends([xml_export | Rest], EdocOpts) ->
    %% For `xml_export', we override the module, regardless of what the user
    %% provided. I don't know if we can support module chaining here as there
    %% is no options passed to the module functions. Perhaps we could use a
    %% `persistent_term', but anyway, I don't have a use case currently to
    %% test this.
    EdocOpts1 = lists:keystore(
                  xml_export, 1, EdocOpts,
                  {xml_export, rebar3_edoc_extensions_export}),
    chain_edoc_backends(Rest, EdocOpts1);
chain_edoc_backends([Option | Rest], EdocOpts)
  when Option =:= doclet orelse Option =:= layout ->
    %% For d`doclet' and `layout', we override the user-configured module by
    %% our own. However, we store the former in another option. When
    %% processing the documentation, our own module will called the
    %% user-configured module and patch its output.
    %%
    %% If there is no user-configured module, we default to `edoc_doclet' and
    %% `edoc_layout'.
    DefaultMod = list_to_atom(io_lib:format("edoc_~s", [Option])),
    ChainedMod = proplists:get_value(Option, EdocOpts, DefaultMod),
    ChainedOption = list_to_atom(
                      io_lib:format("chained_~s", [Option])),
    EdocOpts1 = lists:keystore(
                  Option, 1, EdocOpts,
                  {ChainedOption, ChainedMod}),

    EdocOpts2 = lists:keystore(
                  Option, 1, EdocOpts1,
                  {Option, rebar3_edoc_extensions_wrapper}),
    chain_edoc_backends(Rest, EdocOpts2);
chain_edoc_backends([], EdocOpts) ->
    EdocOpts.
