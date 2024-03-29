%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2021-2022 VMware, Inc. or its affiliates.  All rights reserved.
%% Copyright (c) 2021-2022 Viacheslav Katsuba.
%%

-module(rebar3_edoc_extensions_wrapper).

-include_lib("edoc/include/edoc_doclet.hrl").

%% doclet's Ctxt is a #context{} record in Erlang 23 and #doclet_context{} in
%% Erlang 24+.
-if(?OTP_RELEASE >= 24).
-define(RECORD, doclet_context).
-else.
-define(RECORD, context).
-endif.

-include("rebar3_edoc_extensions.hrl").

%% `edoc_layout` API.
-export([module/2, overview/2]).
%% `edoc_doclet` API.
-export([run/2]).

-spec module(term(), list()) -> term().
module(Element, Options) ->
    edoc_layout:module(Element, Options).

-spec overview(term(), list()) -> [binary() | list()].
overview(Element, Options) ->
    Dir = proplists:get_value(dir, Options),
    Overview = edoc_layout:overview(Element, Options),
    patch_html(Dir, Overview).

-spec run(#doclet_gen{}, #?RECORD{}) -> ok | no_return().
run(#doclet_gen{app = App} = Cmd, #?RECORD{dir = Dir} = Ctxt) ->
    ok = edoc_doclet:run(Cmd, Ctxt),
    ok = patch_index(Dir),
    ok = patch_modules_frame(App, Dir).

-spec patch_index(Dir) -> ok when
      Dir :: file:filename().
patch_index(Dir) ->
    File = filename:join(Dir, "index.html"),
    {ok, Content0} = file:read_file(File),
    Content1 = patch_html(Dir, Content0),
    case file:write_file(File, Content1) of
        ok              -> ok;
        {error, Reason} -> exit({error, Reason})
    end.

-spec patch_modules_frame(App, Dir) -> ok when
      App :: atom(),
      Dir :: file:filename().
patch_modules_frame(App, Dir) ->
    File = filename:join(Dir, "modules-frame.html"),
    {ok, Content0} = file:read_file(File),
    Content1 = add_toc(App, Content0, Dir),
    Content2 = patch_html(Dir, Content1),
    case file:write_file(File, Content2) of
        ok              -> ok;
        {error, Reason} -> exit({error, Reason})
    end.

-spec patch_html(file:filename(), list() | binary()) -> list().
patch_html(Dir, Html) ->
    Html1 = add_head_addon(Dir, Html),
    Html2 = re:replace(
                  Html1,
                  "<body +bgcolor=\"[^\"]*\">",
                  "<body class=\"" ?BODY_CLASSES "\">\n"
                  ?SYNTAX_HIGHLIGHTING_JS,
                  [{return, list}, unicode]),
    Html3 = re:replace(
                  Html2,
                  "<pre>(.*)</pre>",
                  "<pre><code>\\1</code></pre>",
                  [{return, list}, unicode, ungreedy, dotall, global]),
    Html4 = re:replace(
                  Html3,
                  "<pre><code> +(" ?LANG_REGEX ")(?:\n)(.*)</code></pre>",
                  "<pre><code class=\"language-\\1\">\\2</code></pre>",
                  [{return, list}, unicode, ungreedy, dotall, global]),
    Html4.

-spec add_head_addon(Dir, Html) -> Html when
      Dir :: file:filename(),
      Html :: list() | binary().
add_head_addon(Dir, Html) ->
    AddonFile = filename:join(Dir, ?HEAD_ADDON_FILENAME),
    case file:read_file(AddonFile) of
        {ok, Addon} ->
            re:replace(
              Html,
              "</head>",
              [Addon, "</head>"],
              [{return, list}, unicode]);
        _ ->
            Html
    end.

-spec add_toc(term(), binary(), list()) -> list().
add_toc(App, Html, Dir) ->
    case generate_toc(Dir, App) of
        undefined ->
            Html;
        Toc  ->
            re:replace(
              Html,
              "(<h2 class=\"indextitle\">Modules</h2>)",
              Toc ++ "\\1",
              [{return, list}, unicode])
    end.

-spec generate_toc([binary() | list()], term()) -> [binary() | list()] | undefined.
generate_toc(Dir, App) ->
    case get_overview(Dir) of
        undefined ->
            undefined;
        Overview ->
            Lines = re:split(Overview, "\\n", [{return, list}, unicode]),
            Titles = [Line ||
                      Line <- Lines,
                      match =:= re:run(Line, "^=+.*=+$",
                                       [{capture, none}, unicode])],
            generate_toc1(Titles, 0, [], App)
    end.

-spec generate_toc1([binary() | list()], integer(), list(), term()) -> [binary() | list()].
generate_toc1([Title | Rest], CurrentLevel, Result, App) ->
    ReOpts = [{capture, all_but_first, list}, unicode],
    {match, [Equals, Title1]} = re:run(Title, "^(=+) *(.*)", ReOpts),
    Title2 = re:replace(Title1, " *=+$", "", [{return, list}, unicode]),
    Anchor = "#" ++ re:replace(
                      Title2, " ", "_", [{return, list}, unicode, global]),
    Link = "<a href=\"overview-summary.html" ++ Anchor ++ "\"" ++
      "target=\"overviewFrame\">" ++ Title2 ++ "</a>",
    Level = length(Equals) - 1,
    if
        Level =:= CurrentLevel ->
            Result1 = Result ++ ["</li>\n", "<li>", Link],
            generate_toc1(Rest, CurrentLevel, Result1, App);

        CurrentLevel =:= 0 andalso
        Level =:= CurrentLevel + 1  ->
            OverviewLink = "<a href=\"overview-summary.html\"" ++
            "target=\"overviewFrame\">Overview</a>",
            Result1 = Result ++ ["<ul>\n",
                                 "<li>", OverviewLink, "</li>\n",
                                 "<li>", Link],
            generate_toc1(Rest, Level, Result1, App);

        Level =:= CurrentLevel + 1 ->
            Result1 = Result ++ ["\n", "<ul>\n", "<li>", Link],
            generate_toc1(Rest, Level, Result1, App);

        Level < CurrentLevel ->
            Result1 = Result ++ ["</li>\n"
                                 "</ul>\n"
                                 || _ <- lists:seq(Level, CurrentLevel - 1)],
            Result2 = Result1 ++ ["</li>\n", "<li>", Link],
            generate_toc1(Rest, Level, Result2, App)
    end;
generate_toc1([], CurrentLevel, Result, App) ->
    Name = atom_to_list(App),
    ["<h2 class=\"indextitle\">", Name, "</h2>\n",
     Result,
     "</li>\n",
     ["</ul>\n" || _ <- lists:seq(0, CurrentLevel - 1)]].

-spec get_overview(file:filename()) -> binary() | undefined.
get_overview(Dir) ->
    OverviewFile = filename:join(Dir, "overview.edoc"),
    case file:read_file(OverviewFile) of
        {ok, Overview} ->
            Overview;
        {error, _} ->
            undefined
    end.
