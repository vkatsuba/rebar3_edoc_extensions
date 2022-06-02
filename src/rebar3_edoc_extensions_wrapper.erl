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
    LayoutMod = get_chained_mod(layout, Options),
    LayoutMod:module(Element, Options).

-spec overview(term(), list()) -> [binary() | list()].
overview(Element, Options) ->
    LayoutMod = get_chained_mod(layout, Options),
    Overview = LayoutMod:overview(Element, Options),
    patch_html(Overview).

-spec run(#doclet_gen{}, #?RECORD{}) -> ok | no_return().
run(#doclet_gen{app = App} = Cmd, #?RECORD{dir = Dir} = Ctxt) ->
    DocletMod = get_chained_mod(doclet, Ctxt),
    ok = DocletMod:run(Cmd, Ctxt),
    File = filename:join(Dir, "modules-frame.html"),
    {ok, Content0} = file:read_file(File),
    Content1 = add_toc(App, Content0, Dir),
    Content2 = patch_html(Content1),
    case file:write_file(File, Content2) of
        ok              -> ok;
        {error, Reason} -> exit({error, Reason})
    end.

-spec get_chained_mod(Option, Options | Ctxt) -> Value when
      Option :: doclet | layout,
      Options :: [tuple()],
      Ctxt :: #?RECORD{},
      Value :: any().
get_chained_mod(doclet, Options) when is_list(Options) ->
    proplists:get_value(chained_doclet, Options);
get_chained_mod(layout, Options) when is_list(Options) ->
    proplists:get_value(chained_layout, Options);
get_chained_mod(Option, Ctxt) when is_tuple(Ctxt) ->
    #?RECORD{opts = Options} = Ctxt,
    get_chained_mod(Option, Options).

-spec patch_html(list()) -> list().
patch_html(Html) ->
    Html2 = re:replace(
                  Html,
                  "<body +bgcolor=\"[^\"]*\">",
                  "<body class=\"" ?BODY_CLASSES "\">\n"
                  ?SYNTAX_HIGHLIGHTING_JS,
                  [{return, list}]),
    Html3 = re:replace(
                  Html2,
                  "<pre>(.*)</pre>",
                  "<pre><code>\\1</code></pre>",
                  [{return, list}, ungreedy, dotall, global]),
    Html4 = re:replace(
                  Html3,
                  "<pre><code> +(" ?LANG_REGEX ")(?:\n)(.*)</code></pre>",
                  "<pre><code class=\"language-\\1\">\\2</code></pre>",
                  [{return, list}, ungreedy, dotall, global]),
    Html4.

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
              [{return, list}])
    end.

-spec generate_toc([binary() | list()], term()) -> [binary() | list()] | undefined.
generate_toc(Dir, App) ->
    case get_overview(Dir) of
        undefined ->
            undefined;
        Overview ->
            Lines = re:split(Overview, "\\n", [{return, list}]),
            Titles = [Line ||
                      Line <- Lines,
                      match =:= re:run(Line, "^=+.*=+$", [{capture, none}])],
            generate_toc1(Titles, 0, [], App)
    end.

-spec generate_toc1([binary() | list()], integer(), list(), term()) -> [binary() | list()].
generate_toc1([Title | Rest], CurrentLevel, Result, App) ->
    ReOpts = [{capture, all_but_first, list}],
    {match, [Equals, Title1]} = re:run(Title, "^(=+) *(.*)", ReOpts),
    Title2 = re:replace(Title1, " *=+$", "", [{return, list}]),
    Anchor = "#" ++ re:replace(Title2, " ", "_", [{return, list}, global]),
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

-spec get_overview(list()) -> binary() | undefined.
get_overview(Dir) ->
    OverviewFile = filename:join(Dir, "overview.edoc"),
    case file:read_file(OverviewFile) of
        {ok, Overview} ->
            Overview;
        {error, _} ->
            undefined
    end.
