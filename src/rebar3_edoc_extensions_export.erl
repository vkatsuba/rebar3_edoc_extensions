%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2021-2022 VMware, Inc. or its affiliates.  All rights reserved.
%% Copyright (c) 2021-2022 Viacheslav Katsuba.
%%

-module(rebar3_edoc_extensions_export).

-include_lib("xmerl/include/xmerl.hrl").

-include("rebar3_edoc_extensions.hrl").

-export(['#xml-inheritance#'/0]).

-export(['#root#'/4, '#element#'/5, '#text#'/1, '#cdata#'/1]).

-spec '#xml-inheritance#'() -> [].
'#xml-inheritance#'() -> [].

-spec '#text#'(term()) -> term().
'#text#'(Text) ->
    xmerl_html:'#text#'(Text).

-spec '#cdata#'(term()) -> term().
'#cdata#'(Text) ->
    xmerl_html:'#cdata#'(Text).

-spec '#root#'(term(), term(), [], term()) -> term().
'#root#'(Data, Attrs, [], E) ->
    xmerl_html:'#root#'(Data, Attrs, [], E).

-spec '#element#'(term(), term(), term(), term(), term()) -> term().
'#element#'(head = Tag, Data, Attrs, Parents, E) ->
    %% FIXME: We don't have access to EDoc options here. Therefore we assume
    %% that the EDoc directory is `doc' in the current working directory...
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(Cwd, "doc"),
    AddonFile = filename:join(Dir, ?HEAD_ADDON_FILENAME),
    Data1 = case file:read_file(AddonFile) of
                {ok, Addon} -> [Data, Addon];
                _           -> Data
            end,
    xmerl_html:'#element#'(Tag, Data1, Attrs, Parents, E);
'#element#'(body = Tag, Data, _Attrs, Parents, E) ->
    Data1 = [?SYNTAX_HIGHLIGHTING_JS,
             Data],
    Attrs = [#xmlAttribute{name = class,
                           value = ?BODY_CLASSES}],
    xmerl_html:'#element#'(Tag, Data1, Attrs, Parents, E);
'#element#'(pre = Tag, Data, Attrs, Parents, E) ->
    ReOpts = [{capture, all_but_first, list}, dotall],
    case re:run(Data, "^ +(" ?LANG_REGEX ")(?:\n)(.*)", ReOpts) of
        {match, [Lang, Data0]} ->
            Data1 = re:replace(Data0, "^  ", "", [global, multiline]),
            Data2 = ["<code class=\"language-" ++ Lang ++ "\">",
                     Data1,
                     "</code>"],
            xmerl_html:'#element#'(Tag, Data2, Attrs, Parents, E);
        nomatch ->
            Data1 = re:replace(Data, "^  ", "", [global, multiline]),
            Data2 = ["<code>",
                     Data1,
                     "</code>"],
            xmerl_html:'#element#'(Tag, Data2, Attrs, Parents, E)
    end;
'#element#'(tt, Data, Attrs, Parents, E) ->
    xmerl_html:'#element#'(code, Data, Attrs, Parents, E);
'#element#'(Tag, Data, Attrs, Parents, E) ->
    xmerl_html:'#element#'(Tag, Data, Attrs, Parents, E).
