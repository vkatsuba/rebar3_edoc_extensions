-module(rebar3_edoc_extensions_export).

-include_lib("xmerl/include/xmerl.hrl").

-include("rebar3_edoc_extensions.hrl").

-export(['#xml-inheritance#'/0]).

-export(['#root#'/4, '#element#'/5, '#text#'/1]).

-spec '#xml-inheritance#'() -> list().
'#xml-inheritance#'() -> [].

-spec '#text#'(term()) -> term().
'#text#'(Text) ->
    xmerl_html:'#text#'(Text).

-spec '#root#'(term(), term(), term(), term()) -> term().
'#root#'(Data, Attrs, [], E) ->
    xmerl_html:'#root#'(Data, Attrs, [], E).

-spec '#element#'(term(), term(), term(), term(), term()) -> term().
'#element#'(head = Tag, Data, Attrs, Parents, E) ->
    Data1 = [Data,
             ?SYNTAX_HIGHLIGHTING_CSS,
             ?ADDITIONAL_STYLE],
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