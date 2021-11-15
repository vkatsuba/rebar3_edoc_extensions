%%% @doc Main entry point for the rebar3 rebar3_edoc_style plugin.
-module(rebar3_edoc_style).

-export([init/1]).

-ignore_xref([init/1]).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_edoc_style_prv:init(State),
    {ok, State1}.
