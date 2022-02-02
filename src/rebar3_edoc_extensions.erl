%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2021-2022 Viacheslav Katsuba.
%%

%%% @doc Main entry point for the rebar3 rebar3_edoc_extensions plugin.
-module(rebar3_edoc_extensions).

-export([init/1]).

-ignore_xref([init/1]).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_edoc_extensions_prv:init(State),
    {ok, State1}.
