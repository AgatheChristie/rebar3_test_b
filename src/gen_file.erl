-module(gen_file).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = gen_file_prv:init(State),
    {ok, State1}.

-include_lib("kernel/include/file.hrl").


