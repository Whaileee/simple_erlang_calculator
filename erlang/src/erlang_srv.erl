-module(erlang_srv).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, service/0, calculate/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) ->
    io:format("Started"),
    {ok, started}.
handle_call({calculate, Args}, _From, _OldState) ->
    Result = handle_calculate(Args),
    {reply, Result, ok};
handle_call(Args, From, OldState) ->
    io:format("A:~p~nB:~p~nC:~p~n",[Args,From,OldState]),
    io:format("Received call"),
    {reply, "odpowiedz", ok}.

handle_cast(Args,B) ->
    io:format("Received cast"),
    {noreply, ok}.

service() ->
    gen_server:call(?MODULE, [argument]).

calculate(A, B, Operator) ->
    Args = {A,B,Operator},
    gen_server:call(?MODULE, {calculate, Args}).

handle_calculate({A, B, '+'}) ->
    A + B;
handle_calculate({A, B, '-'}) ->
    A - B;
handle_calculate({A, B, '*'}) ->
    A * B;
handle_calculate({A, B, '/'}) ->  
    A / B.
