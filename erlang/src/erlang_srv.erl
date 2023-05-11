-module(erlang_srv).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, service/3, calculate/3, request/3,
         handle_request/1]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  io:format("Started~n"),
  inets:start(httpd,
              [{modules,
                [mod_alias,
                 mod_auth,
                 mod_esi,
                 mod_actions,
                 mod_cgi,
                 mod_dir,
                 mod_get,
                 mod_head,
                 mod_log,
                 mod_disk_log]},
               {port, 8001},
               {server_name, "localhost"},
               {server_root, "./"},
               {document_root, "./htdocs"},
               {directory_index, ["index.html"]},
               {erl_script_alias, {"/test", [erlang_srv]}},
               {bind_address, "localhost"},
               {mime_types,
                [{"html", "text/html"}, {"css", "text/css"}, {"js", "application/x-javascript"}]}]),
  {ok, started}.

handle_call({calculate, Args}, _From, _OldState) ->
  Result = handle_calculate(Args),
  {reply, Result, ok};
handle_call({handle_request, {Input}}, _From, _OldState) ->
  Result = handle_request(Input),
  {reply, Result, ok};
handle_call(Args, From, OldState) ->
  io:format("A:~p~nB:~p~nC:~p~n", [Args, From, OldState]),
  io:format("Received call"),
  {reply, "odpowiedz", ok}.

handle_cast(Args, B) ->
  io:format("Received cast"),
  {noreply, ok}.

service(SessionID, _Env, _Input) ->
  mod_esi:deliver(SessionID,
                  ["Content-Type: text/html\r\n\r\n", "<html><body>Hello, world</body></html>"]).

request(SessionID, Env, Input) ->
  io:format("~p", [Env]),
  Method = proplists:get_value(request_method, Env),
  case Method of
    "GET" ->
      Response = gen_server:call(?MODULE, {handle_request, {Input}}),
      mod_esi:deliver(SessionID, ["Content-Type: application/json\r\n\r\n", Response]);
    _ ->
      Response = jsone:encode(#{<<"error">> => <<"invalid method">>}),
      mod_esi:deliver(SessionID,
                      ["Content-Type: application/json \r\n status: 400 Bad Request\r\n\r\n", Response])
  end.

calculate(A, B, Operator) ->
  Args = {A, B, Operator},
  gen_server:call(?MODULE, {calculate, Args}).

handle_calculate({A, B, "add"}) ->
  integer_to_binary(A + B);
handle_calculate({A, B, "sub"}) ->
  integer_to_binary(A - B);
handle_calculate({A, B, "mult"}) ->
  integer_to_binary(A * B);
handle_calculate({A, B, "div"}) ->
  float_to_binary(A / B).

handle_request(Params) ->
  Args = string:split(Params, ",", all),
  Values = get_values_to_calculate(Args),
  A = list_to_integer(proplists:get_value(a, Values)),
  B = list_to_integer(proplists:get_value(b, Values)),
  Op = proplists:get_value(op, Values),
  Result = handle_calculate({A, B, Op}),
  io:format("Result: ~p", [Result]),
  jsone:encode(#{<<"result">> => Result}).

get_values_to_calculate([]) ->
  [];
get_values_to_calculate([H | T]) ->
  [Key, Value] = string:split(H, "="),
  Acc = [{list_to_atom(Key), Value}],
  get_values_to_calculate(T, Acc).

get_values_to_calculate([], Acc) ->
  Acc;
get_values_to_calculate([H | T], Acc) ->
  [Key, Value] = string:split(H, "="),
  Acc2 = Acc ++ [{list_to_atom(Key), Value}],
  get_values_to_calculate(T, Acc2).
