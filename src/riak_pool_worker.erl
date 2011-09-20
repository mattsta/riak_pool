-module(riak_pool_worker).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {client,
                started = now()}).

%%%----------------------------------------------------------------------
%%% Init
%%%----------------------------------------------------------------------
start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

init(Args) ->
  process_flag(trap_exit, true),
  Host = proplists:get_value(host, Args),
  Port = proplists:get_value(port, Args),
  case riakc_pb_socket:start_link(Host, Port) of
  {ok, Pid} -> {ok, #state{client = Pid}};
      Error -> {error, Error}
  end.

%%%----------------------------------------------------------------------
%%% Requests
%%%----------------------------------------------------------------------
handle_call({What, Args}, _From, #state{client=Client}=State) ->
  {reply, apply(riakc_pb_socket, What, [Client | Args]), State};

handle_call(_Request, _From, State) ->
  {reply, {?MODULE, not_implemented}, State}.


handle_cast(_Msg, State) ->
  {noreply, State}.

%%%----------------------------------------------------------------------
%%% Shutdown
%%%----------------------------------------------------------------------
handle_info(stop, State) ->
  {stop, shutdown, State};
handle_info({'EXIT', _, _}, State) ->
  {stop, shutdown, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{client=Client}) ->
  riakc_pb_socket:stop(Client).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
