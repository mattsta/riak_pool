-module(riak_pool_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, Config} = application:get_env(riak_pool, pools),
  Processes = generate_workers(Config),
  Strategy = {one_for_one, 10, 10},
  {ok,
   {Strategy, lists:flatten(Processes)}}.

generate_workers(AggregateConfig) ->
  [config_to_spec(Conf) || Conf <- AggregateConfig].

config_to_spec({PoolName, PoolConfig}) ->
  Args = [{name, {local, PoolName}},
          {worker_module, riak_pool_worker} | PoolConfig],
  {PoolName, {poolboy, start_link, [Args]},
   permanent, 5000, worker, [poolboy]}.
