-module(riak_pool).

-export([% set_options/2, set_options/3,
         is_connected/1, is_connected/2,
         ping/1, ping/2,
         % get_client_id/1, get_client_id/2,
         % set_client_id/2, set_client_id/3,
         get_server_info/1, get_server_info/2,
         get/3, get/4, get/5,
         put/2, put/3, put/4,
         delete/3, delete/4, delete/5,
         delete_vclock/4, delete_vclock/5, delete_vclock/6,
         delete_obj/2, delete_obj/3, delete_obj/4,
         list_buckets/1, list_buckets/2, list_buckets/3,
         list_keys/2, list_keys/3,
%         stream_list_keys/2, stream_list_keys/3, stream_list_keys/4,
         get_bucket/2, get_bucket/3, get_bucket/4,
         set_bucket/3, set_bucket/4, set_bucket/5,
         mapred/3, mapred/4, mapred/5,
         mapred_stream/4, mapred_stream/5, mapred_stream/6,
         mapred_bucket/3, mapred_bucket/4, mapred_bucket/5,
         mapred_bucket_stream/5, mapred_bucket_stream/6,
         search/3, search/5, search/6,
         get_index/4, get_index/5, get_index/6, get_index/7
         % default_timeout/1
       ]).

-define(WORKER_POOL(PoolName, Function, Args),
          Worker = poolboy:checkout(PoolName),
          Reply = gen_server:call(Worker, {Function, Args}),
          poolboy:checkin(PoolName, Worker),
          Reply
       ).

%%%----------------------------------------------------------------------
%%% Riak Calls (can you say auto-generated-code?)
%%%----------------------------------------------------------------------
% set_options is a per-connection setting.  doesn't make sense in a pool.
%set_options(PoolName, A) ->
%  ?WORKER_POOL(PoolName, set_options, [A]).
%set_options(PoolName, A, B) ->
%  ?WORKER_POOL(PoolName, set_options, [A, B]).

is_connected(PoolName) ->
  ?WORKER_POOL(PoolName, is_connected, []).
is_connected(PoolName, A) ->
  ?WORKER_POOL(PoolName, is_connected, [A]).

ping(PoolName) ->
  ?WORKER_POOL(PoolName, ping, []).
ping(PoolName, A) ->
  ?WORKER_POOL(PoolName, ping, [A]).

% get/set client id doesn't make sense for random pool workers
%get_client_id(PoolName) ->
%  ?WORKER_POOL(PoolName, get_client_id, []).
%get_client_id(PoolName, A) ->
%  ?WORKER_POOL(PoolName, get_client_id, [A]).


%set_client_id(PoolName, A) ->
%  ?WORKER_POOL(PoolName, set_client_id, [A]).
%set_client_id(PoolName, A, B) ->
%  ?WORKER_POOL(PoolName, set_client_id, [A, B]).


get_server_info(PoolName) ->
  ?WORKER_POOL(PoolName, get_server_info, []).
get_server_info(PoolName, A) ->
  ?WORKER_POOL(PoolName, get_server_info, [A]).


get(PoolName, A, B) ->
  ?WORKER_POOL(PoolName, get, [A, B]).
get(PoolName, A, B, C) ->
  ?WORKER_POOL(PoolName, get, [A, B, C]).
get(PoolName, A, B, C, D) ->
  ?WORKER_POOL(PoolName, get, [A, B, C, D]).


put(PoolName, A) ->
  ?WORKER_POOL(PoolName, put, [A]).
put(PoolName, A, B) ->
  ?WORKER_POOL(PoolName, put, [A, B]).
put(PoolName, A, B, C) ->
  ?WORKER_POOL(PoolName, put, [A, B, C]).


delete(PoolName, A, B) ->
  ?WORKER_POOL(PoolName, delete, [A, B]).
delete(PoolName, A, B, C) ->
  ?WORKER_POOL(PoolName, delete, [A, B, C]).
delete(PoolName, A, B, C, D) ->
  ?WORKER_POOL(PoolName, delete, [A, B, C, D]).


delete_vclock(PoolName, A, B, C) ->
  ?WORKER_POOL(PoolName, delete_vclock, [A, B, C]).
delete_vclock(PoolName, A, B, C, D) ->
  ?WORKER_POOL(PoolName, delete_vclock, [A, B, C, D]).
delete_vclock(PoolName, A, B, C, D, E) ->
  ?WORKER_POOL(PoolName, delete_vclock, [A, B, C, D, E]).


delete_obj(PoolName, A) ->
  ?WORKER_POOL(PoolName, delete_obj, [A]).
delete_obj(PoolName, A, B) ->
  ?WORKER_POOL(PoolName, delete_obj, [A, B]).
delete_obj(PoolName, A, B, C) ->
  ?WORKER_POOL(PoolName, delete_obj, [A, B, C]).


list_buckets(PoolName) ->
  ?WORKER_POOL(PoolName, list_buckets, []).
list_buckets(PoolName, A) ->
  ?WORKER_POOL(PoolName, list_buckets, [A]).
list_buckets(PoolName, A, B) ->
  ?WORKER_POOL(PoolName, list_buckets, [A, B]).


list_keys(PoolName, A) ->
  ?WORKER_POOL(PoolName, list_keys, [A]).
list_keys(PoolName, A, B) ->
  ?WORKER_POOL(PoolName, list_keys, [A, B]).


% Worker doesn't support receiving streaming ops
%stream_list_keys(PoolName, A) ->
%  ?WORKER_POOL(PoolName, stream_list_keys, [A]).
%stream_list_keys(PoolName, A, B) ->
%  ?WORKER_POOL(PoolName, stream_list_keys, [A, B]).
%stream_list_keys(PoolName, A, B, C) ->
%  ?WORKER_POOL(PoolName, stream_list_keys, [A, B, C]).


get_bucket(PoolName, A) ->
  ?WORKER_POOL(PoolName, get_bucket, [A]).
get_bucket(PoolName, A, B) ->
  ?WORKER_POOL(PoolName, get_bucket, [A, B]).
get_bucket(PoolName, A, B, C) ->
  ?WORKER_POOL(PoolName, get_bucket, [A, B, C]).


set_bucket(PoolName, A, B) ->
  ?WORKER_POOL(PoolName, set_bucket, [A, B]).
set_bucket(PoolName, A, B, C) ->
  ?WORKER_POOL(PoolName, set_bucket, [A, B, C]).
set_bucket(PoolName, A, B, C, D) ->
  ?WORKER_POOL(PoolName, set_bucket, [A, B, C, D]).


mapred(PoolName, A, B) ->
  ?WORKER_POOL(PoolName, mapred, [A, B]).
mapred(PoolName, A, B, C) ->
  ?WORKER_POOL(PoolName, mapred, [A, B, C]).
mapred(PoolName, A, B, C, D) ->
  ?WORKER_POOL(PoolName, mapred, [A, B, C, D]).


mapred_stream(PoolName, A, B, C) ->
  ?WORKER_POOL(PoolName, mapred_stream, [A, B, C]).
mapred_stream(PoolName, A, B, C, D) ->
  ?WORKER_POOL(PoolName, mapred_stream, [A, B, C, D]).
mapred_stream(PoolName, A, B, C, D, E) ->
  ?WORKER_POOL(PoolName, mapred_stream, [A, B, C, D, E]).


mapred_bucket(PoolName, A, B) ->
  ?WORKER_POOL(PoolName, mapred_bucket, [A, B]).
mapred_bucket(PoolName, A, B, C) ->
  ?WORKER_POOL(PoolName, mapred_bucket, [A, B, C]).
mapred_bucket(PoolName, A, B, C, D) ->
  ?WORKER_POOL(PoolName, mapred_bucket, [A, B, C, D]).


mapred_bucket_stream(PoolName, A, B, C, D) ->
  ?WORKER_POOL(PoolName, mapred_bucket_stream, [A, B, C, D]).
mapred_bucket_stream(PoolName, A, B, C, D, E) ->
  ?WORKER_POOL(PoolName, mapred_bucket_stream, [A, B, C, D, E]).


search(PoolName, A, B) ->
  ?WORKER_POOL(PoolName, search, [A, B]).
search(PoolName, A, B, C, D) ->
  ?WORKER_POOL(PoolName, search, [A, B, C, D]).
search(PoolName, A, B, C, D, E) ->
  ?WORKER_POOL(PoolName, search, [A, B, C, D, E]).


get_index(PoolName, A, B, C) ->
  ?WORKER_POOL(PoolName, get_index, [A, B, C]).
get_index(PoolName, A, B, C, D) ->
  ?WORKER_POOL(PoolName, get_index, [A, B, C, D]).

get_index(PoolName, A, B, C, D, E) ->
  ?WORKER_POOL(PoolName, get_index, [A, B, C, D, E]).
get_index(PoolName, A, B, C, D, E, F) ->
  ?WORKER_POOL(PoolName, get_index, [A, B, C, D, E, F]).

% another per-connection option
%default_timeout(PoolName) ->
%  ?WORKER_POOL(PoolName, default_timeout, []).
