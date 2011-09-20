riak_pool: the simplest way to have persistent riak client connections
======================================================================

Usage
-----

    application:start(riak_pool).
    riak_pool:get(pool_name, <<"bucket">>, <<"key">>).

Pools are defined in your app config (see examples in `riak_pool.app`).
Each pool defined in your config will create a live pool when the application
is started.


Tests
-----
Sorry, no tests yet.  It works for me with the default config provided.
