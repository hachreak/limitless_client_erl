limitless_client_erl
====================

An Erlang driver for [limitless](https://github.com/hachreak/limitless)
distributed rate-limiter.

Build
-----

    $ rebar3 compile

Run
---

Run first the
[limitless service](https://github.com/hachreak/limitless_service).
It's running a complete rate-limiter service.
You can run with demo configuration with:

    $ git clone https://github.com/hachreak/limitless_service.git
    $ cd limitless_service
    $ make node1

Now, open a new shell and run erlang with:

    $ git clone https://github.com/hachreak/limitless_client_erl.git
    $ cd limitless_client_erl
    $ rebar3 shell --config priv/example.config --apps limitless_client_erl
    1> limitless_client_erl:setup_objectid(mypool, <<"token1">>, <<"token">>).
    {ok,#{}}
    2> limitless_client_erl:is_reached_object_id(mypool, <<"token1">>).
    {ok,#{<<"info">> => [#{<<"extra">> => [#{<<"expiry">> => 86382,
                <<"max">> => 1000,
                <<"remaining">> => 1000,
                <<"type">> => <<"Token-Daily">>},
              #{<<"expiry">> => 882,
                <<"max">> => 100,
                <<"remaining">> => 100,
                <<"type">> => <<"Token-15min">>}],
             <<"is_reached">> => false}],
          <<"is_reached">> => false}}
