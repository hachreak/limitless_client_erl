%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2017 Leonardo Rossi
%%%
%%% This software is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This software is distributed in the hope that it will be useful, but
%%% WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this software; if not, write to the Free Software Foundation,
%%% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%% @doc limitless_client_erl top level supervisor.
%% @end

-module(limitless_client_erl_sup).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  RestartStrategy = {one_for_all, 0, 1},
  PoolSpecs = setup(config()),
  {ok, {RestartStrategy, PoolSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

config() ->
  {ok, Pools} = application:get_env(limitless_client_erl, pools),
  Retries = application:get_env(limitless_client_erl, retry, 20),
  {Pools, Retries}.

setup({Pools, Retries}) ->
  lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
      PoolArgs = [
          {name, {local, Name}}, {worker_module, limitless_client_erl_worker}
        ] ++ SizeArgs,
      poolboy:child_spec(Name, PoolArgs, {maps:from_list(WorkerArgs), Retries})
    end, Pools).
