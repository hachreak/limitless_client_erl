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
%% @doc limitless_client_erl public API
%% @end

-module(limitless_client_erl).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  is_reached_object_id/2,
  setup_objectid/3
]).

%%====================================================================
%% API
%%====================================================================

is_reached_object_id(PoolName, ObjectId) ->
  poolboy:transaction(PoolName, fun(Worker) ->
      gen_server:call(Worker, {is_reached_object_id, ObjectId})
    end).

setup_objectid(PoolName, ObjectId, GroupId) ->
  poolboy:transaction(PoolName, fun(Worker) ->
      gen_server:call(Worker, {setup_objectid, ObjectId, GroupId})
    end).

%%====================================================================
%% Internal functions
%%====================================================================
