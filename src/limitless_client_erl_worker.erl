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
%% @doc limitless client pool worker
%% @end

-module(limitless_client_erl_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

%% Application callbacks
-export([start_link/1]).
-export([
  code_change/3,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  init/1,
  terminate/2
]).

-type socket() :: gen_tcp:socket().

-record(state, {socket}).

%%====================================================================
%% API connection
%%====================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init({Args, Retries}) ->
  process_flag(trap_exit, true),
  Socket = wait_connection(Args, Retries),
  {ok, #state{socket=Socket}}.

%%====================================================================
%% API actions
%%====================================================================

handle_cast(_Msg, State) -> {noreply, State}.

handle_call({setup_objectid, ObjectId, GroupId}, _From,
            #state{socket=Socket}=State) ->
  Recv = command(<<"put">>, path_setup(ObjectId, GroupId), Socket),
  {reply, Recv, State};
handle_call({is_reached_object_id, ObjectId}, _From,
            #state{socket=Socket}=State) ->
  Recv = command(<<"put">>, path_is_reached(ObjectId), Socket),
  {reply, Recv, State};
handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, #state{socket=Socket}) ->
  ok = gen_tcp:close(Socket),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec command(binary(), binary(), socket()) -> {atom(), map()}.
command(Method, Path, Socket) ->
  gen_tcp:send(Socket, limitless_client_erl_coder:encode(Method, Path)),
  {ok, Return} = gen_tcp:recv(Socket, 0, 5000),
  limitless_client_erl_coder:decode(Return).

%% Wait server is available for connection
wait_connection(_, 0) ->
  throw(limitless_connection_error);
wait_connection(#{host := Host, port := Port}=Args, Remaining_retries) ->
  error_logger:info_msg(
    "Limitless-pool: remaining retries ~p~n", [Remaining_retries]),
  case gen_tcp:connect(Host, Port, [{active, false}, binary]) of
    {ok, Socket} ->
      error_logger:info_msg("Connected!~n"),
      Socket;
    _Rest ->
      error_logger:info_msg("Sleep..~n"),
      timer:sleep(2000),
      wait_connection(Args, Remaining_retries - 1)
  end.

path_is_reached(ObjectId) ->
  << <<"/objects/">>/binary, ObjectId/binary, <<"/_isreached">>/binary >>.

path_setup(ObjectId, GroupId) ->
  << <<"/objects/">>/binary, ObjectId/binary,
     <<"/groups/">>/binary, GroupId/binary >>.
