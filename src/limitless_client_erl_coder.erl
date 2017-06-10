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
%% @doc limitless client coder
%% @end

-module(limitless_client_erl_coder).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

%% Application callbacks
-export([
  decode/1,
  encode/2,
  encode/3
]).

%%====================================================================
%% API connection
%%====================================================================

-spec encode(binary(), binary()) -> binary().
encode(Method, Path) ->
  encode(Method, Path, #{}).

-spec encode(binary(), binary(), map()) -> binary().
encode(Method, Path, Args) ->
  Packet = Args#{<<"method">> => Method, <<"path">> => Path},
  jsx:encode(Packet).

-spec decode(binary()) -> {atom(), map()}.
decode(Packet) ->
  #{<<"result">> := Result,
    <<"context">> := Context} = jsx:decode(Packet, [return_maps]),
  {list_to_atom(binary_to_list(Result)), Context}.
