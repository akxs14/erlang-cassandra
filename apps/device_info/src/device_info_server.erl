%%%--------------------------------------------------------------------- 
%%% Copyright Advanced Telematic Systems GmbH 2015
%%%
%%% All rights reserved. No part of this computer programs(s) may be 
%%% used, reproduced,stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of 
%%% Advanced Telematic Systems GmbH.
%%%--------------------------------------------------------------------- 

-module(device_info_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-record(state, {}).

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------
-export([
  start_link/0,
  stop/0
]).

%% ---------------------------------------------------------------------------
%% gen_server Function Exports
%% ---------------------------------------------------------------------------
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%%% ===================================================================
%%% API functions
%%% ===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER, stop).


%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

%%-----------------------------------------------------------------------------
%% Function: init/1
%% Purpose: Initializes the OTP server and.
%% Args:
%%      OemName: The name of the OEM whose devices will be deleted. Called by
%%               the supervisor.
%% Returns:
%%          A tuple indicating the result of initialization and the server's state.
%%-----------------------------------------------------------------------------
init([]) ->
  {ok, #state{}}.
