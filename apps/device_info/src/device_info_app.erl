%%%--------------------------------------------------------------------- 
%%% Copyright Advanced Telematic Systems GmbH 2015
%%%
%%% All rights reserved. No part of this computer programs(s) may be 
%%% used, reproduced,stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of 
%%% Advanced Telematic Systems GmbH.
%%%--------------------------------------------------------------------- 

-module(device_info_app).

-behaviour(application).

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------
-export([
  start/2,
  stop/1,
  shell/0
]).


%%% ===================================================================
%%% API functions
%%% ===================================================================

%%-----------------------------------------------------------------------------
%% Function: shell/0
%% Purpose: Called by make shell, used for debugging and development sessions.
%%          Initializes all the dependencies so they don't have to be initialized
%%          manually during a shell session. After all dependencies have been 
%%          initialized it will start device_info itself.
%%
%%          WARNING: Update the 'Dependencies' list when dependencies are add
%%                   or removed from the project.
%%
%% Args:
%%      -
%% Returns:
%%          The state of the initialized device_info_sup process.
%%-----------------------------------------------------------------------------
shell() ->
  Dependencies = [
    crypto,
    asn1,
    public_key,
    ssl,
    ranch,
    cowlib,
    cowboy,
    pooler,
    cqerl,
    dev_info_db,
    eco
  ],
  [start_dependency(Dependency) || Dependency <- Dependencies],
  start(permanent, []).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_Type, _StartArgs) ->
  start_cowboy(),
  device_info_sup:start_link(["pipa"]).

stop(_State) ->
  ok.


%%-----------------------------------------------------------------------------
%% Function: start_dependency/1
%% Purpose: Initializes a application with the given 'Dependency' name.
%%
%% Args:
%%      Dependency: The name of the application to be initialized.
%% Returns:
%%          The state of the initialized application process.
%%-----------------------------------------------------------------------------
start_dependency(Dependency) ->
  io:format("Starting ~p~n",[Dependency]),

  % {ok, F} = eco:setup(<<"deviceinfo.conf">>),
  % AuthPlusPort = eco:term(auth_plus_port, F),

  % io:format("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~n"),
  % io:format("~p~n",[AuthPlusPort]),
  % io:format("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~n"),


  application:start(Dependency).

start_cowboy() ->
  Dispatch = cowboy_router:compile([
    {'_', [
            {"/deviceinfo", authorize_device_handler, []},
            {"/authorize_device", authorize_device_handler, []},
            {"/device/:deviceid", get_device_handler, []},
            {"/admin/api/oem/:oemid/devices", get_oem_devices_handler, []},
            {"/admin/api/clients/:clientid/devices",get_client_devices_handler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, 
                              [{port, 9222}], 
                              [{env, [{dispatch, Dispatch}]}]
                            ).