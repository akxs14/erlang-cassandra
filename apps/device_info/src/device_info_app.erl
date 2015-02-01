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
    eco,
    dev_info_db
  ],
  [start_dependency(Dependency) || Dependency <- Dependencies],
  start(permanent, []).


%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_Type, _StartArgs) ->
  ConfFile = load_conf_file(),
  start_cowboy(get_dev_info_port(ConfFile)),
  device_info_sup:start_link().


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
  application:start(Dependency).


%%-----------------------------------------------------------------------------
%% Function: start_cowboy/1
%% Purpose: Starts a cowboy instance supplying the RESTful API for Dev Info.
%%
%% Args:
%%      DevInfoPort: The port the server will use.
%% Returns:
%%          A tuple containing a status code.
%%-----------------------------------------------------------------------------
start_cowboy(DevInfoPort) ->
  Dispatch = cowboy_router:compile([
    {'_', [
            {"/deviceinfo", authorize_device_handler, []},
            {"/authorize_device", authorize_device_handler, []},
            {"/device/:deviceid", get_device_handler, []},
            {"/admin/api/oem/:oemid/devices", get_oem_devices_handler, []},
            {"/admin/api/clients/:clientid/devices", get_client_devices_handler, []},
            {"/admin/api/oems/:oemid/devices/upload", upload_devices_handler, []},
            {"/upload", cowboy_static, {priv_file, device_info, "upload.html"}},
            {"/files/[...]", cowboy_static, {priv_dir, device_info, "files"}}
    ]}
  ]),
  {ok, Started} = cowboy:start_http(http, 100, 
                              [{port, DevInfoPort}], 
                              [{env, [{dispatch, Dispatch}]}]
                            ).


%%-----------------------------------------------------------------------------
%% Function: load_conf_file/0
%% Purpose: Loads the configuration file ./conf/deviceonfo.conf
%%
%% Args:
%%      - 
%% Returns:
%%          A handle to the files contents loaded in mnesia.
%%-----------------------------------------------------------------------------
load_conf_file() ->
  {ok, ConfFile} = eco:setup(<<"deviceinfo.conf">>, [force_kv]),
  ConfFile.


%%-----------------------------------------------------------------------------
%% Function: get_dev_info_port/1
%% Purpose: Returns the port to be used by cowboy as defined in
%%          deviceinfo.conf.
%%
%% Args:
%%      ConfFile: The handle to configuration's file mnesia loaded data.
%% Returns:
%%          The number of the port to be used.
%%-----------------------------------------------------------------------------
get_dev_info_port(ConfFile) ->
  eco:term(device_info_port, ConfFile).
