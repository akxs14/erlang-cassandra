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

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_Type, _StartArgs) ->
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
                              [{port, 9000}], 
                              [{env, [{dispatch, Dispatch}]}]
                            ),
  device_info_sup:start_link().

stop(_State) ->
  ok.
