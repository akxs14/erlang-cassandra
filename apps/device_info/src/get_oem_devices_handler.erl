%%%----------------------------------------------------------------------------
%%% Copyright Advanced Telematic Systems GmbH 2015
%%%
%%% All rights reserved. No part of this computer programs(s) may be 
%%% used, reproduced,stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of 
%%% Advanced Telematic Systems GmbH.
%%%----------------------------------------------------------------------------

-module(get_oem_devices_handler).

%% REST Callbacks
-export([
  init/2,
  content_types_provided/2,
  get_oem_devices/2
]).

%% ============================================================================
%% API functions
%% ============================================================================
init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, get_oem_devices}
  ], Req, State}.

%%-----------------------------------------------------------------------------
%% Function: get_oem_devices/2
%% Purpose: Returns the information for the device with the given device id,
%%          packaged in JSON.
%% Args:
%%      Req: The request's body.
%%      State: The server's state tuple.
%% Returns:
%%          A JSON response containing the device information.
%%-----------------------------------------------------------------------------
get_oem_devices(Req, State) ->
  OemID = cowboy_req:binding(oemid, Req),
  Devices = dev_info_db_worker:get_oem_devices(OemID),
  % Json = maps:to_json(Devices),
  Body = <<"{\"rest\": \"Hello World!\"}">>,
  {Body, Req, State}.

%% ===================================================================
%% Internal functions definitions
%% ===================================================================
