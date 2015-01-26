%%%--------------------------------------------------------------------- 
%%% Copyright Advanced Telematic Systems GmbH 2015
%%%
%%% All rights reserved. No part of this computer programs(s) may be 
%%% used, reproduced,stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of 
%%% Advanced Telematic Systems GmbH.
%%%--------------------------------------------------------------------- 

-module(authorize_device_handler).

%% REST Callbacks
-export([
  init/2,
  content_types_provided/2,
  authorize/2
]).

%% ===================================================================
%% API functions
%% ===================================================================
init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, authorize}
  ], Req, State}.

%%-----------------------------------------------------------------------------
%% Function: authorize/2
%% Purpose: Returns the information for the device with the given device id,
%%          packaged in JSON.
%% Args:
%%      Req: The request's body.
%%      State: The server's state tuple.
%% Returns:
%%          A JSON response containing the device information.
%%-----------------------------------------------------------------------------
authorize(Req, State) ->
  Body = <<"{\"rest\": \"Hello\"}">>,
  {Body, Req, State}.

%% ===================================================================
%% Internal functions definitions
%% ===================================================================

