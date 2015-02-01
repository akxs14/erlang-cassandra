%%%----------------------------------------------------------------------------
%%% Copyright Advanced Telematic Systems GmbH 2015
%%%
%%% All rights reserved. No part of this computer programs(s) may be 
%%% used, reproduced,stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of 
%%% Advanced Telematic Systems GmbH.
%%%----------------------------------------------------------------------------

-module(upload_devices_handler).

%% REST Callbacks
-export([
  init/2
]).

%% ============================================================================
%% API functions
%% ============================================================================


init(Req, Opts) ->
  {ok, Headers, Req2} = cowboy_req:part(Req),
  {ok, Data, Req3} = cowboy_req:part_body(Req2),
  {file, <<"inputfile">>, Filename, ContentType, _TE}
    = cow_multipart:form_data(Headers),
  io:format("Received file ~p of content type ~p as follow:~n~p~n~n",
    [Filename, ContentType, Data]),
  {ok, Req3, Opts}.
