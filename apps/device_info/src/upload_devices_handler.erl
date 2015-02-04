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


%%-----------------------------------------------------------------------------
%% Function: init/2
%% Purpose: The callback called upon handler's call. It parses the uploaded
%%          file, removes empty records and persists them in the database.
%% Args:
%%      Req: The request body.
%%      Opts: Additional options that have been passed with the request.
%% Returns:
%%          A response with a 200 code.
%%-----------------------------------------------------------------------------
init(Req, Opts) ->
  {ok, Headers, Req2} = cowboy_req:part(Req),
  {ok, Data, Req3} = cowboy_req:part_body(Req2),
  {file, <<"inputfile">>, _Filename, _ContentType, _TE}
    = cow_multipart:form_data(Headers),
  DeviceRecords = parse(Data),
  insert(DeviceRecords),
  {ok, Req3, Opts}.


%%-----------------------------------------------------------------------------
%% Function: insert/1
%% Purpose: Inserts the device records in the database.
%% Args:
%%      Records: The device records to be persisted. They should be binary
%%               terms in a list, like the following example:
%%
%% [[<<"pioneer">>,<<"dev1">>,<<"user1">>],..., [<<"pioneer">>,<<"devN">>,<<"userN">>]]
%%
%% Returns:
%%          An array with the result of every insertion operation.
%%-----------------------------------------------------------------------------
insert(Records) ->
  [dev_info_db_worker:add_oem_device(DeviceID, OemName, Username)
    || [DeviceID, OemName, Username] <- Records].


%%-----------------------------------------------------------------------------
%% Function: parse/1
%% Purpose: Splits the binary blob to a list of binaries on endline characters.
%% Args:
%%      Data: The binary blob to be split. It should look like:
%%
%% <<"pioneer,dev1,u1\npioneer,dev2,u2\npioneer,dev3,u3">>
%%
%% Returns:
%%          An array with a binary for every records, like:
%%
%% [<<"pioneer,dev1,u1">>,<<"pioneer,dev2,u2">>,<<"pioneer,dev3,u3">>,<<>>]
%%
%%-----------------------------------------------------------------------------
parse(Data) ->
  Records = re:split(Data, "\r|\n|\r\n", [] ),
  FilteredRecords = filter_lines(Records),
  split_lines(FilteredRecords).


%%-----------------------------------------------------------------------------
%% Function: filter_lines/1
%% Purpose: Removes empty binaries from a list of binary records.
%% Args:
%%      Records: A list of binary records.
%%
%% [<<"pioneer,dev1,u1">>,<<"pioneer,dev2,u2">>,<<"pioneer,dev3,u3">>,<<>>]
%%
%% Returns:
%%          A list of records with the empty ones removed, as:
%%
%% [<<"pioneer,dev1,u1">>,<<"pioneer,dev2,u2">>,<<"pioneer,dev3,u3">>]
%%
%%-----------------------------------------------------------------------------
filter_lines(Records) ->
  [Record || Record <- Records, Record =/= <<"">>].


%%-----------------------------------------------------------------------------
%% Function: split_lines/1
%% Purpose: Splits every record to separate tokens, for every column value.
%% Args:
%%      Records: A list of binary records.
%%
%% [<<"pioneer,dev1,u1">>,<<"pioneer,dev2,u2">>,<<"pioneer,dev3,u3">>]
%%
%% Returns:
%%          A list of lists containing binary tokens for every column value:
%%
%% [["pioneer">>,<<"dev1">>,<<"u1">>], ["pioneer">>,<<"dev2">>,<<"u2">>],...]
%%
%%-----------------------------------------------------------------------------
split_lines(Records) ->
  [re:split(Record, ",", [] ) || Record <- Records].
