%%%--------------------------------------------------------------------- 
%%% Copyright Advanced Telematic Systems GmbH 2015
%%%
%%% All rights reserved. No part of this computer programs(s) may be 
%%% used, reproduced,stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of 
%%% Advanced Telematic Systems GmbH.
%%%--------------------------------------------------------------------- 

-module(dev_info_db_worker).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("cqerl.hrl").

-record(state, {client}).

%% API
-export([
  get_device/1,
  delete_device/1,
  get_oem_devices/1,
  add_oem_device/3,
  add_oem_device_bound/3,
  delete_oem_devices/1
]).

-export([
  start_link/0,
  stop/0
]).

%% Supervisor callbacks
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


%%-----------------------------------------------------------------------------
%% Function: get_device/1
%% Purpose: Returns the information for the device with the given device id.
%% Args:
%%      DeviceID: The device's universal unique identifier (UUID).
%% Returns:
%%          A map containing the information of the given device.
%%-----------------------------------------------------------------------------
get_device(DeviceID) ->
  gen_server:call(?SERVER, {get_device, DeviceID}).


%%-----------------------------------------------------------------------------
%% Function: delete_device/1
%% Purpose: Deletes the information for the device with the given device id.
%% Args:
%%      DeviceID: The device's universal unique identifier (UUID).
%% Returns:
%%          A #cql_query containing the result of the delete operation.
%%-----------------------------------------------------------------------------
delete_device(DeviceID) ->
  gen_server:call(?SERVER, {delete_device, DeviceID}).


%%-----------------------------------------------------------------------------
%% Function: get_oem_devices/1
%% Purpose: Returns a list of devices related with the given OEM.
%% Args:
%%      OemName: The name of the OEM.
%% Returns:
%%          A list of devices related with the given OEM.
%%-----------------------------------------------------------------------------
get_oem_devices(OemName) ->
  gen_server:call(?SERVER, {get_oem_devices, OemName}).


%%-----------------------------------------------------------------------------
%% Function: add_oem_device/3
%% Purpose: Adds a device and relates it with the given OEM.
%% Args:
%%      OemName: The name of the OEM.
%%      DeviceID: The device's unique user identifier (UUID).
%%      Username: The device owner's username.
%% Returns:
%%          A #cql_query containing the result of the insert operation.
%%-----------------------------------------------------------------------------
add_oem_device(OemName, DeviceID, Username) ->
  gen_server:call(?SERVER, {add_oem_device, OemName, DeviceID, Username}).


%%-----------------------------------------------------------------------------
%% Function: add_oem_device_bound/3
%% Purpose: Adds a device and relates it with the given OEM.
%% Args:
%%      OemName: The name of the OEM.
%%      DeviceID: The device's unique user identifier (UUID).
%%      Username: The device owner's username.
%% Returns:
%%          A #cql_query containing the result of the insert operation.
%%-----------------------------------------------------------------------------
add_oem_device_bound(OemName, DeviceID, Username) ->
  gen_server:call(?SERVER, {add_oem_device_bound, OemName, DeviceID, Username}).


%%-----------------------------------------------------------------------------
%% Function: delete_oem_devices/1
%% Purpose: Deletes all the devices related with an OEM.
%% Args:
%%      OemName: The name of the OEM whose devices will be deleted.
%% Returns:
%%          A #cql_query containing the result of the delete operation.
%%-----------------------------------------------------------------------------
delete_oem_devices(OemName) ->
  gen_server:call(?SERVER, {delete_oem_devices, OemName}).


%%-----------------------------------------------------------------------------
%% Function: start_link/0
%% Purpose: Booting server (and linking to it).
%% Args:
%%      -
%% Returns:
%%          -
%%-----------------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%-----------------------------------------------------------------------------
%% Function: start_link/0
%% Purpose: Stopping server asynchronously.
%% Args:
%%      -
%% Returns:
%%          -
%%-----------------------------------------------------------------------------
stop() ->
  gen_server:cast(?MODULE, stop).


%%%====================================================================
%%% gen_server callbacks
%%%====================================================================


%%-----------------------------------------------------------------------------
%% Function: init/1
%% Purpose: Initializes the OTP server.
%% Args:
%%      OemName: The name of the OEM whose devices will be deleted. Called by
%%               the supervisor.
%% Returns:
%%          A tuple indicating the result of initialization and the server's state.
%%-----------------------------------------------------------------------------
init([]) ->
  {ok, Client} = cqerl:new_client({"127.0.0.1", 9042}),
  io:format("init: ~p~n",[Client]),
  {ok, #state{ client = Client }}.


%%-----------------------------------------------------------------------------
%% Function: handle_call/3 - get_device
%% Purpose: Synchronous callback for call(?SERVER, {get_device, DeviceID}).
%% Args:
%%      {get_device, DeviceID}: The callback identifier and the parameter passed
%%                              by the client.
%%      _From: The client which called the callback.
%%      #state{ client = Client }: The state record, with the client reference
%%                                 being passed to Client.
%% Returns:
%%          The device record.
%%-----------------------------------------------------------------------------
handle_call({get_device, DeviceID}, _From, #state{ client = Client }) ->
  QueryResult = select_client_device(Client, DeviceID),
  {reply, QueryResult, #state{client = Client}};


%%-----------------------------------------------------------------------------
%% Function: handle_call/3 - delete_device
%% Purpose: Synchronous callback for call(?SERVER, {delete_device, DeviceID}).
%% Args:
%%      {delete_device, DeviceID}: The callback identifier and the parameter passed
%%                                 by the client.
%%      _From: The client which called the callback.
%%      #state{ client = Client }: The state record, with the client reference
%%                                 being passed to Client.
%% Returns:
%%          The result of the delete operation.
%%-----------------------------------------------------------------------------
handle_call({delete_device, DeviceID}, _From, #state{ client = Client }) ->
  QueryResult = delete_client_device(Client, DeviceID),
  {reply, QueryResult, #state{client = Client}};


%%-----------------------------------------------------------------------------
%% Function: handle_call/3 - get_oem_devices
%% Purpose: Synchronous callback for call(?SERVER, {get_oem_devices, DeviceID}).
%% Args:
%%      {get_oem_devices, OemName}: The callback identifier and the parameter passed
%%                                   by the client.
%%      _From: The client which called the callback.
%%      #state{ client = Client }: The state record, with the client reference
%%                                 being passed to Client.
%% Returns:
%%          A list of returned devices.
%%-----------------------------------------------------------------------------
handle_call({get_oem_devices, OemName}, _From, #state{ client = Client }) ->
  QueryResult = select_oem_device(Client, OemName),
  {reply, QueryResult, #state{client = Client}};


%%-----------------------------------------------------------------------------
%% Function: handle_call/3 - add_oem_device
%% Purpose: Synchronous callback for call(?SERVER, {add_oem_device, DeviceID}).
%% Args:
%%      {add_oem_device, OemName, DeviceID, Username}: The callback identifier
%%             and the parameter passed by the client.
%%      _From: The client which called the callback.
%%      #state{ client = Client }: The state record, with the client reference
%%                                 being passed to Client.
%% Returns:
%%          The result of the insert query.
%%-----------------------------------------------------------------------------
handle_call({add_oem_device, OemName, DeviceID, Username}, _From, #state{ client = Client }) ->
  QueryResult = insert_device(Client, OemName, DeviceID, Username),
  {reply, QueryResult, #state{client = Client}};


%%-----------------------------------------------------------------------------
%% Function: handle_call/3 - add_oem_device_bound
%% Purpose: Synchronous callback for call(?SERVER, {add_oem_device_bound, DeviceID}).
%% Args:
%%      {add_oem_device_bound, OemName, DeviceID, Username}: The callback identifier
%%             and the parameter passed by the client.
%%      _From: The client which called the callback.
%%      #state{ client = Client }: The state record, with the client reference
%%                                 being passed to Client.
%% Returns:
%%          The result of the insert query.
%%-----------------------------------------------------------------------------
handle_call({add_oem_device_bound, OemName, DeviceID, Username}, _From, #state{ client = Client }) ->
  QueryResult = insert_device(Client, OemName, DeviceID, Username),
  {reply, QueryResult, #state{client = Client}};


%%-----------------------------------------------------------------------------
%% Function: handle_call/3 - get_oem_devices
%% Purpose: Synchronous callback for call(?SERVER, {get_oem_devices, DeviceID}).
%% Args:
%%      {delete_oem_devices, DeviceID}: The callback identifier and the parameter passed
%%                                      by the client.
%%      _From: The client which called the callback.
%%      #state{ client = Client }: The state record, with the client reference
%%                                 being passed to Client.
%% Returns:
%%          The result of the delete query.
%%-----------------------------------------------------------------------------
handle_call({delete_oem_devices, DeviceID}, _From, #state{ client = Client }) ->
  QueryResult = delete_oem_device(Client, DeviceID),
  {reply, QueryResult, #state{client = Client}}.


handle_cast(shutdown, State) ->
  {stop, normal, State}.

handle_info(Info, State) ->
  error_logger:info_msg("~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _Server) ->
  error_logger:info_msg("terminating~n"),
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

%%%====================================================================
%%% Internal Function Definitions
%%%====================================================================


%%-----------------------------------------------------------------------------
%% Function: insert_device/2
%% Purpose: Delete a record with the given device ID from deviceinfo.oem_devices
%% Args:
%%      Client: Cqerl client reference.
%%      DeviceID: The device ID to be deleted.
%% Returns:
%%          A #cql_result{} containing the result of the query operation.
%%-----------------------------------------------------------------------------
insert_device(Client, OemName, DeviceID, Username) ->
  insert_in_table(Client, "deviceinfo", "oem_devices",
    #{"oem_name" => OemName, "device_id" => DeviceID, "username" => Username}).


%%-----------------------------------------------------------------------------
%% Function: delete_in_table/5
%% Purpose: Deletes the rows from Keyspace.Table where 'Column' == 'Value'
%% Args:
%%      Client: Cqerl client reference.
%%      Keyspace: The keyspace used (currently deviceinfo).
%%      Table: The name of the table to query.
%%      Column: The column to used as a filter.
%%      Value: The value to use filter rows against.
%% Returns:
%%          A #cql_result{} containing the result of the query operation.
%%-----------------------------------------------------------------------------
insert_in_table(Client, Keyspace, Table, NewRecord) ->
  {ok, QueryResult} = cqerl:send_query(Client,
    #cql_query{
      statement = prepare_insert_query(Keyspace, Table, maps:keys(NewRecord)),
      values = prepare_value_tuples(NewRecord)
    }),
    QueryResult.


%%-----------------------------------------------------------------------------
%% Function: prepare_insert_query/3
%% Purpose: Formats the insertion SQL query for a given keyspace.table.
%%          The format of the query after formatting will be:
%%            "INSERT INTO keyspace.table (col1, col2, ..., colN) VALUES (?, ?, ..., ?);"
%%          The number of question marks equal the number of columns given to avoid syntax errors.
%% Args:
%%      Keyspace: The keyspace used (currently deviceinfo).
%%      Table: The name of the table to query.
%%      ColumnNames: The name of the columns to be populated.
%% Returns:
%%          A #cql_result{} containing the result of the query operation.
%%-----------------------------------------------------------------------------
prepare_insert_query(Keyspace, Table, ColumnsNames) ->
  "INSERT INTO " ++ Keyspace ++ "." ++ Table ++ " (" ++ string:join(ColumnsNames, ", ") ++
    ") VALUES (" ++ ["?" || _ <- ColumnsNames] ++ ");".


%%-----------------------------------------------------------------------------
%% Function: prepare_value_tuples/1
%% Purpose: Formats the columns/values map from #{C1 => V1, C2 => V2, ...}
%%          to a list of tuples that can be passed to cqerl driver; with the format
%%          [ {C1, V1}, {C2, V2}, {C3, V3}, ...] .
%% Args:
%%      Record: The map containing the the column name/values pairs.
%% Returns:
%%          A list of tuples, with each tuple containing a column name and the
%%          corresponding value.
%%-----------------------------------------------------------------------------
prepare_value_tuples(Record) ->
  maps:to_list(Record).


%%-----------------------------------------------------------------------------
%% Function: delete_client_device/2
%% Purpose: Delete a record with the given device ID from deviceinfo.client_devices
%% Args:
%%      Client: Cqerl client reference.
%%      DeviceID: The device ID to be deleted.
%% Returns:
%%          A #cql_result{} containing the result of the query operation.
%%-----------------------------------------------------------------------------
delete_client_device(Client, DeviceID) ->
  delete_from_table(Client, "deviceinfo", "client_devices", "device_id", DeviceID).


%%-----------------------------------------------------------------------------
%% Function: delete_oem_device/2
%% Purpose: Delete a record with the given device ID from deviceinfo.oem_devices
%% Args:
%%      Client: Cqerl client reference.
%%      DeviceID: The device ID to be deleted.
%% Returns:
%%          A #cql_result{} containing the result of the query operation.
%%-----------------------------------------------------------------------------
delete_oem_device(Client, DeviceID) ->
  delete_from_table(Client, "deviceinfo", "oem_devices", "device_id", DeviceID).


%%-----------------------------------------------------------------------------
%% Function: delete_in_table/5
%% Purpose: Deletes the rows from Keyspace.Table where 'Column' == 'Value'
%% Args:
%%      Client: Cqerl client reference.
%%      Keyspace: The keyspace used (currently deviceinfo).
%%      Table: The name of the table to query.
%%      Column: The column to used as a filter.
%%      Value: The value to use filter rows against.
%% Returns:
%%          A #cql_result{} containing the result of the query operation.
%%-----------------------------------------------------------------------------
delete_from_table(Client, Keyspace, Table, Column, Value) ->
  {ok, QueryResult} = cqerl:send_query(Client,
    #cql_query{
      statement = "DELETE FROM " ++ Keyspace ++ "." ++ Table ++ " WHERE " ++ Column ++ " = ?;",
      values = [ {list_to_atom(Column), Value} ]}),
    QueryResult.


%%-----------------------------------------------------------------------------
%% Function: select_client_device/2
%% Purpose: Get back the record with the given device ID from deviceinfo.client_devices
%% Args:
%%      Client: Cqerl client reference.
%%      DeviceID: The device UUID.
%% Returns:
%%          The records from deviceinfo.client_devices with the given device_id
%%-----------------------------------------------------------------------------
select_client_device(Client, DeviceID) ->
  select_from_table(Client, "deviceinfo", "client_devices", "device_id", DeviceID).


%%-----------------------------------------------------------------------------
%% Function: select_oem_device/2
%% Purpose: Get back the records for the given OEM from deviceinfo.oem_devices
%% Args:
%%      Client: Cqerl client reference.
%%      OemName: The OEM name.
%% Returns:
%%          The records from deviceinfo.oem_devices with the given device_id
%%-----------------------------------------------------------------------------
select_oem_device(Client, OemName) ->
  select_from_table(Client, "deviceinfo", "oem_devices", "oem_name", OemName).


%%-----------------------------------------------------------------------------
%% Function: select_oem_device/5
%% Purpose: Query the given Keyspace.Table for rows where 'Column' == 'Value'
%% Args:
%%      Client: Cqerl client reference.
%%      Keyspace: The keyspace used (currently deviceinfo).
%%      Table: The name of the table to query.
%%      Column: The column to used as a filter.
%%      Value: The value to use filter rows against.
%% Returns:
%%          The records from 'Table' where 'Column' == 'Value'
%%-----------------------------------------------------------------------------
select_from_table(Client, Keyspace, Table, Column, Value) ->
  Statement = "SELECT * FROM " ++ Keyspace ++ "." ++ Table ++ " WHERE " ++ Column ++ " = ?;",
  Values = [ {list_to_atom(Column), Value} ],
  io:format("Statement: ~p~n",[Statement]),
  io:format("Values: ~p~n",[Values]),
  {ok, QueryResult} = cqerl:run_query(Client,
    #cql_query{
      statement = Statement,
      values = Values
    }
  ),
  QueryResult.
