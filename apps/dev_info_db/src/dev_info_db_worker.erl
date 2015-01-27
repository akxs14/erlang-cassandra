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

%%-----------------------------------------------------------------------------
%% API Function Exports
%%-----------------------------------------------------------------------------
-export([
  get_device/1,
  get_oem_devices/1,
  delete_device/2,
  add_oem_device/3,
  add_oem_device_bound/3,
  delete_oem_devices/2
]).

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

-ifdef(TEST).
-compile(export_all).
-endif.

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
delete_device(OemName, DeviceID) ->
  gen_server:call(?SERVER, {delete_device, OemName, DeviceID}).


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
add_oem_device(DeviceID, OemName, Username) ->
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
delete_oem_devices(OemName, Devices) ->
  gen_server:call(?SERVER, {delete_oem_devices, OemName, Devices}).


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
handle_call({delete_device, OemName, DeviceID}, _From, #state{ client = Client }) ->
  delete_client_device(Client, OemName, DeviceID),
  {reply, "Row Deleted", #state{client = Client}};


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
  insert_device(Client, OemName, DeviceID, Username),
  {reply, "Row Inserted", #state{client = Client}};


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
handle_call({delete_oem_devices, OemName, Devices}, _From, #state{ client = Client }) ->
  [delete_oem_device(Client, OemName, DeviceID) || DeviceID <- Devices],
  {reply, ok, #state{client = Client}}.


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
  NewRecord = #{"oem_name" => OemName, "device_id" => DeviceID, "username" => Username},
  insert_in_table(Client, "deviceinfo", "oem_devices", NewRecord).


%%-----------------------------------------------------------------------------
%% Function: delete_client_device/2
%% Purpose: Delete a record with the given device ID from deviceinfo.client_devices
%% Args:
%%      Client: Cqerl client reference.
%%      DeviceID: The device ID to be deleted.
%% Returns:
%%          A #cql_result{} containing the result of the query operation.
%%-----------------------------------------------------------------------------
delete_client_device(Client, OemName, DeviceID) ->
  delete_from_table(Client, "deviceinfo", "oem_devices", "oem_name", OemName, "device_id", DeviceID).


%%-----------------------------------------------------------------------------
%% Function: delete_oem_device/2
%% Purpose: Delete a record with the given device ID from deviceinfo.oem_devices
%% Args:
%%      Client: Cqerl client reference.
%%      DeviceID: The device ID to be deleted.
%% Returns:
%%          A #cql_result{} containing the result of the query operation.
%%-----------------------------------------------------------------------------
delete_oem_device(Client, OemName, DeviceID) ->
  delete_from_table(Client, "deviceinfo", "oem_devices", "oem_name", OemName, "device_id", DeviceID).


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
  select_from_table(Client, "deviceinfo", "oem_devices", "device_id", DeviceID).


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
prepare_insert_query(Keyspace, Table, NewRecord) ->
  ColumnNames = parse_column_names(NewRecord),
  "INSERT INTO " ++ Keyspace ++ "." ++ Table ++ " (" ++ stringify_column_names(ColumnNames) ++
  ") VALUES (" ++ remove_trailing_delimiters(prepare_query_wildcards(ColumnNames)) ++ ");".


%%-----------------------------------------------------------------------------
%% Function: parse_column_names/1
%% Purpose: It parses a map containing a table row record and returns the names
%%          of the table columns. The names are sorted alphabetically.
%% Args:
%%      RowRecord: A map containing the table column names as keys and row
%%                 table row contents as values. It should have the form:
%%                 #{"col1" => "val1", "col2" => "val2", ..., "colN" => "valN",}
%% Returns:
%%          A list containing the column names.
%%          It will have the form ["col1", "col2", ..., "colN"].
%%-----------------------------------------------------------------------------
parse_column_names(RowRecord) ->
  lists:sort(maps:keys(RowRecord)).


%%-----------------------------------------------------------------------------
%% Function: stringify_column_names/1
%% Purpose: It formats a list of table column names to a single, comma
%%          delimited string.
%% Args:
%%      ColumnNames: A list of column names in the form ["col1", "col2", ..., "colN"].
%% Returns:
%%          A formatted string in the form "col1, col2, ..., colN" .
%%-----------------------------------------------------------------------------
stringify_column_names(ColumnNames) ->
  string:join(ColumnNames, ", ").


%%-----------------------------------------------------------------------------
%% Function: prepare_query_wildcards/1
%% Purpose: It generates a string of wildcard question marks from a list of
%%          column names. The arity of wildcards is the same with the list's arity.
%% Args:
%%      ColumnNames: A list with containing the column names used in a query.
%%                   It should have the form ["col1", "col2", ..., "colN"].
%% Returns:
%%          A string of wildcard characters with the arity of the column list.
%%          It will have the form "?, ?, ?, ...,?".
%%-----------------------------------------------------------------------------
prepare_query_wildcards(ColumnNames) ->
  lists:append(["?," || _ <- ColumnNames]).


%%-----------------------------------------------------------------------------
%% Function: stringify_query_wildcards/1
%% Purpose: It removes trailing commas from stringified wildcard lists.
%% Args:
%%      WildcardList: A string of wildcards in the form "?, ?,... , ?,".
%% Returns:
%%          A formatted string without trailing delimiting commas in the form
%%          "?, ?, ..., ?, ?" .
%%-----------------------------------------------------------------------------
remove_trailing_delimiters(WildcardString) ->
  string:strip(WildcardString, right, $,).


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
  [{list_to_atom(Column), Value} || {Column, Value} <- maps:to_list(Record)].


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
  Statement = "SELECT * FROM " ++ Keyspace ++ "." ++ Table ++ " WHERE " ++ Column ++ " = ? ALLOW FILTERING;",
  ValueTuple = [ {list_to_atom(Column), Value} ],
  {ok, QueryResult} = cqerl:run_query(Client,
    #cql_query{
      statement = Statement,
      values = ValueTuple
    }
  ),
  { query_tuples, RowHeader, RowsValues } = extract_query_results(QueryResult),
  assemble_result_hash(Table, RowHeader, RowsValues).



assemble_result_hash(Table, _RowHeader, []) ->
  maps:from_list([ {Table , []} ]);

assemble_result_hash(Table, RowHeader, RowsValues) ->
  maps:from_list([ {Table ,[pack_row(RowHeader, Row) || Row <- RowsValues]} ]).



pack_row(RowHeader, Row) ->
  Headers = [Header || { _, _, _, Header, _ } <- RowHeader],
  % ColumnTypes = [ColumnType || { _, _, _, _, ColumnType } <- RowHeader],
  add_kv(Headers, Row, []).



add_kv([HeaderH | []], [RowH | []], KVList) ->
  ListValue = list_to_binary(RowH),
  maps:from_list([{HeaderH, ListValue} | KVList]);

add_kv([HeaderH | HeaderT], [RowH | RowT], KVList) ->
  ListValue = list_to_binary(RowH),
  add_kv(HeaderT, RowT, [{HeaderH, ListValue} | KVList]).


%%-----------------------------------------------------------------------------
%% Function: extract_query_results/1
%% Purpose: Parses the column names with their respective data types, and a list
%%          of tuples containing the values of all returned rows.
%% Args:
%%      QueryResults: A #cql_result record containing the results of the
%%                    submitted query. The record has the structure:
%%
%% #{ cql_result, 
%%   [ {cqerl_result_column_spec, KeySpace,Table, ColumnName, DataType}, {...}, ..... ],
%%   [ [ColVal1,..,ColValN], [ColVal1,..,ColValN], ... ],
%%   { cql_query, .... },
%%   { <ProcessID>m #Ref<RefID> }
%% }
%%
%% Returns:
%%          A tuple containing the specs of the selected table columns and
%%          a list containing the values of every returned row. The tuple's
%%          structure is:
%%
%% { query_tuples,
%%   [ {cqerl_result_column_spec, KeySpace,Table, ColumnName, DataType}, {...}, ..... ],
%%   [ [ColVal1,..,ColValN], [ColVal1,..,ColValN], ... ]
%% }
%%-----------------------------------------------------------------------------
extract_query_results(QueryResult) ->
  { _, RowHeader, RowsValues, _, _ } = QueryResult,
  { query_tuples, RowHeader, RowsValues }.


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
delete_from_table(Client, Keyspace, Table, KeyColumn, KeyValue, Column, Value) ->
  Statement = "DELETE FROM " ++ Keyspace ++ "." ++ Table ++ " WHERE "
   ++ Column ++ " = ? AND " ++ KeyColumn ++ "= ?;",
  Value = [{list_to_atom(Column), Value}, {list_to_atom(KeyColumn), KeyValue}],
  {ok, void} = cqerl:run_query(Client,
    #cql_query{
      statement = Statement,
      values = Value
    }),
  ok.


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
  Statement = prepare_insert_query(Keyspace, Table, NewRecord),
  Values = prepare_value_tuples(NewRecord),
  {ok, void} = cqerl:run_query(Client,
    #cql_query{
      statement = Statement,
      values = Values
    }),
  ok.
