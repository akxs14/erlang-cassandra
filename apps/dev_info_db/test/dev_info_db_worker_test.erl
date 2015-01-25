%%%--------------------------------------------------------------------- 
%%% Copyright Advanced Telematic Systems GmbH 2015
%%%
%%% All rights reserved. No part of this computer programs(s) may be 
%%% used, reproduced,stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of 
%%% Advanced Telematic Systems GmbH.
%%%--------------------------------------------------------------------- 

-module(dev_info_db_worker_test).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Unit Tests
%%%====================================================================

my_test() ->
  ?assert(0 + 0 =:=  0).

my_second_test() ->
  ?assert(0 + 1 =:= 1 ).

simple_test() ->
  ?assert(1 + 2 =:= 3).

prepare_insert_query_test() ->
  Keyspace        = "deviceinfo",
  Table           = "oem_devices",
  NewRecord       = #{"device_id" => "DeviceID", "oem_name" => "OemName", "username" => "Username"},
  InsertQuery     = "INSERT INTO deviceinfo.oem_devices (device_id, oem_name, username) VALUES (?,?,?);",
  FormattedQuery  = dev_info_db_worker:prepare_insert_query(Keyspace, Table, NewRecord),
  ?assertEqual(InsertQuery, FormattedQuery).


parse_column_names_test() ->
  TestRecord = #{"oem_name" => "OemName", "device_id" => "DeviceID", "username" => "Username"},
  ColumnList = dev_info_db_worker:parse_column_names(TestRecord),
  ?assertEqual(["device_id", "oem_name", "username"], ColumnList).


stringify_column_names_test() ->
  ColumnList = ["device_id", "oem_name", "username"],
  ColumnString = dev_info_db_worker:stringify_column_names(ColumnList),
  ?assertEqual("device_id, oem_name, username", ColumnString).


prepare_query_wildcards_test() ->
  ColumnList = ["device_id", "oem_name", "username"],
  WildcardList = dev_info_db_worker:prepare_query_wildcards(ColumnList),
  ?assertEqual("?,?,?,", WildcardList).


remove_trailing_delimiters_test() ->
  WildcardsTrailed = "?,?,?,",
  WildcardNotTrailed = dev_info_db_worker:remove_trailing_delimiters(WildcardsTrailed),
  ?assertEqual("?,?,?", WildcardNotTrailed).


prepare_value_tuples_test() ->
  TestRecord = #{"oem_name" => "OemName", "device_id" => "DeviceID", "username" => "Username"},
  TuplesList = dev_info_db_worker:prepare_value_tuples(TestRecord),
  ?assertEqual([{"device_id", "DeviceID"}, {"oem_name", "OemName"}, {"username", "Username"}], TuplesList).
