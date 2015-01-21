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
  Keyspace = "deviceinfo",
  Table = "oem_devices",
  ColumnsNames = ["oem_name", "device_id", "username"],
  InsertQuery = "INSERT INTO deviceinfo.oem_devices (oem_name, device_id, username) VALUES (?,?,?);",
  ?assertEqual(InsertQuery, dev_info_db_worker:prepare_insert_query(Keyspace, Table, ColumnsNames)).
