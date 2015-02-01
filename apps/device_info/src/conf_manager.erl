%%%----------------------------------------------------------------------------
%%% Copyright Advanced Telematic Systems GmbH 2015
%%%
%%% All rights reserved. No part of this computer programs(s) may be 
%%% used, reproduced,stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of 
%%% Advanced Telematic Systems GmbH.
%%%----------------------------------------------------------------------------

-module(conf_manager).

%% REST Callbacks
-export([
  load_conf_file/0,
  get_cassandra_host/1,
  get_cassandra_port/1,
  get_dev_info_port/1
]).

%% ============================================================================
%% API functions
%% ============================================================================


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
get_cassandra_host(ConfFile) ->
  eco:term(cassandra_host, ConfFile).


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
get_cassandra_port(ConfFile) ->
  eco:term(cassandra_port, ConfFile).
