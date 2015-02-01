%%%----------------------------------------------------------------------------
%%% Copyright Advanced Telematic Systems GmbH 2015
%%%
%%% All rights reserved. No part of this computer programs(s) may be 
%%% used, reproduced,stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of 
%%% Advanced Telematic Systems GmbH.
%%%----------------------------------------------------------------------------

-module(oauth_2_client).

%% REST Callbacks
-export([
  get_access_token/0
]).

%% ============================================================================
%% API functions
%% ============================================================================

get_access_token() ->
  ConfFile = conf_manager:load_conf_file(),

  AuthPlusSecret = conf_manager:get_auth_plus_pass(ConfFile),
  AuthPlusPass = conf_manager:get_auth_plus_secret(ConfFile),
  AuthPlusHost = conf_manager:get_auth_plus_host(ConfFile),
  AuthPlusPort = conf_manager:get_auth_plus_port(ConfFile),

  {ok, Headers, Client} = oauth2c:retrieve_access_token(
    AuthPlusPass,
    AuthPlusHost + "/" ++ AuthPlusPort,
    <<"Client">>,
    AuthPlusSecret),
  Client.
