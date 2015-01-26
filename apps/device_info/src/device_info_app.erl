-module(device_info_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {'_', [
          {"/device",  get_device_handler,  []},
          {"/authorize_device", auth_device_handler, []},
          {"^/admin/api/oems/[^/]+/devices.*", oem_devices_handler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
    {env, [{dispatch, Dispatch}] }
  ]),  
  device_info_sup:start_link().

stop(_State) ->
  ok.
