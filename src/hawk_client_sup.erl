%% @author Barulin Maxim <mbarulin@gmail.com>
%% @copyright 2016 Barulin Maxim
%% @version 0.0.3
%% @title hawk_client_sup
%% @doc Корневой супервизор системы.


-module(hawk_client_sup).

-behaviour(supervisor).

-include("env.hrl").
-include("mac.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1]).
-export([start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
			  % queue server
			  {   hawk_client_queue,
                  {hawk_client_queue,start_link,[]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                              % Type     = worker | supervisor
                  [hawk_client_queue]                                       % Modules  = [Module] | dynamic
              },
			  % global clients supervisor
              {   hawk_client_clients,
                  {hawk_client_clients,start_link,[]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  [hawk_client_clients]                                       % Modules  = [Module] | dynamic
              },
			  % api manager for post request
			  {   hawk_client_api_manager,
                  {hawk_client_api_manager, start_link, []},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  [hawk_client_api_manager]                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%% ====================================================================
%% Internal functions
%% ====================================================================

