%% @author Barulin Maxim <mbarulin@gmail.com>
%% @copyright 2016 Barulin Maxim
%% @version 0.0.3
%% @title hawk_client_clients
%% @doc Супервизор над супервизорами корневых пользователей


-module(hawk_client_clients).
-behaviour(supervisor).
-include("env.hrl").
-include("mac.hrl").

-export([init/1]).
-export([start_link/0]).

%% ====================================================================
%% API functions
%% ====================================================================

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              {   undefined,                               % Id       = internal id
                  {hawk_client_client, start_link,[]},                  % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                                  % Type     = worker | supervisor
                  [hawk_client_client]                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%% ====================================================================
%% Internal functions
%% ====================================================================

