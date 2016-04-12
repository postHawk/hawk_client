%% @author Barulin Maxim <mbarulin@gmail.com>
%% @copyright 2016 Barulin Maxim
%% @version 0.0.3
%% @title hawk_client_client
%% @doc Супервизор над процессами одного (корневого) пользователя.
%% @todo В клиентской версии этот уровнь не нужен, нужно его убрать


-module(hawk_client_client).
-behaviour(supervisor).
-include("env.hrl").
-include("mac.hrl").

-export([init/1]).
-export([start_link/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================

start_link(Name) ->
    supervisor:start_link(?MODULE, [Name]).

init([Name]) ->
	gproc:reg_or_locate({n, l, Name}),
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {hawk_client_chat_worker, start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%% ====================================================================
%% Internal functions
%% ====================================================================

