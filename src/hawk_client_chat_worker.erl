%% @author Barulin Maxim <mbarulin@gmail.com>
%% @copyright 2016 Barulin Maxim
%% @version 0.0.3
%% @title hawk_client_chat_worker
%% @doc Модуль, который отвечает за поддержание связи с пользователем и обработку запросов.

-module(hawk_client_chat_worker).
-author('mbarulin@gmail.com').
-behaviour(gen_fsm).

-export([start_link/1]).
-import(crypto, [hmac/2]).
-include("env.hrl").
-include("mac.hrl").
%% gen_fsm callbacks
-export([init/1, handle_event/3,
	handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
	'WAIT_FOR_SOCKET'/2,
	'WAIT_FOR_DATA'/2,
	'WAIT_USER_MESSAGE'/2,
	'WAIT_LOGIN_MESSAGE'/2,
	'POST_ANSWER'/2
]).

-export([handle_req_by_type/4]).

-record(state,{
	socket,
	addr,
	host_name,
	key,
	curent_login,
	register_login,
	transport,
	parent
}).

-spec get_data_from_worker(Params :: any()) -> any().
%% @doc Вызывает апи для получения информации
get_data_from_worker(Params) -> gen_server:call(hawk_client_api_manager, Params, 30000).

-spec start_link(Parent :: binary()) -> {ok, pid()}.
%% @doc Старт модуля
start_link(Parent) -> gen_fsm:start_link(?MODULE, [Parent], []).

-spec init(Parent :: binary()) ->  {ok, 'WAIT_FOR_SOCKET', State :: #state{}}.
%% @doc Инициализация модуля
init([Parent]) -> {ok, 'WAIT_FOR_SOCKET', #state{parent=Parent}}.

-spec 'WAIT_FOR_SOCKET'(
	{socket_ready, Socket :: tuple() , H_name :: binary(), Transport :: module()} |
	{data, Data :: binary()}
	, State :: #state{}) ->  {next_state, 'WAIT_FOR_DATA', State :: #state{}}.
%% @doc Ждём готовый сокет
'WAIT_FOR_SOCKET'({socket_ready, Socket, H_name, Transport}, State) ->
	% Now we own the socket
	{
		next_state,
		'WAIT_FOR_DATA', State#state{
		socket=Socket,
		host_name=list_to_binary(H_name),
		transport=Transport
	}
	};

'WAIT_FOR_SOCKET'(_Other, State) -> {next_state, 'WAIT_FOR_SOCKET', State}.

-spec 'WAIT_FOR_DATA'({data, Data :: binary()} | timeout
	, State :: #state{}) ->
	{next_state, 'WAIT_LOGIN_MESSAGE', State :: #state{}} | {stop, normal, State :: #state{}}.
%% @doc Обрабатываем поступившие данные
'WAIT_FOR_DATA'({data, Data}, State) ->
	handle_req_by_type(hawk_client_lib:is_post_req(Data), Data, State);

%% @doc Падаем по таймауту
'WAIT_FOR_DATA'(timeout, State) ->
	error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
	{stop, normal, State};

%% @doc Пришло что-то неожиданное, игнорируем
'WAIT_FOR_DATA'(_Data, State) -> {next_state, 'WAIT_FOR_DATA', State}.

-spec 'WAIT_LOGIN_MESSAGE'({data, Data :: binary()}, State :: #state{}) ->
	{next_state, 'WAIT_USER_MESSAGE', State :: #state{}} | {stop, normal, State :: #state{}}.
%% @doc Пришло сообщение с логином пользвоателя
'WAIT_LOGIN_MESSAGE'({data, Bin}, State) ->
	{ok, Data} =  handle_data(Bin),
	IsJson = jsx:is_json(Data),
	% в текущей версии поддерживается старый тип авторизации по логину
	%без токена
	%@todo убрать авторизацию без токена
	if
		IsJson /= false -> handle_auth_type(strong, jsx:decode(Data),  State) ;
		true -> handle_auth_type(simple, Data, State)
	end.

-spec 'WAIT_USER_MESSAGE'(
	{data, Data :: binary()} |
	{new_message, Data :: binary()} |
	{'EXIT', _Pid :: pid(), _Reason :: any()}, State) ->
	{next_state, 'WAIT_USER_MESSAGE', State} | {stop, normal, State}.
%% @doc Основной обработчик передаваемых сообщений
'WAIT_USER_MESSAGE'({data, Bin}, #state{socket=S, transport=Transport, parent=Mlogin} = State) ->
	{ok, Data} =  handle_data(Bin),

	case jsx:is_json(Data) of
		true ->
			J_data = jsx:decode(Data),

			case J_data of
				false ->
					Transport:close(S),
					{stop, normal, State};
				_ ->

					Action = proplists:get_value(<<"hawk_action">>, J_data),
					Login = get_user_login(J_data),
					case dets:lookup(reg_users_data, {Login, Mlogin}) of
						[] ->
							hawk_client_lib:get_server_message(
								proplists:get_value(<<"event">>, J_data), ?ERROR_USER_NOT_REGISTER
							);
						_ ->
							handle_json_message({
								Action, get_to_from_record(J_data), J_data
							}, State)
					end,
					{next_state, 'WAIT_USER_MESSAGE', State}
			end;
		false ->
			Transport:close(S),
			{stop, normal, State}
	end;

%% @doc Обработчик сообщения от другого процесса (пользователя)
'WAIT_USER_MESSAGE'({new_message, Bin}, #state{socket=S, transport=Transport} = State) ->
	hawk_client_lib:send_message(true, jsx:encode(Bin), S, Transport),
	{next_state, 'WAIT_USER_MESSAGE', State};

%% @doc Обработчик закрытия дочернего процесса
'WAIT_USER_MESSAGE'({'EXIT', _Pid, _Reason}, State) ->
	{next_state, 'WAIT_USER_MESSAGE', State}.

-spec 'POST_ANSWER'({data, Data :: binary()}, State) ->  {stop, normal, State}.
%% @doc Обработчик POST-запроса из библиотеки
'POST_ANSWER'({data, Data}, #state{socket=S, transport=Transport} = State) ->
	{ok, {http_request,_Method,{abs_path, _URL},_}, H} = erlang:decode_packet(http, Data, []),
	[_Headers, {body, POST}] = hawk_client_lib:parse_header(H),

	Res =
		%определяемся с запрашиваемой командой апи
		case  hawk_client_lib:get_api_action(POST) of
			false ->
				hawk_client_lib:get_server_message(<<"check_data">>, ?ERROR_UNKNOW_DATA_TYPE);
			{ok, Qtype, JSON} ->
				%отправку сообщений обрабатываем отдельно
				case Qtype of
					<<"send_group_message">> -> api_action({Qtype, JSON}, State, off_output) ;
					<<"send_message">> -> api_action({Qtype, JSON}, State) ;
					_ -> api_action({Qtype, JSON})
				end
		end,

	Frame = hawk_client_lib:convert_to_binary([
		"HTTP/1.1 200 OK\r\n",
		"Cache-Control: no-cache\r\n",
		"Content-Type: application/json; charset=UTF-8\r\n",
		"Connection: close\r\n",
		("Content-Length:" ++ integer_to_list(byte_size(Res)) ++ "\r\n"),
		"\r\n",
		Res
	]),


	hawk_client_lib:send_message(false, Frame, S, Transport),
	Transport:close(S),

	{stop, normal, State}.

-spec handle_req_by_type(get, Data :: binary(), S :: tuple(), Transport :: module()) ->
	{next_state, 'WAIT_LOGIN_MESSAGE', State :: #state{}} | {stop, normal, State :: #state{}}.
-spec handle_req_by_type(post | get, Data :: binary(), State :: #state{}) -> {stop, normal, State :: #state{}}.
%% @doc Обрабатываем запрос в зависимости от типа - GET (от браузера), POST (от библиотеки)
%todo нужно вынести обработку POST запросов в отдельный модуль
handle_req_by_type(get, Data, S, Transport) -> handle_req_by_type(get, Data, #state{socket=S, transport=Transport}).
handle_req_by_type(post, Data, State)       -> ?MODULE:'POST_ANSWER'({data, Data}, State);
%% @doc Обрабатываем handshake запрос
handle_req_by_type(get, Data, #state{socket=S, transport=Transport} = State) ->
	{ok, {http_request,_Method,{abs_path, _URL},_}, H} = erlang:decode_packet(http, Data, []),
	[Headers, {body, _Body}] = hawk_client_lib:parse_header(H),

	Key = proplists:get_value(<<"Sec-WebSocket-Key">>, Headers),
	case Key of
		undefined -> B_all_answ = hawk_client_lib:get_server_message(<<"handshake">>, ?ERROR_INVALID_HANDSHAKE);
		_ -> {ok, B_all_answ} = get_awsw_key(Key)
	end,

	hawk_client_lib:send_message(false, B_all_answ, S, Transport),
	{next_state, 'WAIT_LOGIN_MESSAGE', State}.

-spec handle_auth_type(simple | strong, Data :: binary(), State) -> {stop, normal, State}.
%% @doc Простая авторизация
%% @deprecated
handle_auth_type(simple, Data, State) ->
	{ok, User} = ?get_user_by_key(),
	handle_login_format(check_login_format(Data), Data, State#state{key=maps:get(<<"key">>, User)});
%% @doc Усиленная авторизация с токеном
handle_auth_type(strong, Data,  #state{parent=MLogin, host_name=H_name} = State) ->
	Login = proplists:get_value(<<"id">>, Data),
	Token = proplists:get_value(<<"token">>, Data),

	Check =
		if
			Token /= undefined -> compare_token({Login, MLogin, H_name}, Token);
			true -> false
		end,
	if
		Check == true ->
			{ok, User} = ?get_user_by_key(),
			handle_login_format(check_login_format(Login), Login, State#state{key=maps:get(<<"key">>, User)});
		true -> handle_login_format(invalid_token, Login, State)
	end.

-spec handle_login_format(true | false | invalid_token, User_id :: binary(), State) ->
	{next_state, 'WAIT_LOGIN_MESSAGE', State} |{stop, normal, State} | {next_state, 'WAIT_USER_MESSAGE', State}.
%% @doc Проверяет регистрация пользователя
handle_login_format(true, User_id, #state{host_name=H_name, key=Key} = State) ->
	Ch_res = get_data_from_worker({check_user_by_domain, Key, User_id, H_name}),
	handle_login_main_data(Ch_res, User_id, State);

%% @doc Логин пришёл в неверном формате
handle_login_format(false, _User_id, #state{socket=S, transport=Transport} = State) ->
	hawk_client_lib:send_message(true, hawk_client_lib:get_server_message(<<"check_login">>, ?ERROR_INVALID_LOGIN_FORMAT), S, Transport),
	{next_state, 'WAIT_LOGIN_MESSAGE', State};

%% @doc Пришёл неверный токен
handle_login_format(invalid_token, _, #state{socket=S, transport=Transport} = State) ->
	hawk_client_lib:send_message(true, hawk_client_lib:get_server_message(<<"check_token">>, ?ERROR_INVALID_TOKEN), S, Transport),
	{next_state, 'WAIT_LOGIN_MESSAGE', State}.

-spec handle_login_main_data({ok,false, Reason :: atom()} | {ok,true} , Register_login :: binary(), State) ->
	{next_state, 'WAIT_LOGIN_MESSAGE', State} |{stop, normal, State} | {next_state, 'WAIT_USER_MESSAGE', State} .
%% @doc Возвращает сообщение об ошибке авторизации или регистрирует пользователя
handle_login_main_data({ok,false, Reason}, _, #state{socket=S, transport=Transport} = State) ->
	Msg =
		case Reason of
			no_id     -> hawk_client_lib:get_server_message(<<"check_user">>, ?ERROR_USER_NOT_REGISTER);
			no_login  -> hawk_client_lib:get_server_message(<<"check_user">>, ?ERROR_INVALID_API_KEY);
			no_domain -> hawk_client_lib:get_server_message(<<"check_user">>, ?ERROR_DOMAIN_NOT_REGISTER)
		end,

	hawk_client_lib:send_message(true, Msg, S, Transport),
	Transport:close(S),
	{stop, normal, State};

handle_login_main_data({ok,true}, Register_login, #state{socket=S, transport=Transport, host_name=H_name} = State) ->
	RegLogin = {H_name, Register_login},
	gproc:reg({p,l,RegLogin}, undefined),

	hawk_client_lib:send_message(true, hawk_client_lib:get_server_message(<<"check_user">>, false, ?OK), S, Transport),
	%если для пользователя есть сообщения пока он был оффлайн отправляем их ему
	case hawk_client_queue:get(RegLogin) of
		[{_Key, _Size, List}] ->
			lists:foreach(fun(To_data)->
				handle_user_message(on_output, [self()], Register_login, To_data, State)
			              end, List),
			hawk_client_queue:clear(RegLogin);
		[] ->
			true
	end,

	{next_state, 'WAIT_USER_MESSAGE', State#state{curent_login=RegLogin, register_login=Register_login}}.

-spec handle_json_message({Type :: binary(),
	{ToUser :: binary(), undefined} |
	{undefined, ToGrp :: [binary()]} |
	{ToUser :: binary(), ToGrp :: [binary()]},
	J_data :: tuple()}, State :: #state{}) -> ok.
%% @doc Обработчик отправки сообщения одному пользователю
handle_json_message({<<"send_message">>, {ToUser, undefined}, J_data},
	#state{socket=S, host_name=H_name, curent_login=CurentLogin, transport=Transport, key=Key} = State) ->
	Domains = proplists:get_value(<<"domains">>, J_data),
	C_j_data = hawk_client_lib:delete_keys([<<"key">>, <<"domains">>], J_data),

	%запрещаем отправку сообщений самому себе
	if
		{H_name, ToUser} =/= CurentLogin ->
			handle_user_message(
				on_output, get_data_from_worker({get_pids, Key, [ToUser], Domains}), ToUser, C_j_data, State
			);
		true ->
			hawk_client_lib:send_message(
				true, hawk_client_lib:get_server_message(<<"send_message">>, ?ERROR_SEND_MESSAGE_YOURSELF), S, Transport
			)
	end;
%% @doc Обработчик отправки сообщения группе
handle_json_message({<<"send_message">>, {undefined, ToGrp}, J_data},
	#state{socket=S, transport=Transport, key=Key} = State) when is_list(ToGrp) ->

	api_action({<<"send_group_message">>, [{<<"key">>, Key}|J_data]}, State, on_output),
	Reply = hawk_client_lib:get_server_message(<<"send_group_message">>, false, ?OK),
	hawk_client_lib:send_message(true, Reply, S, Transport);

%% @doc Обработчик отправки сообщения пользователю и группе одновременно
handle_json_message({<<"send_message">>, {ToUser, ToGrp}, J_data},  State) when is_list(ToGrp) ->
	handle_json_message({<<"send_message">>, {ToUser, undefined}, J_data}, State) ,
	handle_json_message({<<"send_message">>, {undefined, ToGrp}, J_data}, State);

%% @doc Обработчик неверного формата сообщения
handle_json_message({<<"send_message">>, _To, _J_data}, #state{socket=S, transport=Transport}) ->
	hawk_client_lib:send_message(
		true, hawk_client_lib:get_server_message(<<"send_message">>, ?ERROR_INVALID_FORMAT_DATA), S, Transport
	);

%% @doc Возвращает список созданных пользователем групп
handle_json_message({<<"get_group_list">>, _To, J_data}, #state{socket=S, transport=Transport, key=Key}) ->
	Res = get_data_from_worker({
		get_group_list,
		Key,
		?GROUP_ACCESS_PUBLIC,
		proplists:get_value(<<"domains">>, J_data)
	}),

	Reply =	jsx:encode([
		{event, proplists:get_value(<<"event">>, J_data)} | hawk_client_lib:get_server_message(<<"get_group_list">>, false, Res, false)
	]),
	hawk_client_lib:send_message(true, Reply, S, Transport);

%% @doc Возвращает список групп прстого пользователя
handle_json_message({<<"get_group_by_simple_user">>, _To, J_data}, #state{socket=S, transport=Transport, key=Key}) ->
	Res = get_data_from_worker({
		get_group_by_simple_user,
		Key,
		get_user_login(J_data),
		?GROUP_ACCESS_PUBLIC,
		proplists:get_value(<<"domains">>, J_data)
	}),

	Reply =	jsx:encode([
		{event, proplists:get_value(<<"event">>, J_data)}
		| hawk_client_lib:get_server_message(<<"get_group_by_simple_user">>, false, Res, false)
	]),

	hawk_client_lib:send_message(true, Reply, S, Transport);

%% @doc Добавляет пользователя в группу
handle_json_message({<<"add_in_groups">>, _To, J_data}, #state{socket=S, transport=Transport, key=Key}) ->
	Res = get_data_from_worker({
		add_in_groups,
		Key,
		get_user_login(J_data),
		proplists:get_value(<<"groups">>, J_data),
		proplists:get_value(<<"domains">>, J_data),
		?GROUP_ACCESS_PUBLIC
	}),

	Reply =	jsx:encode([{event, proplists:get_value(<<"event">>, J_data)}|jsx:decode(Res)]),
	hawk_client_lib:send_message(true, Reply, S, Transport);

%% @doc Удаляет пользователя из группы
handle_json_message({<<"remove_from_groups">>, _To, J_data}, #state{socket=S, transport=Transport, key=Key}) ->
	Res = get_data_from_worker({
		remove_from_group,
		Key,
		get_user_login(J_data),
		proplists:get_value(<<"groups">>, J_data),
		proplists:get_value(<<"domains">>, J_data),
		?GROUP_ACCESS_PUBLIC
	}),

	Reply =	jsx:encode([{event, proplists:get_value(<<"event">>, J_data)}|jsx:decode(Res)]),
	hawk_client_lib:send_message(true, Reply, S, Transport);

%% @doc Возвращает список пользователей по группам
handle_json_message({<<"get_by_group">>, _To, J_data}, #state{socket=S, transport=Transport, key=Key}) ->
	Res = get_data_from_worker({
		get_by_group,
		Key,
		proplists:get_value(<<"groups">>, J_data),
		proplists:get_value(<<"domains">>, J_data),
		?GROUP_ACCESS_PUBLIC
	}),

	Reply =
		case Res of
			false ->
				hawk_client_lib:get_server_message(<<"get_by_group">>, ?ERROR_INVALID_GROUP_COUNT);
			_ ->
				jsx:encode([
					{event, proplists:get_value(<<"event">>, J_data)} |
					hawk_client_lib:get_server_message(<<"get_by_group">>, false, Res, false)
				])
		end,

	hawk_client_lib:send_message(true, Reply, S, Transport);

%% @doc Возвращает статус пользователя online/offline
handle_json_message({<<"is_online">>, _To, J_data}, #state{socket=S, transport=Transport}) ->
	Res = api_action({<<"is_online">>, J_data}),

	Reply = jsx:encode([{event, proplists:get_value(<<"event">>, J_data)} | jsx:decode(Res)]),
	hawk_client_lib:send_message(true, Reply, S, Transport);

handle_json_message({Action, _To, _J_data}, #state{socket=S, transport=Transport}) ->
	hawk_client_lib:send_message(
		true, hawk_client_lib:get_server_message(Action, ?ERROR_INVALID_FORMAT_DATA), S, Transport
	).

%===============================================

%===============================================

-spec handle_user_message(Output :: atom(), Pids :: [pid()], User :: #{}, J_data :: tuple(), State :: #state{} ) -> ok.
%% @doc Ставит сообщение для оффлайн-пользователя в очередь
handle_user_message(Output, [], User, J_data, #state{socket=S, transport=Transport, host_name=Host} = _State) ->
	case Output of
		on_output ->
			Key = {Host, User},
			case hawk_client_queue:add(Key, J_data) of
				false ->
					hawk_client_queue:new(Key, 5),
					hawk_client_queue:add(Key, J_data);
				ok ->
					true
			end,
			hawk_client_lib:send_message(true, hawk_client_lib:get_server_message(<<"send_message">>, ?ERROR_USER_NOT_ONLINE), S, Transport);
		_ ->
			true
	end;
%% @doc Отправлет сообщение процессам другого пользователя
handle_user_message(Output, Pids, _User, J_data, #state{socket=S, transport=Transport} = _State) ->
	lists:foreach(fun(Pid)->
		Reply = hawk_client_lib:send_message_to_pid(Pid, J_data),
		case Output of
			on_output ->
				hawk_client_lib:send_message(true, jsx:encode([{event, proplists:get_value(<<"event">>, J_data)}|Reply]), S, Transport);
			_ ->
				true
		end
	end, Pids).

%===============================================

%===============================================
%===========ACTION ON USER
%===============================================
-spec api_action({Type :: binary(), J_data :: tuple()}) -> ok.
%% @doc Регистрирует пользователя в системе
api_action({<<"register_user">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Id = proplists:get_value(<<"id">>, J_data),

	case check_login_format(Id) of
		true ->
			get_data_from_worker({register_user, Key, Id});
		false ->
			hawk_client_lib:get_server_message(<<"register_user">>, ?ERROR_INVALID_LOGIN_FORMAT)
	end;

%% @doc Отменяет регистрацию пользователя в системе
api_action({<<"unregister_user">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Id = proplists:get_value(<<"id">>, J_data),
	case check_login_format(Id) of
		true ->
			get_data_from_worker({unregister_user, Key, Id});
		false ->
			hawk_client_lib:get_server_message(<<"unregister_user">>, ?ERROR_INVALID_LOGIN_FORMAT)
	end;

%% @doc Возвращает статус пользователя online/offline
api_action({<<"is_online">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Id = proplists:get_value(<<"id">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	Res = get_data_from_worker({is_online, Key, Id, Domains}),
	jsx:encode(hawk_client_lib:get_server_message(<<"is_online">>, false, Res, false));

%===============================================
%===========ACTION ON CHANELS
%===============================================
%% @doc Регистрирует новый канал
api_action({<<"add_chanel">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Name = proplists:get_value(<<"name">>, J_data),
	Access = proplists:get_value(<<"access">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	get_data_from_worker({add_chanel, Key, Name,  Access, Domains});

%% @doc Удаляет канал
api_action({<<"remove_chanel">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Name = proplists:get_value(<<"name">>, J_data),
	Access = proplists:get_value(<<"access">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	get_data_from_worker({remove_chanel, Key, Name, Access, Domains});

%===============================================
%===========ACTION ON GROUPS
%===============================================
%% @doc Добавляет пользователя в группу
api_action({<<"add_in_groups">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Id = proplists:get_value(<<"id">>, J_data),
	Groups = proplists:get_value(<<"groups">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),

	if
		is_list(Groups) -> get_data_from_worker({add_in_groups, Key, Id, Groups, Domains, ?GROUP_ACCESS_ALL});
		true -> hawk_client_lib:get_server_message(<<"add_in_groups">>, ?ERROR_INVALID_GROUP_FORMAT)
	end;

%% @doc Удаляет пользователя из группы
api_action({<<"remove_from_groups">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Id = proplists:get_value(<<"id">>, J_data),
	Groups = proplists:get_value(<<"groups">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),

	if
		is_list(Groups) -> get_data_from_worker({remove_from_group, Key, Id, Groups, Domains, ?GROUP_ACCESS_ALL});
		true -> hawk_client_lib:get_server_message(<<"remove_from_groups">>, ?ERROR_INVALID_GROUP_FORMAT)
	end;

%% @doc Создаёт новую группу
api_action({<<"add_groups">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Groups = proplists:get_value(<<"groups">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	get_data_from_worker({add_groups, Key, Groups, Domains});

%% @doc Удаляет группу
api_action({<<"remove_groups">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Groups = proplists:get_value(<<"groups">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	get_data_from_worker({remove_groups, Key, Groups, Domains});

%% @doc Возаращает список созданных групп
api_action({<<"get_group_list">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Access = proplists:get_value(<<"access">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	Groups = get_data_from_worker({get_group_list, Key, Access, Domains}),
	jsx:encode(Groups);

%% @doc список групп по простому пользователю
api_action({<<"get_group_by_simple_user">>, J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Access = proplists:get_value(<<"access">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	Login = proplists:get_value(<<"login">>, J_data),

	Groups = get_data_from_worker({get_group_by_simple_user, Key, Login, Access, Domains}),
	jsx:encode(Groups);

%% @doc список пользователей по группам
api_action({<<"get_by_group">>,  J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Groups = proplists:get_value(<<"groups">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),

	if
		is_list(Groups) ->
			case get_data_from_worker({get_by_group, Key, Groups, Domains, ?GROUP_ACCESS_ALL}) of
				false ->
					hawk_client_lib:get_server_message(<<"get_by_group">>, ?ERROR_INVALID_GROUP_COUNT);
				Res ->
					jsx:encode(Res)
			end;
		true ->
			hawk_client_lib:get_server_message(<<"get_by_group">>, ?ERROR_INVALID_GROUP_FORMAT)
	end;

%===============================================
%===========ACTION ON TOKEN
%===============================================
%% @doc Возращает токен аторизации
api_action({<<"get_token">>,  J_data}) ->
	Key = proplists:get_value(<<"key">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	Login = proplists:get_value(<<"login">>, J_data),
	Salt = proplists:get_value(<<"salt">>, J_data),

	Tokens = get_data_from_worker({get_token, Key, Login, Salt, Domains}),
	hawk_client_lib:get_server_message(<<"get_token">>, false, Tokens).

%===============================================
%===========ACTION ON MESSAGE
%===============================================
-spec api_action({Type :: binary(), J_data :: tuple()}, State :: #state{}) -> ok.
%% @doc Отправка сообщения пользователю
api_action({<<"send_message">>, J_data}, #state{key=Key} = State) ->
	To = proplists:get_value(<<"to">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	C_j_data = hawk_client_lib:delete_keys([<<"key">>, <<"domains">>], J_data),

	handle_user_message(off_output, get_data_from_worker({get_pids, Key, [To], Domains}), To, C_j_data, State),
	hawk_client_lib:get_server_message(<<"send_message">>, false, ?OK).

-spec api_action({Type :: binary(), J_data :: tuple()}, State :: #state{}, Output :: atom()) -> ok.
%% @doc Отправка сообщения группе
api_action({<<"send_group_message">>, J_data}, #state{parent=Parent} = State, Output) ->
	Key = proplists:get_value(<<"key">>, J_data),
	From = proplists:get_value(<<"from">>, J_data),
	Text = proplists:get_value(<<"text">>, J_data),
	Domains = proplists:get_value(<<"domains">>, J_data),
	Event = proplists:get_value(<<"event">>, J_data),

	Groups =
		case proplists:get_value(<<"groups">>, J_data) of
			undefined ->
				To = proplists:get_value(<<"to">>, J_data),
				proplists:get_value(<<"group">>, To);
			Gr -> Gr
		end,

	Check =
		if
			Parent =/= <<"post_sup">> ->
				Restriction = ?GROUP_ACCESS_PUBLIC,
				get_data_from_worker({check_user_domains, Key, Domains, Parent});
			true ->
				Restriction = ?GROUP_ACCESS_ALL,
				true
		end,

	if
		is_list(Groups) andalso is_list(Domains) andalso Check == true ->
			Res = get_data_from_worker({get_by_group, Key, Groups, Domains, Restriction}),
			%@todo код крайне не оптимален, для больших групп могут наблюдаться проблемы производительности
			hawk_client_lib:loop_lists(fun([Dom, G, Record]) ->
				Acc = get_access_to_group(G, Dom),

				Allow =
					if
						Acc == false -> false;
						Acc == ?GROUP_ACCESS_PUBLIC -> true;
						Acc == ?GROUP_ACCESS_PRIVATE -> get_data_from_worker({is_user_in_group, Key, From, G, Dom});
						true -> false
					end,

				case Allow of
					true ->
						Group = proplists:get_value(G, Record),
						send_user_message(
							proplists:get_value(users, Group), From, G, Text, Event, Key, Dom, State, Output
						);
					false ->
						hawk_client_lib:get_server_message(<<"send_group_message">>, ?ERROR_ACCESS_DENIED_TO_GROUP)
				end
			end, [Domains, Groups, Res]),

			hawk_client_lib:get_server_message(<<"send_group_message">>, false, ?OK);
		true ->
			hawk_client_lib:get_server_message(<<"send_group_message">>, ?ERROR_INVALID_GROUP_FORMAT)
	end.

send_user_message(Users, From, G, Text, Event, Key, Dom, State, Output) ->
	send_user_message(Users, From, G, Text, Event, Key, Dom, State, Output, []).

send_user_message([], _From, _G, _Text, _Event, _Key, _Dom, _State, _Output, _Sended) -> true;

send_user_message([U|T] = _Users, From,  G, Text, Event, Key, Dom, State, Output, Sended) ->
	Ulogin = proplists:get_value(user, U),
	O = proplists:get_value(online, U),
	NewSended = if
		O ->
			To_data = [{from, From}, {to_user, Ulogin}, {to_group, G}, {text, Text}, {event, Event}],
			Ex = lists:member(Ulogin, Sended),
			if
				Ex == true -> true;
				true ->	handle_user_message(
					Output,
					get_data_from_worker({get_pids, Key, [Ulogin], [Dom]}),
					Ulogin,
					To_data,
					State
				),
				[Ulogin|Sended]
			end;
		true -> Sended
	end,
	send_user_message(T, From, G, Text, Event, Key, Dom, State, Output, NewSended).

%===============================================

handle_event(Event, StateName, StateData) -> {stop, {StateName, undefined_event, Event}, StateData}.
handle_sync_event(Event, _From, StateName, StateData) -> {stop, {StateName, undefined_event, Event}, StateData}.

-spec handle_info({Protocol :: binary(), Socket :: tuple(), Bin :: binary()}, StateName :: atom(), StateData :: #state{}) ->
	{next_state, 'WAIT_FOR_DATA', State :: #state{}}.
%% @doc Запускает основной цикл обработки пользовательских сообщений
%% вызывается из hawk_client_listener:handle_info/2
handle_info({?PROTOCOL, Socket, Bin}, StateName, #state{socket=Socket, transport=Transport} = StateData) ->
	Transport:setopts(Socket, [{active, once}]),
	?MODULE:StateName({data, Bin}, StateData);

handle_info({?PROTOCOL_CLOSE, Socket}, _StateName,
	#state{socket=Socket} = StateData) ->
	{stop, normal, StateData};

handle_info(Data, StateName, StateData) -> ?MODULE:StateName(Data, StateData).

terminate(_Reason, _StateName, #state{socket=Socket, transport=Transport}) ->
	(catch Transport:close(Socket)),
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

-spec get_awsw_key(Key :: binary()) -> {ok, binary()}.
%% @doc Формирует ответ для handshake запроса
get_awsw_key(Key) ->
	Key1 = [Key, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"],
	<<Mac/binary>> = crypto:hash(sha, list_to_binary(Key1)),

	B_prot = <<"HTTP/1.1 101 Switching Protocols\r\n">>,
	B_upg = <<"Upgrade: WebSocket\r\n">>,
	B_conn = <<"Connection: Upgrade\r\n">>,
	Answ_key = [<<"Sec-WebSocket-Accept: ">>, base64:encode(Mac), <<"\r\n\r\n">>],
	B_key = list_to_binary(Answ_key),

	B_all_answ = <<B_prot/binary, B_upg/binary, B_conn/binary, B_key/binary>>,
	{ok, B_all_answ}.

-spec unmask(Payload :: integer(), Masking :: binary()) -> binary().
%% @doc Расшифровка web-socket сообщения
unmask(Payload, Masking) ->
	unmask(Payload, Masking, <<>>).

-spec unmask(Payload :: integer(), Masking :: binary(), Acc :: binary()) -> binary().
%% @doc Расшифровка web-socket сообщения
unmask(Payload, Masking = <<MA:8, MB:8, MC:8, MD:8>>, Acc) ->
	case size(Payload) of
		0 -> Acc;
		1 ->
			<<A:8>> = Payload,
			<<Acc/binary, (MA bxor A)>>;
		2 ->
			<<A:8, B:8>> = Payload,
			<<Acc/binary, (MA bxor A), (MB bxor B)>>;
		3 ->
			<<A:8, B:8, C:8>> = Payload,
			<<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
		_Other ->
			<<A:8, B:8, C:8, D:8, Rest/binary>> = Payload,
			Acc1 = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
			unmask(Rest, Masking, Acc1)
	end.

-spec handle_data(Data :: integer()) -> {ok, binary()}.
%% @doc Расшифровка web-socket сообщения
handle_data(Data) ->
	<<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, Len:7, Rest/binary>> = Data,
	%Len2 = size(Data),
	if
		(Len >= 126) and (Len =< 65535) ->
			<<Dop_len:16/unsigned-integer, Masking:4/binary, Payload:Dop_len/binary, _Next/binary>> = Rest;
		Len > 65536 ->
			<<Dop_len:64/unsigned-integer, Masking:4/binary, Payload:Dop_len/binary, _Next/binary>> = Rest;
		true ->
			_Dop_len = 0,
			<<Masking:4/binary, Payload:Len/binary, _Next/binary>> = Rest
	end,

	Line = unmask(Payload, Masking),
	{ok, Line}.

-spec check_login_format(Data :: binary()) -> boolean().
%% @doc Проверка формата логина
check_login_format(Data) ->
	case re:run(Data, "^[a-zA-Z0-9]{3,64}$") of
		{match, _} ->
			true;
		nomatch ->
			false
	end.

-spec get_access_to_group(G :: binary(), Dom :: binary()) -> binary() | false.
%% @doc Возвращает тип доступа к группе
get_access_to_group(G, Dom) ->
	case dets:lookup(groups_to_user, {G, Dom}) of
		[] -> false;
		[{_Key, _Users, Access}] -> Access
	end.

-spec get_to_from_record(J_data :: tuple()) -> {U :: binary(), Gr :: binary()}.
%% @doc Определяет кому направлено сообщение,
%% группе или одному пользователю
get_to_from_record(J_data) ->
	case proplists:get_value(<<"to">>, J_data) of
		undefined ->
			Gr = undefined,
			U = undefined;
		To ->
			Gr = proplists:get_value(<<"group">>, To),
			U = proplists:get_value(<<"user">>, To)
	end,
	{U, Gr}.

-spec compare_token(Key :: binary(), Token :: binary()) -> boolean().
%% @doc Сравнивает два токена
compare_token(Key, Token) ->
	case ets:lookup(token_storage, Key) of
		[] -> false;
		[{_, Token}] -> true;
		_ -> false
	end.

-spec get_user_login(_data :: tuple()) -> binary().
%% @doc Возвращает логин пользователя из исходных данных
get_user_login(J_data)->
	case proplists:get_value(<<"id">>, J_data) of
		undefined -> proplists:get_value(<<"from">>, J_data);
		L -> L
	end.