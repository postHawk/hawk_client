%% @author Barulin Maxim <mbarulin@gmail.com>
%% @copyright 2016 Barulin Maxim
%% @version 0.0.3
%% @title hawk_client_api_worker
%% @doc Рабочий процесс, обрабатывающий запрос к апи. Вызывается и создаётся hawk_client_api_manager


-module(hawk_client_api_worker).
-behaviour(gen_server).
-include("env.hrl").
-include("mac.hrl").

-export([init/1, start_link/0, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-record(state, {listener, acceptor, module}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
%% @doc Запуск модуля
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% @doc Инициализация модуля.
init([]) ->
	{ok, #state{}}.

-spec handle_cast({{pid(), reference()} | terminate, Params :: any()}, State :: #state{}) -> {noreply, State :: #state{}} | {stop, normal, State :: #state{}}.
%% @doc Обработчик запроса к апи
handle_cast({From, Params}, State) ->
	%проверяем корректность ключа для апи
	%@todo ключ надо брать из параметров окружения,
	%@todo тогда можно не передавать его во всех вызовах апи
	{ok, User} = ?get_user_by_key(),
	Key = element(1, Params),
	case User of
		false ->
			Reply = hawk_client_lib:get_server_message(
				atom_to_binary(Key, utf8),
				?ERROR_INVALID_API_KEY
			);
		_ -> Reply = api_action(Params, User)
	end,

	hawk_client_api_manager:restore_worker(self()),
	gen_server:reply(From, Reply),
	{noreply, State};

handle_cast(terminate, State) -> {stop, normal, State}.

-spec api_action(
	{register_user, _Key :: binary(), Id :: binary()} |
	{unregister_user, _Key :: binary(), Id :: binary()} |
	{add_in_groups, _Key :: binary(), Login:: binary(), Groups :: [binary()], Domains :: [binary()], Restriction :: binary()} |
	{remove_from_group, _Key :: binary(), Login:: binary(), Groups :: [binary()], Domains :: [binary()], Restriction :: binary()} |
	{get_by_group, _Key :: binary(), Groups :: [binary()], Domains :: [binary()], Restriction :: binary()} |
	{add_groups, _Key :: binary(), Groups :: [binary()], Domains :: [binary()]} |
	{remove_groups, _Key :: binary(), Groups :: [binary()], Domains :: [binary()]} |
	{get_group_list, _Key :: binary(), Access :: binary(), Domains :: [binary()]} |
	{get_group_by_simple_user,  _Key :: binary(), Login :: binary(), Access :: binary(), Domains :: [binary()]} |
	{add_chanel, _Key :: binary(), Name :: binary(), Access :: binary(), Domains :: [binary()]} |
	{remove_chanel, _Key :: binary(), Name :: binary(), Domains :: [binary()]} |
	{check_user_by_domain, Key :: binary(), Id :: binary(), Domain :: binary()} |
	{check_user_domains, Key :: binary(), Domains :: [binary()], Login :: binary()} |
	{get_pids, _Key :: binary(), Login :: binary(), Domains :: [binary()]} |
	{is_user_in_group, _Key :: binary(), Login :: binary(), Group :: binary(), Dom :: binary()} |
	{get_token, _Key :: binary(), Login :: binary(), Salt :: binary(), Domains :: [binary()]}, User :: #{}) -> binary() | list().


%% @doc регистрирует логин пользователя в связке с корневым пользователем
api_action({register_user, _Key, Id}, User) ->
	Mlogin = maps:get(<<"login">>, User),
	case dets:lookup(reg_users_data, {Id, Mlogin}) of
		[] ->
			Mlogin = maps:get(<<"login">>, User),
			dets:insert(reg_users_data, {{Id , Mlogin}, Mlogin});
		_ -> true
	end,

	hawk_client_lib:get_server_message(<<"register_user">>, false, ?OK);

%% @doc Удаляет связь пользователя и корневого пользователя
api_action({unregister_user, _Key, Id}, User) ->
	Mlogin = maps:get(<<"login">>, User),
	dets:delete(reg_users_data, {Id, Mlogin}),
	hawk_client_lib:get_server_message(<<"unregister_user">>, false, ?OK);

%% @doc Добавляет пользователя в группу
api_action({add_in_groups, _Key, Login, Groups, Domains, Restriction}, User) ->
	%проверяем что домен пользователя зарегистрирован и подтверждён
	case check_user_domains(maps:get(<<"domain">>, User), Domains) of
		true ->
			Mlogin = maps:get(<<"login">>, User),
			%проверяем, что пользователь зарегистрирован
			case dets:lookup(reg_users_data, {Login, Mlogin}) of
				[] ->
					hawk_client_lib:get_server_message(<<"add_in_groups">>, ?ERROR_USER_NOT_REGISTER);
				_ ->
					add_user_to_group(
						Login, Groups, Domains, maps:get(<<"login">>, User), Restriction
					),
					hawk_client_lib:get_server_message(<<"add_in_groups">>, false, ?OK)
			end;
		false ->
			hawk_client_lib:get_server_message(<<"add_in_groups">>, ?ERROR_DOMAIN_NOT_REGISTER)
	end;

%% @doc Удяляет пользователя из группы
api_action({remove_from_group, _Key, Login, Groups, Domains, Restriction}, User) ->
	case check_user_domains(maps:get(<<"domain">>, User), Domains) of
		true ->
			Mlogin = maps:get(<<"login">>, User),
			case dets:lookup(reg_users_data, {Login, Mlogin}) of
				[] ->
					hawk_client_lib:get_server_message(<<"remove_from_group">>, ?ERROR_USER_NOT_REGISTER);
				_ ->
					remove_user_from_group(Login, Groups, Domains, maps:get(<<"login">>, User), Restriction),
					hawk_client_lib:get_server_message(<<"remove_from_group">>, false, ?OK)
			end;
		false ->
			hawk_client_lib:get_server_message(<<"remove_from_group">>, ?ERROR_DOMAIN_NOT_REGISTER)
	end;

%% @doc Возвращает список пользователей по группе
api_action({get_by_group, _Key, Groups, Domains, Restriction}, User) ->
	get_users_by_groups(Groups, Domains, Restriction, maps:get(<<"login">>, User));

%% @doc Добавляет группу
api_action({add_groups, _Key, Groups, Domains}, User) ->
	add_groups(Groups, Domains, maps:get(<<"login">>, User));

%% @doc Удяляет группу
api_action({remove_groups, _Key, Groups, Domains}, User) ->
	remove_groups(Groups, Domains, maps:get(<<"login">>, User));

%% @doc Возвращает список созданных пользователем групп
api_action({get_group_list, _Key, Access, Domains}, User) ->
	get_group_list(Access, Domains, maps:get(<<"login">>, User));

%% @doc Возвращает список групп простого пользователя
api_action({get_group_by_simple_user, _Key, Login, Access, Domains}, User) ->
	get_group_by_simple_user(Login, Access, Domains, maps:get(<<"login">>, User));

%% @doc Добавляет канал
%@todo функционал не готов
api_action({add_chanel, _Key, Name, Access, Domains}, User) ->
	{ok, User} = ?get_user_by_key(),
	add_chanel(Name, Access, Domains, User);

%% @doc Удяляет канал
%@todo функционал не готов
api_action({remove_chanel, _Key, Name, Domains}, User) ->
	remove_chanel(Name, Domains, User);

%% @doc Проверяет полную регистрацию пользователя на домене
api_action({check_user_by_domain, Key, Id, Domain}, User) ->
	Key = maps:get(<<"key">>, User),
	gen_server:call({global, hawk_server_api_manager}, {check_user_by_domain, Key, Id, Domain}, 30000);

%% @doc Проверяет регистрацию домена в системе
api_action({check_user_domains, Key, Domains, Login}, User) ->
	Key = maps:get(<<"key">>, User),
	gen_server:call({global, hawk_server_api_manager}, {check_user_domains, Key, Domains, Login}, 30000);

%% @doc Возвращает список процессов пользователя
api_action({get_pids, _Key, Login, Domains}, User) ->
	Mlogin = maps:get(<<"login">>, User),
	get_users_pids(Login, Domains, Mlogin);

%% @doc Проверяет находится ли пользователь в группе
api_action({is_user_in_group, _Key, Login, Group, Dom}, _User) ->
	is_user_in_group(Group, Login, Dom);

%% @doc Возвращает токен авторизации
api_action({get_token, _Key, Login, Salt, Domains}, User) ->
	lists:map(fun(Dom) ->
		Token = get_token(Login, Salt, Dom),
		Mlogin = maps:get(<<"login">>, User),
		set_token({Login, Mlogin, Dom}, Token),
		{Dom, Token}
	end, Domains).

-spec add_user_to_group(Login :: binary(), Groups :: [binary()], Domains :: [binary()], MLogin :: binary(), Restriction :: binary()) -> ok.
%% @doc Добавляет пользователя в группу
add_user_to_group(Login, Groups, Domains, MLogin, Restriction) ->
	hawk_client_lib:loop_lists(fun([Dom, Gr]) ->
		Access = add_groups_to_user(Gr, Dom, Login, Restriction),
		%link group to main user
		add_created_groups(MLogin, Dom, Gr, Access, Restriction)
    end, [Domains, Groups]).

-spec add_groups_to_user(Gr :: binary(), Dom :: binary(), Login :: binary(), Restriction :: binary()) -> binary().
%% @doc Добавляет привязку группы к пользвоателю
add_groups_to_user(Gr, Dom, Login, Restriction) ->
	case dets:lookup(groups_to_user, {Gr, Dom}) of
		[] ->
			Acc = ?DEFAULT_GROUP_ACCESS,
			dets:insert(groups_to_user, {{Gr, Dom}, [Login], Acc}),
			add_group_to_user(Login, Dom, Gr),
			Acc;
		[{_, Users, Acc}] -> add_groups_to_user(Login, Users, Restriction, Gr, Dom, Acc)
	end.

-spec add_groups_to_user(Login :: binary(), Users :: [binary()], Restriction :: binary(), Gr :: binary(), Dom :: binary(), Acc :: binary())
		-> binary().
%% @doc Добавляет привязку группы к пользвоателю если пользователь в неё не входит
add_groups_to_user(Login, Users, Restriction, Gr, Dom, Acc) ->
	case lists:member(Login, Users) of
		false ->
			if
				Restriction == ?GROUP_ACCESS_ALL orelse
					(Restriction == ?GROUP_ACCESS_PUBLIC andalso Acc == ?GROUP_ACCESS_PUBLIC) ->
					dets:insert(groups_to_user, {{Gr, Dom}, [Login | Users], Acc}),
					add_group_to_user(Login, Dom, Gr);
				true ->
					true
			end;
		true ->
			true
	end,
	Acc.

-spec add_group_to_user(Login :: binary(), Dom :: binary(), Gr :: binary()) -> ok.
%% @doc Добавляет привязку пользвоателя к группе
add_group_to_user(Login, Dom, Gr) ->
	case dets:lookup(user_to_groups, {Login, Dom}) of
		[] ->
			dets:insert(user_to_groups, {{Login, Dom}, [Gr]});
		[{_, Grps}] ->
			case lists:member(Gr, Grps) of
				false ->
					dets:insert(user_to_groups, {{Login, Dom}, [Gr | Grps]});
				true ->
					ok
			end
	end.

-spec add_created_groups(MLogin :: binary(), Dom :: binary(), Gr :: binary(), Access :: binary(), Restriction :: binary()) -> ok.
%% @doc Добавляет группу в список групп пользователя
add_created_groups(MLogin, Dom, Gr, Access, Restriction) ->
	case dets:lookup(created_groups, {MLogin, Dom}) of
		[] ->
			dets:insert(created_groups, {{MLogin, Dom}, [{Gr, Access}]});
		[{_, Grps}] -> add_created_groups(Grps, Gr, Restriction, Access, MLogin, Dom)
	end.

-spec add_created_groups(Grps :: [binary()], Gr :: binary(), Restriction :: binary(), Access :: binary(), MLogin :: binary(), Dom :: binary())
		-> ok.
%% @doc Добавляет группу в список групп пользователя, если её там нет
add_created_groups(Grps, Gr, Restriction, Access, MLogin, Dom) ->
	NGrps = [GName || {GName, _} <- Grps],
	case lists:member(Gr, NGrps) of
		true ->
			ok;
		false ->
			if
				Restriction == ?GROUP_ACCESS_ALL orelse
					(Restriction == ?GROUP_ACCESS_PUBLIC andalso Access == ?GROUP_ACCESS_PUBLIC) ->
					dets:insert(created_groups, {{MLogin, Dom}, [{Gr, Access} | Grps]});
				true ->
					ok
			end
	end.

-spec remove_user_from_group(Login :: binary(), Groups :: [binary()], Domains :: [binary()], MLogin :: binary(), Restriction :: binary())
		-> ok.
%% @doc Удаляет пользователя из группы
remove_user_from_group(Login, Groups, Domains, MLogin, Restriction) ->
	hawk_client_lib:loop_lists(fun([Dom, Gr]) ->
		case dets:lookup(groups_to_user, {Gr, Dom}) of
			[] ->
				true;
			[{_, Users, Access}] -> remove_user_from_group(Restriction, Access, Login, Users, Gr, Dom, MLogin)
		end
	end, [Domains, Groups]).

-spec remove_user_from_group(Restriction :: binary(), Access :: binary(), Login :: binary(), Users :: [binary()], Gr :: binary(), Dom :: binary(), MLogin :: binary())
		-> ok.
%% @doc Удаляет пользователя из группы, если позволяют права и он там есть
remove_user_from_group(Restriction, Access, Login, Users, Gr, Dom, MLogin) ->
	if
		Restriction == ?GROUP_ACCESS_ALL orelse
			(Restriction == ?GROUP_ACCESS_PUBLIC andalso Access == ?GROUP_ACCESS_PUBLIC) ->
			case lists:delete(Login, Users) of
				[] ->
					%remove empty group
					dets:delete(groups_to_user, {Gr, Dom}),
					remove_created_group(MLogin, Dom, Gr);
				U ->
					dets:insert(groups_to_user, {{Gr, Dom}, U, Access})
			end,
			remove_group_to_user(Login, Dom, Gr);
		true ->
			true
	end.

-spec remove_created_group(MLogin :: binary(), Dom :: binary(), Gr :: binary()) -> ok.
%% @doc Обновляет или удаляет список созданных групп если в там никого не осталось
remove_created_group(MLogin, Dom, Gr)->
	case dets:lookup(created_groups, {MLogin, Dom}) of
		[] -> ok;
		[{_, Grps}] ->
			case hawk_client_lib:delete_keys([Gr], Grps) of
				[]  -> dets:delete(created_groups, {MLogin, Dom});
				L -> dets:insert(created_groups, {{MLogin, Dom}, L})
			end
	end.

-spec remove_group_to_user(Login :: binary(), Dom :: binary(), Gr :: binary()) -> ok.
%% @doc Удаляет привязку пользователя к группе
remove_group_to_user(Login, Dom, Gr) ->
	case dets:lookup(user_to_groups, {Login, Dom}) of
		[] ->
			ok;
		[{_, Grps}] ->
			dets:insert(user_to_groups, {{Login, Dom}, lists:delete(Gr, Grps)})
	end.

-spec get_users_pids(Logins :: [binary()], Domains :: [binary()], Mlogin :: binary()) -> [pid()].
%% @doc Возвращает  список процессов пользователей
get_users_pids(Logins, Domains, Mlogin) when is_list(Logins), is_list(Domains) ->
	Fun = fun(Login) ->
		case dets:lookup(reg_users_data, {Login, Mlogin}) of
			[] -> [];
			_ -> get_pids_by_hosts(Domains, Login)
		end
	end,
	lists:flatten(lists:map(Fun, Logins));

get_users_pids(Login, Domain, Mlogin) -> get_users_pids([Login], [Domain], Mlogin).

-spec get_pids_by_hosts(Hosts :: [binary()], Login :: binary()) -> [pid()] | [].
%% @doc Возвращает  список процессов на всех хостах
get_pids_by_hosts(Hosts, Login) ->
	Fun = fun(Host) ->
		case gproc:lookup_pids({p, l, {Host, Login}}) of
			undefined ->
				[];
			Pid ->
				Pid
		end
	end,
	lists:flatten(lists:map(Fun, Hosts)).

-spec get_users_by_groups(Groups :: [binary()], Domains :: [binary()], Restriction :: binary(), MLogin :: binary()) -> list().
%% @doc Возвращает  список пользователей по группам
get_users_by_groups(Groups, Domains, Restriction, MLogin) when is_list(Groups), is_list(Domains) ->
	Fun = fun(Gr) ->
		[{Gr, [{access, Restriction}, {users, get_users_in_group(record, Gr, Domains, Restriction, MLogin)}]}]
	end,

	List = lists:map(Fun, Groups),
	case hawk_client_lib:list_is_empty(List) of
		true ->
			[];
		false ->
			[L || L <- List, L /= []]
	end;

get_users_by_groups(_, _, _, _) -> false.

-spec get_users_in_group(Type :: record | list, Gr :: binary(), Domains :: [binary()], Restriction :: binary(), MLogin :: binary()) -> list().
%% @doc Возвращает  список пользователей в одной группе
get_users_in_group(Type, Gr, Domains, Restriction, Mlogin) ->
	FunD = fun(Dom) ->
		case dets:lookup(groups_to_user, {Gr, Dom}) of
			[] ->
				[];
			[{_, Users, Access}] -> get_users_in_group(Type, Restriction, Access, Users, Dom, Mlogin)
		end
	end,
	[List] = lists:map(FunD, Domains),

	case hawk_client_lib:list_is_empty(List) of
		true -> [];
		false -> List
	end.

%% @doc Возвращает  список пользователей в заданном формате
get_users_in_group(record, Restriction, Access, Users, Dom, Mlogin) ->
	if
		Restriction == ?GROUP_ACCESS_ALL orelse
			(Restriction == ?GROUP_ACCESS_PUBLIC andalso Access == ?GROUP_ACCESS_PUBLIC) ->
			get_users_records(Users, Dom, Mlogin);
		true -> []
	end;
get_users_in_group(list, _Restriction, _Access, Users, _Dom, _Mlogin) -> Users.

-spec get_users_records(Users :: [binary()], Dom :: binary(), MLogin :: binary()) -> list().
%% @doc Возвращает одну сформированную запись пользователя
get_users_records(Users, Dom, Mlogin) ->
	Fun = fun(U) ->
		[{user, U}, {online, is_user_online(U, Dom, Mlogin)}]
	end,
	lists:map(Fun, Users).

-spec get_group_by_simple_user(Login :: binary(), Access :: binary(), Domains :: [binary()], MLogin :: binary()) -> list().
%% @doc Возвращает  список групп одного пользователя
get_group_by_simple_user(Login, Access, Domains, MLogin) ->
	Groups = get_group_list(Access, Domains, MLogin),
	[UGroups] = lists:map(fun(Dom) ->
		[
			[{name, GName}, {access, Acc}] || [{name, GName}, {access, Acc}] <- Groups, is_user_in_group(GName, Login, Dom)
		]
	end, Domains),
	UGroups.

-spec is_user_in_group(Group :: binary(), Login :: binary(), Dom :: binary()) -> list().
%% @doc Проверяет входит ли пользователь в группу
is_user_in_group(Group, Login, Dom) ->
	case dets:lookup(user_to_groups, {Login, Dom}) of
		[] -> false;
		[{_, Grps}] -> lists:member(Group, Grps)
	end.

-spec is_user_online(U :: binary(), Dom :: binary(), Mlogin :: binary()) -> boolean().
%% @doc Проверяет онлайн ли пользователь
is_user_online(U, Dom, Mlogin) ->
	case get_users_pids(U, Dom, Mlogin) of
		[] -> false;
		_ -> true
	end.

-spec check_user_domains(UDomains :: [binary()], Domains :: [binary()]) -> boolean().
%% @doc Проверяет входит ли домен пользователя в список зарегистрированных
check_user_domains(UDomains, Domains) when is_list(Domains) and is_list(UDomains) ->
	case string:str(lists:sort(UDomains), lists:sort(Domains)) of
		0 -> false;
		_ -> true
	end;

check_user_domains(_UDomains, _Domains) -> false.

-spec add_groups(Groups :: [binary()], Domains :: [binary()], MLogin :: binary()) -> ok.
%% @doc Создание новой группы
add_groups(Groups, Domains, MLogin) ->
	hawk_client_lib:loop_lists(fun([Dom, Gr]) ->
		Access = proplists:get_value(<<"access">>, Gr),
		Name = proplists:get_value(<<"name">>, Gr),
		%add group or change access
		case dets:lookup(groups_to_user, {Name, Dom}) of
			[] -> dets:insert(groups_to_user, {{Name, Dom}, [], Access});
			[{Key, Users, _A}] -> dets:insert(groups_to_user, {Key, Users, Access})
		end,
		%link group to main user
		case dets:lookup(created_groups, {MLogin, Dom}) of
			[] -> dets:insert(created_groups, {{MLogin, Dom}, [{Name, Access}]});
			[{_, Grps}] ->
				NewGrps = [{GName, Acc} || {GName, Acc} <- Grps, GName /= Name],
				dets:insert(created_groups, {{MLogin, Dom}, [{Name, Access} | NewGrps]})
		end
	end, [Domains, Groups]),

	hawk_client_lib:get_server_message(<<"add_groups">>, false, ?OK).

-spec remove_groups(Groups :: [binary()], Domains :: [binary()], MLogin :: binary()) -> ok.
%% @doc Удаление группы
remove_groups(Groups, Domains, MLogin) ->
	lists:foreach(fun(Gr) ->
		Users = lists:flatten(get_users_in_group(list, Gr, Domains, ?GROUP_ACCESS_ALL, MLogin)),
		%delete user group and group
		hawk_client_lib:loop_lists(fun([User, Dom]) ->
			remove_group_to_user(User, Dom, Gr),
			dets:delete(groups_to_user, {Gr, Dom}),
			case dets:lookup(created_groups, {MLogin, Dom}) of
				[] -> true;
				[{_, Grps}] ->
					NewGrps = [{Name, Acc} || {Name, Acc} <- Grps, Name /= Gr],
					dets:insert(created_groups, {{MLogin, Dom}, NewGrps})
			end
		end, [Users, Domains]),

		%send message that group delete
		To_data = [{from, <<"server">>}, {action, <<"group_removed">>}, {group, Gr}],
		lists:foreach(fun(Pid) ->
			hawk_client_lib:send_message_to_pid(Pid, To_data)
		end, get_users_pids(Users, Domains, MLogin))
	end, Groups),
	hawk_client_lib:get_server_message(<<"remove_groups">>, false, ?OK).

-spec get_group_list(Access :: binary(), Domains :: [binary()], MLogin :: binary()) -> ok.
%% @doc Список групп
get_group_list(Access, Domains, MLogin) ->
	Groups = lists:map(fun(Dom) ->
		case dets:lookup(created_groups, {MLogin, Dom}) of
			[] -> [];
			[{_, Grps}] ->
				case Access of
					?GROUP_ACCESS_ALL -> [[{name, Name}, {access, Acc}] || {Name, Acc} <- Grps];
					_ -> [[{name, Name}, {access, Acc}] || {Name, Acc} <- Grps, Acc == Access]
				end
		end
	end, Domains),

	hd(Groups).

-spec add_chanel(Name :: binary(), Access :: binary(), Domains :: [binary()], User :: #{}) -> ok.
%% @doc Создание нового канала
add_chanel(Name, Access, Domains, User) -> add_groups([{<<"name">>, {chanel, Name}}, {<<"access">>, Access}], Domains, User).

-spec remove_chanel(Name :: binary(), Domains :: [binary()], User :: #{}) -> ok.
%% @doc Удаление канала
remove_chanel(Name, Domains, User) -> remove_groups([{chanel, Name}], Domains, User).

-spec get_token(Login :: binary(), Salt :: binary(), Dom :: binary()) -> binary().
%% @doc Формирование токена авторизации
get_token(Login, Salt, Dom) ->
	<<Mac/binary>> = crypto:hash(sha, list_to_binary([Login, Salt, Dom])),
	base64:encode(Mac).

-spec set_token(Key :: binary(), Token :: binary()) -> binary().
%% @doc Запись токена авторизации
set_token(Key, Token) ->
	ets:insert(token_storage, {Key, Token}).

handle_info(_Data, State) -> {noreply, State}.
handle_call(_Msg, _From, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.