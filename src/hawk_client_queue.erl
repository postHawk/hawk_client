%% @author Barulin Maxim <mbarulin@gmail.com>
%% @copyright 2016 Barulin Maxim
%% @version 0.0.3
%% @title hawk_client_queue
%% @doc Простая реализация стека для хранения данных


-module(hawk_client_queue).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([new/2, add/2, insert/2, get/1, shift/1, size/1, clear/1, delete/1]).

-record(state, {}).
%% @doc Создание нового стека
new(Name, Length) -> gen_server:call(?MODULE, {new, Name, Length}).
%% @doc Добавление значения в заданный стек.
%% Если размер превышен, то будет удалён первый элемент
add(Name, NData) -> gen_server:call(?MODULE, {add, Name, NData}).
%% @doc Добавление значения в заданный стек.
%% Если размер превышен, то добавление будет проигнорировано
insert(Name, NData) -> gen_server:call(?MODULE, {insert, Name, NData}).
%% @doc Возвращает заданный стек
get(Name) -> gen_server:call(?MODULE, {get, Name}).
%% @doc Возвращает первый элемент заданного стека и удаляет его.
shift(Name) -> gen_server:call(?MODULE, {shift, Name}).
%% @doc Очищает заданный стек
clear(Name) -> gen_server:call(?MODULE, {clear, Name}).
%% @doc Удаляет заданный стек
delete(Name) -> gen_server:call(?MODULE, {delete, Name}).
%% @doc Возвращает размер заданного стека
size(Name) -> gen_server:call(?MODULE, {size, Name}).

%% @doc Инициализация модуля
init([]) ->
	ok = filelib:ensure_dir("data/message_queue"),
	dets:open_file(message_queue, [{access, read_write}, {type, set}, {auto_save, 10000}, {file, "data/message_queue"}]),
    {ok, #state{}}.

%% @doc Старт модуля
start_link()  ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Создание нового стека
handle_call({new, Name, Length}, _From, State) ->
	dets:insert_new(message_queue, {Name, Length, []}),
    {reply, ok, State};
%% @doc Добавление значения в заданный стек.
%% Если размер превышен, то будет удалён первый элемент
handle_call({add, Name, NData}, _From, State) ->
    Reply = case dets:lookup(message_queue, Name) of
		[]                     -> false;
		[{Name, Length, Data}] ->
			CLen = length(Data),
			NewData = if 
				CLen > Length ->
					[_H|T] = Data,
					lists:reverse([NData|lists:reverse(T)]);
				true -> [NData|Data]
			end,
			dets:insert(message_queue, {Name, Length, NewData})
	end,
    {reply, Reply, State};

%% @doc Добавление значения в заданный стек.
%% Если размер превышен, то добавление будет проигнорировано
handle_call({insert, Name, NData}, _From, State) ->
    Reply = case dets:lookup(message_queue, Name) of
		[]                     -> false;
		[{Name, Length, Data}] ->
			CLen = length(Data),
			if 
				CLen > Length -> false;
				true          -> dets:insert(message_queue, {Name, Length, [NData|Data]})
			end
	end,
    {reply, Reply, State};

%% @doc Возвращает заданный стек
handle_call({get, Name}, _From, State) ->
    Reply = dets:lookup(message_queue, Name),
    {reply, Reply, State};

%% @doc Возвращает первый элемент заданного стека и удаляет его.
handle_call({shift, Name}, _From, State) ->
    Reply = case dets:lookup(message_queue, Name) of
		[]                     -> [];
		[{Name, _Length, []}]  -> [];
		[{Name, Length, Data}] ->
			[H|T] = Data,
			dets:insert(message_queue, {Name, Length, T}),
			H
	end,
    {reply, Reply, State};

%% @doc Очищает заданный стек
handle_call({clear, Name}, _From, State) ->
    Reply = case dets:lookup(message_queue, Name) of
		[]                      -> false;
		[{Name, Length, _Data}] -> dets:insert(message_queue, {Name, Length, []})
	end,
    {reply, Reply, State};

%% @doc Удаляет заданный стек
handle_call({delete, Name}, _From, State) ->
    dets:delete(message_queue, Name),
    {reply, ok, State};

%% @doc Возвращает размер заданного стека
handle_call({size, Name}, _From, State) ->
    Reply = case dets:lookup(message_queue, Name) of
		[]                      -> 0;
		[{Name, _Length, Data}] -> length(Data)
	end,
    {reply, Reply, State}.

handle_cast(_, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


