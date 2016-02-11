-module(stl).
-export([start/0, start/1,
		 stop/0,
		 close/1, close/2,
		 shutdown/2,
		 send/2,
		 peername/1,
		 listen/2, listen/3, listen/4,
		 transport_accept/1, transport_accept/2,
		 controlling_process/2,
		 stl_accept/1, stl_accept/2, stl_accept/3]).


-include("stl.hrl").


%%------------------------------------------------------------------------------
%% @doc Starts the SSL application.
-spec start() -> ok | {error, Reason::term()}.

start() -> ssl:start().


%%------------------------------------------------------------------------------
%% @doc Starts the SSL application by the passed type.
-spec start(Type) -> ok | {error, Reason::term()} when
		Type :: permanent | transient | temporary.

start(Type) -> ssl:start(Type).


%%------------------------------------------------------------------------------
%% @doc Stops the SSL application.
-spec stop() -> ok.

stop() -> ssl:stop().


%%------------------------------------------------------------------------------
%% @doc Closes a connection.
-spec close(StlSocket) -> ok | {error, Reason::term()} when
		StlSocket :: stlsocket().

close(#stlsocket{sslsocket=SslSocket}) -> ssl:close(SslSocket).


%%------------------------------------------------------------------------------
%% @doc Closes a connection.
-spec close(StlSocket, How) -> ok | {ok, port()} | {error, Reason::term()} when
		StlSocket :: stlsocket(),
		How :: timeout() | {pid(), timeout()}.

close(#stlsocket{sslsocket=SslSocket}, How) -> ssl:close(SslSocket, How).


%%------------------------------------------------------------------------------
%% @doc Immediately closes a socket in one or two directions.
-spec shutdown(Socket, How) -> ok | {error, Reason} when
		Socket :: stlsocket(),
		How :: read | write | read_write,
		Reason :: ssl:reason().

shutdown(#stlsocket{sslsocket=Socket}, How) -> ssl:shutdown(Socket, How).


%%------------------------------------------------------------------------------
%% @doc Writes `Data` to `Socket`.
-spec send(Socket, Data) -> ok | {error, Reason::term()} when
		Socket :: stlsocket(),
		Data :: iodata().

send(#stlsocket{sslsocket=Socket}, Data) -> ssl:send(Socket, Data).


%%------------------------------------------------------------------------------
%% @doc Returns the address and port number of the peer.
-spec peername(Socket) -> {ok, {Address, Port}} | {error, Reason::term()} when
		Socket :: stlsocket(),
		Address :: ssl:ipaddress(),
		Port :: integer().

peername(#stlsocket{sslsocket=Socket}) -> ssl:peername(Socket).


%%------------------------------------------------------------------------------
%% @doc Creates an STL listen socket with default limiter and riak options.
-spec listen(Port, Options) -> {ok, ListenSocket} | {error, Reason::term()} when
		Port :: integer(),
		Options :: ssl:ssloptions(),
		ListenSocket :: stlsocket().

listen(Port, Options) ->
	listen(Port, Options, default_limit, {"127.0.0.1", 8087}).


%%------------------------------------------------------------------------------
%% @doc Creates an STL listen socket with default riak options.
-spec listen(Port, Options, LimitType) -> {ok, ListenSocket} | {error, Reason::term()} when
		Port :: integer(),
		Options :: ssl:ssloptions(),
		LimitType :: limits(),
		ListenSocket :: stlsocket().

listen(Port, Options, LimitType) ->
	listen(Port, Options, LimitType, {"127.0.0.1", 8087}).


%%------------------------------------------------------------------------------
%% @doc Creates an STL listen socket.
-spec listen(Port, Options, LimitType, {RiakIp, RiakPort}) -> {ok, ListenSocket} | {error, Reason::term()} when
		Port :: integer(),
		Options :: ssl:ssloptions(),
		LimitType :: limits(),
		RiakIp :: string(),
		RiakPort :: integer(),
		ListenSocket :: stlsocket().

listen(Port, Options, LimitType, Riak = {RiakIp, RiakPort}) when (is_list(RiakIp) andalso is_integer(RiakPort)) ->
	Result = ssl:listen(Port, [{active, true} | Options]),
	case Result of
		{ok, Socket} ->
			{ok, #stlsocket{sslsocket=Socket, lt=validate_limit(LimitType), riak=Riak}};
		Error = {error, _Reason} -> Error
	end.


%%------------------------------------------------------------------------------
%% @doc Accepts an incoming connection request on a listen socket.
-spec transport_accept(ListenSocket) -> {ok, NewSocket} | {error, Reason} when
		ListenSocket :: stlsocket(),
		NewSocket :: stlsocket(),
		Reason :: ssl:reason().

transport_accept(ListenSocket) -> transport_accept(ListenSocket, infinity).


%%------------------------------------------------------------------------------
%% @doc Accepts an incoming connection request on a listen socket with timeout.
-spec transport_accept(ListenSocket, Timeout) -> {ok, NewSocket} | {error, Reason} when
		ListenSocket :: stlsocket(),
		NewSocket :: stlsocket(),
		Timeout :: integer(),
		Reason :: ssl:reason() | blocked.

transport_accept(ListenSocket = #stlsocket{sslsocket=SslListenSocket}, Timeout) ->
	case ssl:transport_accept(SslListenSocket, Timeout) of
		{ok, NewSocket} ->
			Sock = ListenSocket#stlsocket{sslsocket=NewSocket},
			case is_block_ip(NewSocket) of
				true ->
					Res = {error, {blocked_ip, get_ip(NewSocket)}},
					close(Sock),
					Res;
				false ->
					{ok, Sock}
			end;
		Error = {error, _Reason} -> Error
	end.


%%------------------------------------------------------------------------------
%% @doc Assigns a new controlling process to the STL socket.
-spec controlling_process(StlSocket, NewOwner) -> ok when
		StlSocket :: stlsocket(),
		NewOwner :: pid().

controlling_process(#stlsocket{pid=Pid}, NewOwner) ->
	Pid ! {new_owner, NewOwner},
	ok.


%%------------------------------------------------------------------------------
%% @doc Performs the SSL/TLS server-side handshake.
-spec stl_accept(Socket) -> {ok, Socket} | {error, Reason::term()} when
	Socket :: stlsocket().

-spec stl_accept(Socket, Timeout | SslOptons) -> {ok, Socket} | {error, Reason::term()} when
	Socket :: stlsocket(),
	Timeout :: integer(),
	SslOptons :: ssl:ssloptions().

-spec stl_accept(Socket, SslOptons, Timeout) -> {ok, Socket} | {error, Reason::term()} when
	Socket :: stlsocket(),
	SslOptons :: ssl:ssloptions(),
	Timeout :: integer().


stl_accept(Socket = #stlsocket{sslsocket=SslSocket}) ->
	init(
		 Socket, 
		 ssl:ssl_accept(SslSocket)).

stl_accept(Socket = #stlsocket{sslsocket=SslSocket}, Timeout) when is_integer(Timeout) ->
	init(
		 Socket,
		 ssl:ssl_accept(SslSocket, Timeout));

stl_accept(Socket = #stlsocket{sslsocket=SslSocket}, SslOptons) ->
	init(
		 Socket,
		 ssl:ssl_accept(SslSocket, SslOptons)).

stl_accept(Socket = #stlsocket{sslsocket=SslSocket}, SslOptons, Timeout) ->
	init(
		 Socket,
		 ssl:ssl_accept(SslSocket, SslOptons, Timeout)).


%%------------------------------------------------------------------------------
%%==============================================================================
%%------------------------------------------------------------------------------


init(Socket = #stlsocket{sslsocket=SslSocket}, AcceptationResult) ->
	OwnerPid = self(),
	case AcceptationResult of
		ok ->
			ssl:setopts(SslSocket, [{active, once}]),
			Pid = spawn(fun() -> controller(OwnerPid) end),
			ssl:controlling_process(SslSocket, Pid),
			FinalSocket = Socket#stlsocket{sslsocket=SslSocket, pid=Pid},
			Pid ! FinalSocket,
			{ok, FinalSocket};

		{ok, NewSocket} ->
			ssl:setopts(NewSocket, [{active, once}]),
			Pid = spawn(fun() -> controller(OwnerPid) end),
			ssl:controlling_process(NewSocket, Pid),
			FinalSocket = Socket#stlsocket{sslsocket=NewSocket, pid=Pid},
			Pid ! FinalSocket,
			{ok, FinalSocket};

		Error = {error, _Reason} -> Error
	end.

%%------------------------------------------------------------------------------

controller(Owner) ->
	receive
		StlSocket -> controller(StlSocket, Owner)
	end.

controller(StlSocket = #stlsocket{sslsocket=SslSocket}, Owner) ->
	receive
		{ssl, SslSocket, Data} ->
			Owner ! {stl, StlSocket, Data},
			{ok, BlockTime} = update_log(StlSocket),
			spawn(fun() -> 
							timer:sleep(erlang:trunc(BlockTime * 1000)),
							ssl:setopts(SslSocket, [{active, once}])
				  end),
			controller(StlSocket, Owner);

		{ssl_closed, SslSocket} ->
			Owner ! {stl_closed, StlSocket},
			ssl:close(SslSocket);


		{new_owner, NewOwner} ->
			controller(StlSocket, NewOwner)
	end.

%%------------------------------------------------------------------------------

get_ip(SslSocket) ->
	{ok, {PeerIp, _}} = ssl:peername(SslSocket),
	Ip = list_to_binary(tuple_to_list(PeerIp)),
	Ip.

%%------------------------------------------------------------------------------

is_block_ip(SslSocket) ->
	Ip = get_ip(SslSocket),
	stl_limiter:is_block(Ip).

%%------------------------------------------------------------------------------

update_log(#stlsocket{sslsocket=SslSocket, lt=LimitType, riak=Riak}) ->
	Ip = get_ip(SslSocket),
	stl_limiter:update_log(Ip, LimitType, Riak).

%%------------------------------------------------------------------------------

validate_limit(no_limit=LimitType) -> LimitType;
validate_limit(default_limit=LimitType) -> LimitType;
validate_limit({func, Fun}=LimitType) when is_function(Fun, 4)-> LimitType.
