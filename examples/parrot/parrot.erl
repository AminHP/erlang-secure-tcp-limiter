-module(parrot).
-export([start/0, start/1]).


start() -> start(5000).

start(Port) ->
	stl:start(),
	{ok, Listen} = stl:listen(Port, [{certfile, "certs/cert.pem"}, {keyfile, "certs/key.pem"}, {reuseaddr, true}]), 
					%%- defult value for limit type and riak addr are `default_limit` and `{"127.0.0.1", 8087}` 
	%{ok, Listen} = stl:listen(Port, [{certfile, "certs/cert.pem"}, {keyfile, "certs/key.pem"}, {reuseaddr, true}], no_limit, {"127.0.0.1", 10087}),
	%{ok, Listen} = stl:listen(Port, [{certfile, "certs/cert.pem"}, {keyfile, "certs/key.pem"}, {reuseaddr, true}], {func, fun(_PerDay, _PerHour, _PerMin, _PerSec) -> 3 end}),
	spawn(fun() -> loop(Listen) end),
	ok.


loop(Listen) ->
	T = stl:transport_accept(Listen),
	case T of
		{ok, Socket} ->
			{ok, Socket1} = stl:stl_accept(Socket),
			Pid = spawn(fun() -> handler(Socket1) end),
			ok = stl:controlling_process(Socket1, Pid),
			loop(Listen);
		{error, closed} ->
			closed;
		{error, Reason} ->
			io:format("error in accepting: ~p~n", [Reason]),
			loop(Listen)
	end.


handler(Socket) ->
	receive
		{stl, Socket, Data} ->
			stl:send(Socket, Data),
			handler(Socket);
		{stl_closed, Socket} ->
			stl:close(Socket)
	end.
