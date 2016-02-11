-ifndef(stl).
-define(stl, true).

-export_type([stlsocket/0,
			  limits/0]).


-record(stlsocket, {sslsocket = nil, pid = nil, lt = nil, riak = nil}).
-type stlsocket() :: #stlsocket{}.

-type limits() :: no_limit | default_limit | {func, function()}.

-endif. % -ifdef(stl_api).
