-module(stl_limiter).
-export([update_log/3,
		 is_block/1]).

-include("stl.hrl").


%% Riak Models.
-record(count, {per_day, per_hour, per_minute, per_second}).
-record(log, {last_req_time, req_count, blocked_to}).

-define(stl_bucket, <<"stl">>).


%%------------------------------------------------------------------------------
%% @doc Updates log in database.
-spec update_log(Ip::binary(), LimitType, {RiakIp, RiakPort}) -> {ok, BlockTime::number()} when
		LimitType :: limits(),
		RiakIp :: string(),
		RiakPort :: integer().

update_log(Ip, LimitType, {RiakIp, RiakPort}) ->
	{ok, Pid} = riakc_pb_socket:start_link(RiakIp, RiakPort),
	Now = timestamp(),
	case riakc_pb_socket:get(Pid, ?stl_bucket, Ip) of
		{ok, FetchedLog} ->
			Log = binary_to_term(riakc_obj:get_value(FetchedLog)),
			Count = Log#log.req_count,
			UpdatedCount = update_count(Count, Now, Log#log.last_req_time),
			BlockTime = calc_block_time(UpdatedCount, LimitType),
			BlockedTo = Now + BlockTime,
			NewLog = Log#log{last_req_time=Now, req_count=UpdatedCount, blocked_to=BlockedTo},
			UpdatedLogObj = riakc_obj:update_value(FetchedLog, NewLog),
			riakc_pb_socket:put(Pid, UpdatedLogObj);

		{error, notfound} ->
			Count = #count{per_day=1, per_hour=1, per_minute=1, per_second=1},
			Log = #log{last_req_time=Now, req_count=Count, blocked_to=Now},
			Object = riakc_obj:new(?stl_bucket, Ip, Log),
			riakc_pb_socket:put(Pid, Object),
			BlockTime = 0
	end,
	{ok, BlockTime}.


%%------------------------------------------------------------------------------
%% @doc Tells whether the ip blocked or not.
-spec is_block(Ip::binary()) -> boolean().

is_block(Ip) ->
	{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
	case riakc_pb_socket:get(Pid, ?stl_bucket, Ip) of
		{ok, FetchedLog} ->
			Log = binary_to_term(riakc_obj:get_value(FetchedLog)),
			timestamp() < Log#log.blocked_to;
		{error, notfound} ->
			false
	end.


%%------------------------------------------------------------------------------
%%==============================================================================
%%------------------------------------------------------------------------------

update_count(Count, Now, Last) when Now =:= Last ->
	#count{per_day=PerDay, per_hour=PerHour, per_minute=PerMin, per_second=PerSec} = Count,
	Count#count{per_day=PerDay+1, per_hour=PerHour+1, per_minute=PerMin+1, per_second=PerSec+1};

update_count(Count, Now, Last) when Now div 60 =:= Last div 60 ->
	#count{per_day=PerDay, per_hour=PerHour, per_minute=PerMin} = Count,
	Count#count{per_day=PerDay+1, per_hour=PerHour+1, per_minute=PerMin+1, per_second=1};

update_count(Count, Now, Last) when Now div 3600 =:= Last div 3600 ->
	#count{per_day=PerDay, per_hour=PerHour} = Count,
	Count#count{per_day=PerDay+1, per_hour=PerHour+1, per_minute=1, per_second=1};

update_count(Count, Now, Last) when Now div 3600*24 =:= Last div 3600*24 ->
	#count{per_day=PerDay} = Count,
	Count#count{per_day=PerDay+1, per_hour=1, per_minute=1, per_second=1};

update_count(Count, _Now, _Last) ->
	Count#count{per_day=1, per_hour=1, per_minute=1, per_second=1}.

%%------------------------------------------------------------------------------

timestamp() ->
	{MegaSecs, Secs, _} = erlang:timestamp(),
	MegaSecs * 1000000 + Secs.

%%------------------------------------------------------------------------------

default_limit_fun(PerDay, PerHour, PerMin, PerSec) ->
	default_limit_fun(PerDay, PerHour, PerMin, PerSec, 0).

default_limit_fun(PerDay, PerHour, PerMin, PerSec, BlockTime) when PerSec > 3 -> 
	default_limit_fun(PerDay, PerHour, PerMin, 0, BlockTime + 0.5);

default_limit_fun(PerDay, PerHour, PerMin, PerSec, BlockTime) when PerMin > 45 -> 
	default_limit_fun(PerDay, PerHour, 0, PerSec, BlockTime + 60);

default_limit_fun(PerDay, PerHour, PerMin, PerSec, BlockTime) when PerHour > 2500 -> 
	default_limit_fun(PerDay, 0, PerMin, PerSec, BlockTime + 3*3600);

default_limit_fun(PerDay, PerHour, PerMin, PerSec, BlockTime) when PerDay > 2500*6 -> 
	default_limit_fun(0, PerHour, PerMin, PerSec, BlockTime + 30*24*3600);


default_limit_fun(_PerDay, _PerHour, _PerMin, _PerSec, BlockTime) -> BlockTime.

%%------------------------------------------------------------------------------

calc_block_time(#count{per_day=PerDay, per_hour=PerHour, per_minute=PerMin, per_second=PerSec}, LimitType) ->
	case LimitType of
		no_limit ->
			0;
		default_limit ->
			default_limit_fun(PerDay, PerHour, PerMin, PerSec);
		{func, LimiterFun} ->
			LimiterFun(PerDay, PerHour, PerMin, PerSec)
	end.
