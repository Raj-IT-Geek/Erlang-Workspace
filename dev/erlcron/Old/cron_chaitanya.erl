

handle_job(DateTime, #cron{name = _Name,
						function = {M, F, A},
						schedule = Schedule}) ->
	case is_schedule_now(DateTime, Schedule) of
		false ->
			ok;
		true ->
			spawn_link(?MODULE, run_job, [M, F, A])
	end.

is_schedule_now({{_YYYY, MM, DD} = Date, {H, M, _S}},
			           {Min, Hour, DayOfMonth, Month, DayOfWeek}) ->
	Day = calendar:day_of_the_week(Date),
	lists:all(
		fun try_match/1,
		    [ {M, Min}, {H, Hour}, {DD, DayOfMonth}, {MM, Month}, {Day, DayOfWeek} ]
	).

try_match({_, ‘_’}) -> true;
try_match({X, X}) -> true;
try_match({X, [$/|Vs]}) ->
	case catch list_to_integer(Vs) of
		{‘EXIT’, _Reason} ->
			false;
		V ->
			if
				X rem V ==0 -> true;
				true -> false;
			end
	end;
try_match({X,L}) when is_list(L) ->
	lists:member(X, L);
try_match(_) ->
	false.

run_job(M, F, A) ->
	case catch erlang:apply(M, F, A) of
		{‘EXIT’ Reason} ->
			lager:info(“Oops! your function ~p crashed ~p~n”, [{M, F, A}, Reason]);
		Other ->
			lager:info(“response from job is ~p”, [Other])
	end.
