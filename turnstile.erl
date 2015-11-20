-module(turnstile).
-export([start/0, init/1, coin/0, push/0]).

-include_lib("eunit/include/eunit.hrl").

-define(TURNSTILE_INSTANCE_NAME, turnstile).

start() ->
	Pid = spawn(turnstile, init, [locked]),
	register(?TURNSTILE_INSTANCE_NAME, Pid).

init(InitialState) ->
	io:format("Starting turnstile with ~s state~n", [InitialState]),
	loop(InitialState).

loop(State) ->
	receive
		{From, coin} -> UpdatedState = server_handle_coin(From, State);
		{From, push} -> UpdatedState = server_handle_push(From, State)
	end,
	loop(UpdatedState).

server_tell(To, Message) ->
	To ! {?TURNSTILE_INSTANCE_NAME, Message}.

may_coin_action(State) ->
	case State of
		locked -> {ok, unlocked};
		_ -> {error, invalid_action}
	end.

may_push_action(State) ->
	case State of
		unlocked -> {ok, locked};
		_ -> {error, invalid_action}
	end.

server_handle_coin(From, CurrentState) ->
	case may_coin_action(CurrentState) of
		{ok, NewState} ->
		       server_tell(From, io_lib:format("State changed to ~s", [NewState])),
			NewState;
		{error, invalid_action} -> server_tell(From, "Hey, If you place a coin when the turnstile is unlocked, you loose your coin."),
			     CurrentState
	end.

server_handle_push(From, CurrentState) ->
	case may_push_action(CurrentState) of 
		{ok, NewState} ->
			server_tell(From, io_lib:format("State changed to ~s", [NewState])),
			NewState;
		{error, invalid_action} -> server_tell(From, "Hey, you need to place a coin to pass this turnstile! Please look at your pocket!"),
		     CurrentState
	end.

ask(Message) ->
	?TURNSTILE_INSTANCE_NAME ! {self(), Message},
	await_result().

await_result() ->
	receive
		{?TURNSTILE_INSTANCE_NAME, Message} -> io:format("Response from turnstile was: ~s~n", [Message])
	end.

coin() -> ask(coin).

push() -> ask(push).

%% Unit tests
a_coin_action_should_unlock_a_locked_turnstile_test() ->
	State = locked,
	{ok, NewState} = may_coin_action(State),
	?assertEqual(NewState, unlocked).

a_coin_action_should_be_ignored_on_an_unlocked_turnstile_test() ->
	State = unlocked,
	Result = may_coin_action(State),
	?assertEqual(Result, {error, invalid_action}).

a_push_action_should_lock_an_unlocked_turnstile_test() ->
	State = unlocked,
	{ok, NewState} = may_push_action(State),
	?assertEqual(NewState, locked).

a_push_action_should_be_ignored_on_a_locked_turnstile_test() ->
	State = locked,
	Result = may_push_action(State),
	?assertEqual(Result, {error, invalid_action}).

