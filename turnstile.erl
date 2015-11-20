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

may_coin_action(locked) -> {ok, unlocked};
may_coin_action(unlocked) -> {error, invalid_action}.

may_push_action(unlocked) -> {ok, locked};
may_push_action(locked) -> {error, invalid_action}.

server_handle_coin(From, CurrentState) ->
	Result = may_coin_action(CurrentState),
	server_handle_coin_result(Result, From, CurrentState).

server_handle_coin_result({ok, NewState}, From, _LastState) ->
	server_tell(From, io_lib:format("State changed to ~s", [NewState])),
	NewState;
server_handle_coin_result({error, invalid_action}, From, LastState) ->
	server_tell(From, "Hey, if you place a coin when the turnstile is unlocked, you loose your coin."),
	LastState.

server_handle_push(From, CurrentState) ->
	Result = may_push_action(CurrentState),
	server_handle_push_result(Result, From, CurrentState).

server_handle_push_result({ok, NewState}, From, _LastState) ->
	server_tell(From, io_lib:format("State changed to ~s", [NewState])),
	NewState;
server_handle_push_result({error, invalid_action}, From, LastState) -> server_tell(From, "Hey, you need to place a coin to pass this turnstile! Please look at your pocket!"),
	LastState.

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

