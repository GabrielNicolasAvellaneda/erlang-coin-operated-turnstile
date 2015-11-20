-module(turnstile).
-export([start/0, init/1, coin/0, push/0]).

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

server_handle_coin(From, State) ->
	case State of
		locked ->
		        server_tell(From, "State changed to unlocked"), 	
			unlocked;
		_ -> server_tell(From, "Hey, If you place a coin when the turnstile is unlocked, you loose your coin."),
		     State
	end.

server_handle_push(From, State) ->
	case State of
		unlocked ->
			server_tell(From, "State changed to locked"),	       
			locked;
		_ -> server_tell(From, "Hey, you need to place a coin to pass this turnstile! Please look at your pocket!"),
		     State
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

