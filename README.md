# erlang-coin-operated-turnstile

A Simple FSM in Erlang for a coin operated turnstile.

## State transition table of the Coin Operated Turnstile

| Current State | Action/Input | Next State | Output Description |
|---------------|--------------|------------|--------------------|
| Locked        | coin         | Unlocked   | Unlock turnstile so customer can push through |
| Locked        | push         | Locked     | None |
| Unlocked      | coin         | Unlocked   | None |
| Unlocked      | push         | Locked     | When customer has pushed through, lock turnstile |

