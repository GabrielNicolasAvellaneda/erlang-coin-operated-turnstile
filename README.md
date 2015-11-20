# erlang-coin-operated-turnstile

A Simple FSM in Erlang for a coin operated turnstile.

## State transition table of the Coin Operated Turnstile

| Current State | Action/Input | Next State |
|---------------|--------------|------------|
| Locked        | coin         | Unlocked   |
| Locked        | push         | Locked     |
| Unlocked      | coin         | Unlocked   |
| Unlocked      | push         | Locked     |

