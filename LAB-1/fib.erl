%% Define the module
-module(fib).

%% Export the functions
-export([fib_p/1, fib_g/1, tail_fib/1]).

%% Fibonacci function using pattern matching
fib_p(0) -> 0;
fib_p(1) -> 1;
fib_p(N) when N > 1 ->
    fib_p(N - 1) + fib_p(N - 2).

%% Fibonacci function using guards
fib_g(N) when N =:= 0 -> 0;
fib_g(N) when N =:= 1 -> 1;
fib_g(N) when N > 1 ->
    fib_g(N - 1) + fib_g(N - 2).

%% Tail-recursive Fibonacci function
tail_fib(N) ->
    tail_fib(N, 0, 1).

tail_fib(0, A, _) ->
    A;
tail_fib(N, A, B) when N > 0 ->
    tail_fib(N - 1, B, A + B).
