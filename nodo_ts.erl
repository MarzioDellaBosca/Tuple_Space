-module(nodo_ts).
-export([init/1, loop/0, in/3, in/4, rd/3, rd/4, out/3, nodes/2]).

init(Name) ->
    Pid = spawn(?MODULE, loop, []),
    register(Name, Pid),
    {ok, Pid}.

in(Node, TS, Tuple) ->
    Node ! {in, TS, Tuple, self(), Node},
    receive
        {ok, Tuple} -> {ok, Tuple};
        {err, unfollowed} -> {error, TS, unfollowed}
    end.

in(Node, TS, Tuple, Timeout) ->
    Node ! {in, TS, Tuple, self(), Node},
    receive
        {ok, Tuple} -> {ok, Tuple};
        {err, unfollowed} -> {error, TS, unfollowed}
    after Timeout * 1000 ->
        {timeout, [Timeout]}
    end.

rd(Node, TS, Tuple) ->
    Node ! {rd, TS, Tuple, self(), Node},
    receive
        {ok, Tuple} -> {ok, Tuple};
        {err, unfollowed} -> {error, TS, unfollowed}
    end.

rd(Node, TS, Tuple, Timeout) ->
    Node ! {rd, TS, Tuple, self(), Node},
    receive
        {ok, Tuple} -> {ok, Tuple};
        {err, unfollowed} -> {error, TS, unfollowed}
    after Timeout * 1000 ->
        {timeout, [Timeout]}
    end.

out(Node, TS, Tuple) ->
    Node ! {out, TS, Tuple, self(), Node},
    receive
        {ok, Tuple} -> {ok, Tuple};
        {err, unfollowed} -> {error, TS, unfollowed}
    end.

nodes(Node, TS) ->
    Node ! {nodes, TS, self(), Node},
    receive
        {ok, Nodes} -> Nodes;
        {err, unfollowed} -> {err, TS, unfollowed}
    end.

loop() ->
    receive
        {in, TS, Tuple, From, Node} ->
            TS ! {in, Node, Tuple},
            receive
                {ok, Tuple} ->
                    From !
                        {ok, Tuple};
                {err, unfollowed} ->
                    From !
                        {err, unfollowed}
            end,
            loop();
        {rd, TS, Tuple, From, Node} ->
            TS ! {rd, Node, Tuple},
            receive
                {ok, Tuple} ->
                    From !
                        {ok, Tuple};
                {err, unfollowed} ->
                    From !
                        {err, unfollowed}
            end,
            loop();
        {out, TS, Tuple, From, Node} ->
            %io:format("Node received out message: ~p~n", [{out, TS, Tuple, From}]),
            TS ! {out, Node, Tuple},
            receive
                {ok, Tuple} ->
                    From !
                        {ok, Tuple};
                {err, unfollowed} ->
                    From !
                        {err, unfollowed}
            end,
            loop();
        {nodes, TS, From, Node} ->
            TS ! {nodes, Node},
            receive
                {ok, Nodes} ->
                    From !
                        {ok, Nodes};
                {err, unfollowed} ->
                    From !
                        {err, unfollowed}
            end,
            loop()
    end.
