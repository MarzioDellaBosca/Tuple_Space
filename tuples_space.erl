-module(tuples_space).
-export([
    new/1, loop/1, in/2, in/3, rd/2, rd/3, out/2, addNode/2, removeNode/2, nodes/1, crash_test/1
]).

%%% Creazione di un nuovo Tuple Space
new(Name) ->
    Mio = self(),
    Pid = spawn(fun() -> watchdog(Name, [Mio]) end),
    {ok, Pid}.

%%% Watchdog che avvia e monitora il loop
watchdog(Name, Nodes) ->
    % Permette di catturare l'uscita del processo figlio
    process_flag(trap_exit, true),
    LoopPid = spawn_link(?MODULE, loop, [{[], Nodes, self()}]),
    register(Name, LoopPid),
    receive
        {'EXIT', LoopPid, Reason} ->
            io:format("Loop terminato con motivo: ~p. Riavvio...~n", [Reason]),
            watchdog(Name, Nodes);
        {crash_test, From} ->
            From ! {ok, restarted},
            watchdog(Name, Nodes)
    end.

% Funzioni interfaccia per separare la logica dell'applicazione dalle interazioni con gli utenti o i processi
in(Pid, Tuple) ->
    Pid ! {in, self(), Tuple},
    receive
        {ok, Tuple} ->
            {ok, Tuple};
        {err, unfollowed} ->
            {err, unfollowed}
    end.

in(Pid, Tuple, Timeout) ->
    Pid ! {in, self(), Tuple},
    receive
        {ok, Tuple} ->
            {ok, Tuple};
        {err, unfollowed} ->
            {err, Pid, unfollowed}
    after Timeout * 1000 ->
        {timeout, [Timeout]}
    end.

rd(Pid, Tuple) ->
    Pid ! {rd, self(), Tuple},
    receive
        {ok, Tuple} ->
            {ok, Tuple};
        {err, unfollowed} ->
            {err, self(), Pid, unfollowed}
    end.

rd(Pid, Tuple, Timeout) ->
    Pid ! {rd, self(), Tuple},
    receive
        {ok, Tuple} ->
            {ok, Tuple};
        {err, unfollowed} ->
            {err, self(), Pid, unfollowed}
    after Timeout * 1000 ->
        {timeout, [Timeout]}
    end.

out(Pid, Tuple) ->
    Pid ! {out, self(), Tuple},
    receive
        {ok, Tuple} -> {ok, Tuple};
        {err, unfollowed} -> {err, self(), Pid, unfollowed}
    end.

addNode(Pid, Node) ->
    Pid ! {addNode, Node, self()},
    receive
        {ok, Node} -> {ok, Node, Pid};
        {err, followed} -> {err, Pid, already_followed}
    end.

removeNode(Pid, Node) ->
    Pid ! {removeNode, Node, self()},
    receive
        {ok, Node} -> {ok, Node, Pid};
        {err, unfollowed} -> {err, Pid, unfollowed}
    end.

nodes(Pid) ->
    Pid ! {nodes, self()},
    receive
        {ok, Nodes} -> Nodes;
        {err, unfollowed} -> {err, Pid, unfollowed}
    end.

crash_test(Pid) ->
    Pid ! {crash_test, self()},
    receive
        {ok, restarted} -> {ok, restarted}
    after 3000 -> {error, timeout}
    end.

loop({Tuples, Nodes, WatchdogPid} = State) ->
    receive
        {in, From, Tuple} ->
            case matching_nodes(From, Nodes) of
                % se il nodo non è registrato a questo spazio tuple non può eseguire operazioni
                {err, From} ->
                    From ! {err, unfollowed},
                    loop(State);
                {ok, From} ->
                    case filter(Tuple, Tuples) of
                        % ritorna la tupla se è presente nello spazio, in questo caso è anche rimossa
                        {err, Tuple} ->
                            From ! {err, Tuple},
                            loop(State);
                        {ok, Tuple, Rest} ->
                            From ! {ok, Tuple},
                            loop({Rest, Nodes, WatchdogPid})
                    end
            end;
        {rd, From, Tuple} ->
            case matching_nodes(From, Nodes) of
                {err, From} ->
                    From ! {err, unfollowed},
                    loop(State);
                {ok, From} ->
                    case filter(Tuple, Tuples) of
                        {err, Tuple} ->
                            From ! {err, Tuple},
                            loop(State);
                        {ok, Tuple, _} ->
                            From ! {ok, Tuple},
                            loop(State)
                    end
            end;
        {out, From, Tuple} ->
            case matching_nodes(From, Nodes) of
                {err, From} ->
                    From ! {err, unfollowed},
                    loop(State);
                {ok, From} ->
                    From ! {ok, Tuple},
                    loop({[Tuple | Tuples], Nodes, WatchdogPid})
            end;
        {addNode, Node, From} ->
            case matching_nodes(Node, Nodes) of
                {err, Node} ->
                    From ! {ok, Node},
                    loop({Tuples, [Node | Nodes], WatchdogPid});
                {ok, Node} ->
                    From ! {err, followed},
                    loop(State)
            end;
        {removeNode, Node, From} ->
            case matching_nodes(Node, Nodes) of
                {err, Node} ->
                    From ! {err, unfollowed},
                    loop(State);
                {ok, Node} ->
                    From ! {ok, Node},
                    Rest = lists:delete(Node, Nodes),
                    loop({Tuples, Rest, WatchdogPid})
            end;
        {nodes, Node} ->
            case matching_nodes(Node, Nodes) of
                {err, Node} ->
                    Node ! {err, unfollowed},
                    loop(State);
                {ok, Node} ->
                    Node ! {ok, Nodes},
                    loop(State)
            end;
        {crash_test, From} ->
            WatchdogPid ! {crash_test, From},
            break
    end.

filter(Tupla, Tuples) ->
    case lists:member(Tupla, Tuples) of
        false ->
            {err, Tupla};
        true ->
            {ok, Tupla, lists:delete(Tupla, Tuples)}
    end.

matching_nodes(Pid, Nodes) ->
    case lists:member(Pid, Nodes) of
        false ->
            {err, Pid};
        true ->
            {ok, Pid}
    end.
