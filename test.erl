-module(test).
-export([run_tests/1]).

%%% Funzione principale per eseguire i test
run_tests(Iterations) ->
    {ok, _} = tuples_space:new(test_ts),

    % Test per N esecuzioni
    io:format("\nTesting average execution times, (~p run)...~n", [Iterations]),
    AvgTimeOut = test_average_execution(test_ts, out, Iterations),
    AvgTimeRd = test_average_execution(test_ts, rd, Iterations),
    AvgTimeIn = test_average_execution(test_ts, in, Iterations),
    AvgTimeAddNode = test_average_execution(test_ts, addNode, Iterations),
    AvgTimeRemoveNode = test_average_execution(test_ts, removeNode, Iterations),

    AvgTimeOutNode = test_average_execution(test_ts, outNode, Iterations),
    AvgTimeRdNode = test_average_execution(test_ts, rdNode, Iterations),
    AvgTimeInNode = test_average_execution(test_ts, inNode, Iterations),

    io:format("\nAverage execution times from main module(microseconds):~n"),
    io:format("in: ~p, rd: ~p, out: ~p, addNode: ~p, removeNode: ~p~n", [
        AvgTimeIn, AvgTimeRd, AvgTimeOut, AvgTimeAddNode, AvgTimeRemoveNode
    ]),

    io:format("\nAverage execution times from node module(microseconds):~n"),
    io:format("in: ~p, rd: ~p, out: ~p~n", [AvgTimeInNode, AvgTimeRdNode, AvgTimeOutNode]),

    Ti = erlang:system_time(microsecond),
    {ok, restarted} = tuples_space:crash_test(test_ts),
    Tf = erlang:system_time(microsecond),
    Td = Tf - Ti,

    io:format("Recovery time: ~p microseconds~n", [Td]),

    ok.

%%% Test singolo per funzione, ritorna il tempo di esecuzione in microsecondi
test_single_execution(Pid, Func) ->
    Tuple = {test, tuple},
    case Func of
        out ->
            {Time, _} = timer:tc(tuples_space, out, [Pid, Tuple]),
            Time;
        in ->
            % Assicuriamo che la tupla esista
            tuples_space:out(Pid, Tuple),
            {Time, _} = timer:tc(tuples_space, in, [Pid, Tuple]),
            Time;
        rd ->
            tuples_space:out(Pid, Tuple),
            {Time, _} = timer:tc(tuples_space, rd, [Pid, Tuple]),
            Time;
        outNode ->
            UniqueName = list_to_atom("test_node_" ++ integer_to_list(erlang:monotonic_time())),
            {ok, NodePid} = nodo_ts:init(UniqueName),
            {Time, _} = timer:tc(nodo_ts, out, [NodePid, Pid, Tuple]),
            unregister(UniqueName),
            Time;
        inNode ->
            UniqueName = list_to_atom("test_node_" ++ integer_to_list(erlang:monotonic_time())),
            {ok, NodePid} = nodo_ts:init(UniqueName),
            {Time, _} = timer:tc(nodo_ts, in, [NodePid, Pid, Tuple]),
            unregister(UniqueName),
            Time;
        rdNode ->
            UniqueName = list_to_atom("test_node_" ++ integer_to_list(erlang:monotonic_time())),
            {ok, NodePid} = nodo_ts:init(UniqueName),
            {Time, _} = timer:tc(nodo_ts, rd, [NodePid, Pid, Tuple]),
            unregister(UniqueName),
            Time;
        addNode ->
            % Ogni nodo deve avere un nome univoco perchè altrimenti non può essere registrato
            UniqueName = list_to_atom("test_node_" ++ integer_to_list(erlang:monotonic_time())),
            {ok, NodePid} = nodo_ts:init(UniqueName),
            {Time, _} = timer:tc(tuples_space, addNode, [Pid, NodePid]),
            unregister(UniqueName),
            Time;
        removeNode ->
            UniqueName = list_to_atom("test_node_" ++ integer_to_list(erlang:monotonic_time())),
            {ok, NodePid} = nodo_ts:init(UniqueName),
            tuples_space:addNode(Pid, NodePid),
            {Time, _} = timer:tc(tuples_space, removeNode, [Pid, NodePid]),
            unregister(UniqueName),
            Time
    end.

%%% Test della media dei tempi di esecuzione su N run
test_average_execution(Pid, Func, Iterations) ->
    Tuple = {test, tuple},
    TotalTime =
        case Func of
            out ->
                lists:sum([test_single_execution(Pid, out) || _ <- lists:seq(1, Iterations)]);
            in ->
                lists:foreach(fun(_) -> tuples_space:out(Pid, Tuple) end, lists:seq(1, Iterations)),
                lists:sum([test_single_execution(Pid, in) || _ <- lists:seq(1, Iterations)]);
            rd ->
                lists:foreach(fun(_) -> tuples_space:out(Pid, Tuple) end, lists:seq(1, Iterations)),
                lists:sum([test_single_execution(Pid, rd) || _ <- lists:seq(1, Iterations)]);
            outNode ->
                lists:sum([test_single_execution(Pid, outNode) || _ <- lists:seq(1, Iterations)]);
            inNode ->
                lists:sum([test_single_execution(Pid, inNode) || _ <- lists:seq(1, Iterations)]);
            rdNode ->
                lists:sum([test_single_execution(Pid, rdNode) || _ <- lists:seq(1, Iterations)]);
            addNode ->
                lists:sum([test_single_execution(Pid, addNode) || _ <- lists:seq(1, Iterations)]);
            removeNode ->
                lists:sum([test_single_execution(Pid, removeNode) || _ <- lists:seq(1, Iterations)])
        end,
    TotalTime / (Iterations).
