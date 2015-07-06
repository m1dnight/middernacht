-module(all_tests).
-compile(export_all).

run() ->
    eunit:test({dir, "ebin/"}).
