-module(scratchpad).
-compile(export_all).


parse_scheme(Url) ->
    httpd:parse_query(Url).
