-module(index).
-compile(export_all).

make() ->
    (catch ets:delete(index)),
    Ets = ets:new(index, [bag, named_table]),
    {ok, C} = re:compile("(?:\\,|\\.|\\;|\\:|\\s|[0-9]+)+"),
    {ok, Files} = file:list_dir("."),
    Orgs = [F || F <- Files, filename:extension(F) == ".org"],
    [add_index(I, C) || I <- Orgs],
    ets:tab2file(Ets, "index.ets"),
    ets:delete(index).

add_index(File, C) ->
    io:format("adding:~p~n",[File]),
    {ok, Bin} = file:read_file(File),
    L = re:split(Bin, C),
    [ets:insert(index, {to_lower(I),File}) || I <- L].

to_lower(B) ->
    list_to_binary(string:to_lower(binary_to_list(B))).

open() ->
    ets:file2tab("index.ets").

lookup(X) ->
    ets:lookup(index,X).

    


