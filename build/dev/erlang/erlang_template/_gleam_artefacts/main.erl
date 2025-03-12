-module(main).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).
-export_type([board/0]).

-type board() :: {board, integer(), integer(), bitstring()}.

-file("src/main.gleam", 12).
-spec new(integer(), integer()) -> board().
new(Width, Height) ->
    _pipe = gleam@list:repeat(<<0:(lists:max([(Width), 0]))>>, Height),
    _pipe@1 = gleam@list:fold(_pipe, <<>>, fun gleam@bit_array:append/2),
    {board, Width, Height, _pipe@1}.

-file("src/main.gleam", 23).
-spec each(board(), fun((bitstring()) -> any())) -> nil.
each(Board, Fun) ->
    case erlang:element(4, Board) of
        <<>> ->
            nil;

        _ ->
            First = begin
                _pipe = erlang:element(4, Board),
                _pipe@1 = gleam_stdlib:bit_array_slice(_pipe, 0, 1),
                gleam@result:unwrap(_pipe@1, <<255>>)
            end,
            Rest = begin
                _pipe@2 = erlang:element(4, Board),
                _pipe@3 = gleam_stdlib:bit_array_slice(
                    _pipe@2,
                    1,
                    erlang:element(3, Board) - 1
                ),
                _pipe@4 = gleam@result:unwrap(_pipe@3, <<255>>),
                {board,
                    erlang:element(2, Board),
                    erlang:element(3, Board) - 1,
                    _pipe@4}
            end,
            Fun(First),
            each(Rest, Fun)
    end.

-file("src/main.gleam", 50).
-spec main() -> list(integer()).
main() ->
    Output = new(8, 8),
    _pipe = Output,
    each(_pipe, fun gleam@io:debug/1),
    _pipe@1 = erlang:element(4, Output),
    _pipe@2 = binary:bin_to_list(_pipe@1),
    gleam@io:debug(_pipe@2).
