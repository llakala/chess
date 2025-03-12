-module(bitboard).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).
-export_type([bit_board/0]).

-type bit_board() :: {bit_board, integer(), integer(), bitstring()}.

-file("src/bitboard.gleam", 11).
-spec new(integer(), integer()) -> bit_board().
new(Width, Height) ->
    _pipe = gleam@list:repeat(<<0:(lists:max([(Width), 0]))>>, Height),
    _pipe@1 = gleam@list:fold(_pipe, <<>>, fun gleam@bit_array:append/2),
    {bit_board, Width, Height, _pipe@1}.

-file("src/bitboard.gleam", 18).
-spec each(bit_board(), fun((bitstring()) -> any())) -> nil.
each(Board, Func) ->
    Width = erlang:element(2, Board),
    Height = erlang:element(3, Board),
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
                _pipe@3 = gleam_stdlib:bit_array_slice(_pipe@2, 1, Height - 1),
                _pipe@4 = gleam@result:unwrap(_pipe@3, <<255>>),
                {bit_board, Width, Height - 1, _pipe@4}
            end,
            Func(First),
            each(Rest, Func)
    end.

-file("src/bitboard.gleam", 47).
-spec main() -> nil.
main() ->
    Output = new(8, 8),
    _pipe = Output,
    each(_pipe, fun gleam@io:debug/1).
