-module(binboard).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([to_string/1, get_pos/2, main/0]).
-export_type([bin_board/0, coord/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type bin_board() :: {bin_board, integer(), integer(), integer()}.

-type coord() :: {coord, integer(), integer()}.

-file("src/binboard.gleam", 20).
?DOC(
    " Break a string into N sections, separated on newlines.\n"
    " Returns an error if string wasn't divisible by number of times\n"
).
-spec break_string(binary(), integer()) -> {ok, binary()} | {error, binary()}.
break_string(Str, Times) ->
    Len = string:length(Str),
    Newliner = fun(Acc, Elem) ->
        [First | Rest] = case Acc of
            [_ | _] -> Acc;
            _assert_fail ->
                erlang:error(#{gleam_error => let_assert,
                            message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                            value => _assert_fail,
                            module => <<"binboard"/utf8>>,
                            function => <<"break_string"/utf8>>,
                            line => 26})
        end,
        case string:length(First) >= (case Times of
            0 -> 0;
            Gleam@denominator -> Len div Gleam@denominator
        end) of
            true ->
                [Elem, First | Rest];

            false ->
                [<<Elem/binary, First/binary>> | Rest]
        end
    end,
    case case Times of
        0 -> 0;
        Gleam@denominator@1 -> Len rem Gleam@denominator@1
    end of
        0 ->
            _pipe = Str,
            _pipe@1 = gleam@string:split(_pipe, <<""/utf8>>),
            _pipe@2 = lists:reverse(_pipe@1),
            _pipe@3 = gleam@list:fold(_pipe@2, [<<""/utf8>>], Newliner),
            _pipe@4 = gleam@string:join(_pipe@3, <<"\n"/utf8>>),
            {ok, _pipe@4};

        _ ->
            {error,
                <<"Number of splits doesn't go into the string's length"/utf8>>}
    end.

-file("src/binboard.gleam", 46).
-spec power(integer(), integer()) -> integer().
power(Num, Power) ->
    Power@1 = begin
        _pipe = Power,
        erlang:float(_pipe)
    end,
    _pipe@1 = gleam@int:power(Num, Power@1),
    _pipe@2 = gleam@result:unwrap(_pipe@1, +0.0),
    erlang:trunc(_pipe@2).

-file("src/binboard.gleam", 54).
-spec to_string(bin_board()) -> binary().
to_string(Board) ->
    _pipe = erlang:element(4, Board),
    _pipe@1 = gleam@int:to_base2(_pipe),
    _pipe@2 = gleam@string:pad_start(
        _pipe@1,
        erlang:element(2, Board) * erlang:element(3, Board),
        <<"0"/utf8>>
    ),
    _pipe@3 = break_string(_pipe@2, erlang:element(3, Board)),
    gleam@result:unwrap_both(_pipe@3).

-file("src/binboard.gleam", 64).
?DOC(
    " Returns whether the coordinate is filled\n"
    " Will return an error if the position is invalid\n"
).
-spec get_pos(bin_board(), coord()) -> {ok, boolean()} | {error, binary()}.
get_pos(Board, Pos) ->
    case (erlang:element(3, Pos) > erlang:element(3, Board)) orelse (erlang:element(
        2,
        Pos
    )
    > erlang:element(2, Board)) of
        true ->
            {error, <<"Invalid board position!"/utf8>>};

        false ->
            Index = (erlang:element(3, Pos) * erlang:element(2, Board)) + erlang:element(
                2,
                Pos
            ),
            Positions = erlang:element(3, Board) * erlang:element(2, Board),
            Complement = power(2, (Positions - 1) - Index),
            Anded = erlang:'band'(erlang:element(4, Board), Complement),
            _pipe = (Anded > 0),
            {ok, _pipe}
    end.

-file("src/binboard.gleam", 87).
-spec main() -> nil.
main() ->
    Value = 9,
    My_board = {bin_board, 2, 2, Value},
    gleam_stdlib:println(<<"Board:"/utf8>>),
    _pipe = My_board,
    _pipe@1 = to_string(_pipe),
    gleam_stdlib:println(_pipe@1),
    gleam_stdlib:println(<<""/utf8>>).
