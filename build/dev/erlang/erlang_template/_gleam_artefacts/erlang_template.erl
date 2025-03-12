-module(erlang_template).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-file("src/erlang_template.gleam", 31).
-spec move_decoder() -> gleam@dynamic@decode:decoder({binary(),
    erlang_template@chess:player(),
    list(binary())}).
move_decoder() ->
    gleam@dynamic@decode:field(
        <<"fen"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_string/1},
        fun(Fen) ->
            gleam@dynamic@decode:field(
                <<"turn"/utf8>>,
                erlang_template@chess:player_decoder(),
                fun(Turn) ->
                    gleam@dynamic@decode:field(
                        <<"failed_moves"/utf8>>,
                        gleam@dynamic@decode:list(
                            {decoder, fun gleam@dynamic@decode:decode_string/1}
                        ),
                        fun(Failed_moves) ->
                            gleam@dynamic@decode:success(
                                {Fen, Turn, Failed_moves}
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/erlang_template.gleam", 38).
-spec handle_move(gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()).
handle_move(Request) ->
    wisp:require_string_body(
        Request,
        fun(Body) ->
            Decode_result = gleam@json:parse(Body, move_decoder()),
            case Decode_result of
                {error, _} ->
                    wisp:bad_request();

                {ok, Move} ->
                    Move_result = erlang_template@chess:move(
                        erlang:element(1, Move),
                        erlang:element(2, Move),
                        erlang:element(3, Move)
                    ),
                    case Move_result of
                        {ok, Move@1} ->
                            _pipe = wisp:ok(),
                            wisp:string_body(_pipe, Move@1);

                        {error, Reason} ->
                            _pipe@1 = wisp:internal_server_error(),
                            wisp:string_body(_pipe@1, Reason)
                    end
            end
        end
    ).

-file("src/erlang_template.gleam", 24).
-spec handle_request(gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()).
handle_request(Request) ->
    case fun gleam@http@request:path_segments/1(Request) of
        [<<"move"/utf8>>] ->
            handle_move(Request);

        _ ->
            wisp:ok()
    end.

-file("src/erlang_template.gleam", 9).
-spec main() -> nil.
main() ->
    wisp:configure_logger(),
    Secret_key_base = wisp:random_string(64),
    _assert_subject = begin
        _pipe = fun handle_request/1,
        _pipe@1 = wisp@wisp_mist:handler(_pipe, Secret_key_base),
        _pipe@2 = mist:new(_pipe@1),
        _pipe@3 = mist:bind(_pipe@2, <<"0.0.0.0"/utf8>>),
        _pipe@4 = mist:port(_pipe@3, 8000),
        mist:start_http(_pipe@4)
    end,
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"erlang_template"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 13})
    end,
    gleam_erlang_ffi:sleep_forever().
