-module(erlang_template@chess).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([player_decoder/0, move/3]).
-export_type([player/0]).

-type player() :: white | black.

-file("src/erlang_template/chess.gleam", 8).
-spec player_decoder() -> gleam@dynamic@decode:decoder(player()).
player_decoder() ->
    gleam@dynamic@decode:then(
        {decoder, fun gleam@dynamic@decode:decode_string/1},
        fun(Player_string) -> case Player_string of
                <<"white"/utf8>> ->
                    gleam@dynamic@decode:success(white);

                <<"black"/utf8>> ->
                    gleam@dynamic@decode:success(black);

                _ ->
                    gleam@dynamic@decode:failure(
                        white,
                        <<"Invalid player"/utf8>>
                    )
            end end
    ).

-file("src/erlang_template/chess.gleam", 17).
-spec move(binary(), player(), list(binary())) -> {ok, binary()} |
    {error, binary()}.
move(Fen, Turn, Failed_moves) ->
    erlang:error(#{gleam_error => todo,
            message => <<"`todo` expression evaluated. This code has not yet been implemented."/utf8>>,
            module => <<"erlang_template/chess"/utf8>>,
            function => <<"move"/utf8>>,
            line => 22}).
