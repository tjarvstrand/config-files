
-module(user_default).

-export([p/1,
         c/1,
         c/2]).

p(T) ->
  io:format("~p~n", [T]).


%%------------------------------------------------------------------------------
%% @doc
%% equivalent to c:c(M, []).
%% @end
-spec c(module()) -> {ok, module()} | {error, term()}.
%%------------------------------------------------------------------------------
c(M) -> c(M, []).

%%------------------------------------------------------------------------------
%% @doc
%% Like c:c but looks in the entire code-path.
-spec c(module(), compile:options()) -> {ok, module()} | {error, term()}.
%%------------------------------------------------------------------------------
c(M, Opts) ->
  try
    case shell_default:c(M, Opts++[debug_info]) of
      error ->
        Source = proplists:get_value(source, M:module_info(compile)),
        OutDir = filename:dirname(code:which(M)),
        shell_default:c(Source, Opts ++ [{outdir,OutDir}]);
      O -> O
    end
  catch error: E -> E
  end.



%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
