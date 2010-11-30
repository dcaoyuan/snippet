%% @copyright 2007 Hynek Vychodil
%% @author Hynek Vychodil <vychodil.hynek@gmail.com>
%%   [http://pichis_blog.blogspot.com/]
%% @version 0.0.2
%% @end
%% =====================================================================
%% @doc Wide Finder project round 3
%%
%% Using less binary bindings than wfbm4_ets1.erl by Anders.

-module(wf_pichi3).

-export([main/1, start_bmets/1]).

-compile([native]).

main([File]) -> start(File), halt().

start_bmets(FileName) ->
    {ok, F} = nlt_reader:open(FileName),
    Reader = fun () -> nlt_reader:read(F) end,
    T = ets:new(wft, [public, set, {keypos, 1}]),
    Tbl = init(),
    Map = fun (B) -> find(B, Tbl, T) end,
    Reduce = fun (_, _) -> none end,
    file_map_reduce:map_reduce(Reader, {Map, Reduce}),
    lists:foreach(fun ({K, V}) ->
                          io:format("~p: ~s~n", [V, K])
                  end,
                  top_ten(T)).

top_ten(Tab) ->
    TopTen = fun (M, Acc) when length(Acc) < 10 ->
                     [M | Acc];
                 ({_, X} = E, [{_, C} | Es]) when X > C ->
                     lists:keysort(2, [E | Es]);
                 (_E, Acc) -> Acc
             end,
    lists:reverse(ets:foldl(TopTen, [], Tab)).

safe_add_key(Key, T) ->
    try ets:update_counter(T, Key, 1)
    catch
      error:_ -> case ets:insert_new(T, {Key, 1}) of
         true -> ok;
         false -> ets:update_counter(T, Key, 1)
      end
    end.

-define(STR, "] \"GET /ongoing/When/").

-define(REVSTR, "/nehW/gniogno/ TEG\" ]").

-define(STRLEN, 21).      %length(?STR)

-define(DATELEN, 16).     %length("200x/2000/00/00/")

-define(MATCHHEADLEN, 5). %length("200x/")

set_shifts(_, Count, Tbl)
    when Count =:= (?STRLEN) - 1 ->
    Tbl;
set_shifts([H | T], Count, Tbl) ->
    Shift = (?STRLEN) - Count - 1,
    set_shifts(T, Count + 1, dict:store(H, Shift, Tbl)).

set_defaults([], Tbl) -> Tbl;
set_defaults([V | T], Tbl) ->
    set_defaults(T, dict:store(V, ?STRLEN, Tbl)).

init() ->
    D = set_shifts(?STR, 0,
                   set_defaults(lists:seq(1, 255), dict:new())),
    list_to_tuple([S
                   || {_C, S} <- lists:sort(dict:to_list(D))]).

match_front(_, _, [], _, _) -> {true, 0};
match_front(Bin, N, [C1 | T], Comps, Tbl) ->
    <<_:N/binary, C2:8, _/binary>> = Bin,
    case C1 of
      C2 -> match_front(Bin, N - 1, T, Comps + 1, Tbl);
      _ ->
          case element(C2, Tbl) of
            ?STRLEN -> {false, ?STRLEN};
            Shift when Comps >= Shift -> {false, 1};
            Shift -> {false, Shift - Comps}
          end
    end.

scan_key(B, N, S) when N < S ->
    <<_:N/binary, C, _/binary>> = B,
    case C of
      $\s -> {ok, N};
      $. -> {none, N};
      $\n -> {none, N};
      _ -> scan_key(B, N + 1, S)
    end;
scan_key(_, N, _) -> {none, N}.

find(Bin, Tbl, Tab) ->
    find(Bin, Tbl, Tab, 0,
         size(Bin) - (?STRLEN) - (?DATELEN)).

find(Bin, Tbl, Tab, N, S) when N < S ->
    case match_front(Bin, N + (?STRLEN) - 1, ?REVSTR, 0,
                     Tbl)
        of
      {false, Shift} -> find(Bin, Tbl, Tab, N + Shift, S);
      {true, _} ->
          N1 = N + (?STRLEN),
          N2 = N1 + 3,
          case Bin of
            <<_:N2/binary, "x/", _:4/binary, $/, _:2/binary, $/,
              _:2/binary, $/, _/binary>> ->
                case scan_key(Bin, N1 + (?DATELEN),
                              S + (?STRLEN) + (?DATELEN))
                    of
                  {none, F} -> find(Bin, Tbl, Tab, F, S);
                  {ok, F} ->
                      N3 = N1 + (?MATCHHEADLEN),
                      L = F - N3,
                      <<_:N3/binary, Key:L/binary, _/binary>> = Bin,
                      safe_add_key(Key, Tab),
                      find(Bin, Tbl, Tab, F, S)
                end;
            _ -> find(Bin, Tbl, Tab, N1, S)
          end
    end;
find(_, _, _, _, _) -> done.
