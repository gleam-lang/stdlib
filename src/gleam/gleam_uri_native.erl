-module (gleam_uri_native).
-export ([parse/1, to_string/1, parse_query/1, query_to_string/1]).

find_key(Key, Map) ->
  case maps:find(Key, Map) of
    {ok, Value}  ->
      {ok, Value};
    error ->
      {error, nil}
  end.

parse(String) ->
  case uri_string:parse(String) of
    {error, _Reason, _Term} ->
      {error, nil};
    Map ->
      {ok, {
        uri,
        find_key(scheme, Map),
        find_key(userinfo, Map),
        find_key(host, Map),
        find_key(port, Map),
        maps:get(path, Map),
        find_key(query, Map),
        find_key(fragment, Map)
      }}
  end.

to_string({uri, MaybeScheme, MaybeUserinfo, MaybeHost, MaybePort, Path, MaybeQuery, MaybeFragment}) ->
  Components = [{scheme, MaybeScheme}, {userinfo, MaybeUserinfo}, {host, MaybeHost}, {port, MaybePort}, {path, {ok, Path}}, {query, MaybeQuery}, {fragment, MaybeFragment}],
  Map = maps:from_list([{K, V} || {K, {ok, V}} <- Components]),
  case uri_string:recompose(Map) of
    String when is_binary(String) ->
      String;
    % Return value when empty
    [] -> <<"">>
  end.

parse_query(String) ->
  case uri_string:dissect_query(String) of
    {error, _Reason, _Term} ->
      {error, nil};
    Parts ->
      {ok, Parts}
    end.

query_to_string(Parts) ->
  case uri_string:compose_query(Parts, [{encoding, utf8}]) of
    String when is_binary(String) ->
      String;
    % Return value when empty
    [] -> <<"">>
  end.
