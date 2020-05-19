-module (gleam_uri_native).
-export ([parse/1]).

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
