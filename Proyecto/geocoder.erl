-module(geocoder).
-export([getLocation/2]).
-compile(export_all).

start()->
    ibrowse:start().
    
getLocation(Lat, Long)->
    %https://maps.googleapis.com/maps/api/geocode/json?latlng=40.714224,-73.961452&key=YOUR_API_KEY
    ApiKey = "AIzaSyArrde6GNKbi0fx-PCnimh0uybxhztnvmk",
    ApiUrl = "https://maps.googleapis.com/maps/api/geocode/json?latlng=",
    Url = ApiUrl++Lat++","++Long++"&key="++ApiKey,
    io:format("url: ~p~n",[Url]),  
    inets:start(),
    ssl:start(),
    case httpc:request(get, {Url, []}, [], []) of
    %case ibrowse:send_req("http://google.com", [], get, [],   [{socks5_host, "127.0.0.1"},   {socks5_port, 5335},   {socks5_user, "user4321"},   {socks5_password, "pass7654"}]) of
    {ok, _, _, ResJson}-> 
        %{_, Res} = mochijson2:decode(ResJson),
        %case lists:keyfind(<<"status">>, 1, Res) of
        %{<<"status">>, <<"OK">>} ->
        %    {<<"results">>, [{_, Res1}]} = lists:keyfind(<<"results">>, 1, Res),
        %    {<<"formatted_address">>, {_, Res2}} = lists:keyfind(<<"formatted_address">>, 1, Res1),
        %    Res2;
        %_ ->
        %    false
        %end;
        true;
    _ -> 
        false
    end.