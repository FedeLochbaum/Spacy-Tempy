-module(geocoder).
-export([getLocation/2]).
-compile(export_all).

start()->
    ibrowse:start().
    
getLocation(Lat, Long)->
    %https://maps.googleapis.com/maps/api/geocode/json?latlng=40.714224,-73.961452&key=YOUR_API_KEY
    ApiKey = "AIzaSyAXu-cXHTre2EwEB5lfaeAoNu0j0FUA9VI",
    ApiUrl = "https://maps.googleapis.com/maps/api/geocode/json?latlng=",
    Url = ApiUrl++Lat++","++Long++"&key="++ApiKey,
    io:format("url: ~p~n",[Url]),  
    case ibrowse:send_req(Url, [], get) of
    {ok, _, _, ResJson}-> 
        {_, Res} = mochijson2:decode(ResJson),
        case lists:keyfind(<<"status">>, 1, Res) of
        {<<"status">>, <<"OK">>} ->
            {<<"results">>, [{_, Res1}]} = lists:keyfind(<<"results">>, 1, Res),
            {<<"formatted_address">>, {_, Res2}} = lists:keyfind(<<"formatted_address">>, 1, Res1),
            Res2;
        _ ->
            false
        end;
    _ ->
        false
    end.