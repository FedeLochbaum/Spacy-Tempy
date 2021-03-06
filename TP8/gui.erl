-module(gui).
-export([start/1, init/1]).

-include_lib("wx/include/wx.hrl").

start(Name) ->
  spawn(gui, init, [Name]).

timeNow() ->
  {H, M, S} = erlang:time(),
  H * 3600 + M * 60 + S.

init(Name) ->
  Width = 200,
  Height = 200,
  Server = wx:new(), %Server will be the parent for the Frame
  Frame = wxFrame:new(Server, -1, Name, [{size,{Width, Height}}]),
  wxFrame:show(Frame),
  loop(Frame).


loop(Frame)->
  receive
    waiting ->
      wxFrame:setBackgroundColour(Frame,{255, 255, 0}),
      wxFrame:refresh(Frame),
      loop(Frame);
    enter ->
      io:format("gui: enter SC ~w ~n", [timeNow()]),
      wxFrame:setBackgroundColour(Frame,?wxRED),
      wxFrame:refresh(Frame),
      loop(Frame);
    leave ->
      wxFrame:setBackgroundColour(Frame,?wxBLUE),
      wxFrame:refresh(Frame),
      loop(Frame);
    stop ->
      ok;
    Error ->
      io:format("gui: strange message ~w ~n", [Error]),
      loop(Frame)
  end.
