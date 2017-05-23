-module(gui).
-export([start/1]).
-include_lib("wx/include/wx.hrl").

start(Name) ->
	spawn_link(fun() -> init(Name) end).


init(Name) ->
	Window = new_window(Name),
	wxFrame:setBackgroundColour(Window, {0,0,0}),
	wxFrame:show(Window),
	loop(Window).


new_window(Name) ->
	Wx = wx:new(),
	Window = wxFrame:new(Wx, -1, io_lib:format("~w", [Name])).


color(Window, Tuple) ->
	wxFrame:setBackgroundColour(Window,Tuple),
	wxFrame:refresh(Window).


loop(Window) ->
	receive
		{color, T} ->
			color(Window, T),
			loop(Window);
		stop ->
			ok
	end.
