-module(gui).
-export([start/1]).
-include_lib("wx/include/wx.hrl").

start(Name) ->
	spawn_link(fun() -> init(Name) end).


init(Name) ->
	Window = new_window(Name),
	loop(Window).


new_window(Name) ->
	Wx = wx:new(),
	Window = wxFrame:new(Wx, -1, "Colors"),
	Panel  = wxPanel:new(Window),
	wxFrame:setBackgroundColour(Panel, {0,0,0}),
	wxFrame:show(Window).
	%Window = wxFrame:new(Server, -1, Name, [{size,{200, 200}}]),
	%wxFrame:setBackgroundColour(Window, ?wxBLACK),
    %wxFrame:show(Window),
    %wxFrame:connect(Window, close_window),
    %Window.


color(Window, Tuple) ->
	wxFrame:setBackgroundColour(Window, Tuple),
	wxFrame:refresh(Window).


loop(Window) ->
	receive
		{color, T} ->
			color(Window, T),
			loop(Window);
		stop ->
			ok
	end.
