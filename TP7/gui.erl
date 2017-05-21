-module(gui).
-export([start/1]).
-include_lib("wx/include/wx.hrl").

start(Name) ->
	spawn_link(fun() -> init(Name) end).


init(Name) ->
	Window = new_window(Name),
	loop(Window).


new_window(Name) ->
	Server = wx:new(),
	Window = wxFrame:new(Server, -1, Name, [{size,{200, 200}}]),
	wxFrame:setBackgroundColour(Window, ?wxBLACK),
    wxFrame:show(Window),
    wxFrame:connect(Window, close_window),
    Window.


color(Window, Color) ->
	wxFrame:setBackgroundColour(Window),
	wxFrame:refresh(Window).


loop(Window) ->
	receive
		{color, Color} ->
			color(Window, Color),
			loop(Window);
		stop ->
			ok
	end.