-module(graphics).
-include_lib("wx/include/wx.hrl").
-include("header.hrl").

%% Exports
-export([
	init/0,
	createImages/0,
	create_image/3,
	printCars/3
	]).

%% Constants
-define(Timer, 60).

%% State record to store various elements of the graphics context
-record(state, {
    frame,
    panel,
    dc,
    paint,
    list,
    neighborhood,
    car,
    key,
    accident
}).
  
%% Initialize the graphics frame, panel and draw the initial images
init() ->
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "MAP", [{size,{1600, 900}}]),
    Panel = wxPanel:new(Frame),
    DC = wxPaintDC:new(Panel),
    Paint = wxBufferedPaintDC:new(Panel),
    {Neighborhood, Car} = createImages(),

    wxFrame:show(Frame),
    erlang:send_after(?Timer, self(), timer),

    wxPanel:connect(Panel, paint, [callback]),
    wxFrame:connect(Frame, close_window),

    {Frame, #state{frame = Frame, panel = Panel, dc = DC, paint = Paint,
        neighborhood = Neighborhood, car = Car}}.
		
%% Create initial images for neighborhood and car
createImages() ->
    Neighborhood = create_image("../photos/neighborhood.jpg", 1600, 900),
    Car = create_image("../photos/car.png", 35, 20),
    {Neighborhood, Car}.
	
%% Helper function to create an image with given path and dimensions
create_image(ImagePath, Width, Height) ->
    Image = wxImage:new(ImagePath),
    ScaledImage = wxImage:scale(Image, Width, Height),
    Bitmap = wxBitmap:new(ScaledImage),
    wxImage:destroy(Image),
    wxImage:destroy(ScaledImage),
    Bitmap.
	
%% Recursive function to print cars using their position and direction from an ETS table
printCars('$end_of_table', _, _ ) -> ok;
printCars(Key, Panel, Car) ->
    case ets:lookup(cars, Key) of
        [{_, [{A, B}, D, _, _], _, _, _, _, _}] ->
            DI = wxClientDC:new(Panel),
            Im = wxBitmap:convertToImage(Car),
            RotatedIm = case D of
                left -> Im;
                down -> wxImage:rotate(Im, -300, {A, B});
                right -> wxImage:rotate(Im, 600, {A, B});
                up -> wxImage:rotate(Im, 300, {A, B});
                _ -> undefined
            end,
            BitIm = wxBitmap:new(RotatedIm),
            wxDC:drawBitmap(DI, BitIm, {A, B}),
            printCars(ets:next(cars, Key), Panel, Car);
        _ ->
            ets:delete(cars, Key),
            ok
    end.
