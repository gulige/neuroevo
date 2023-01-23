-module(visor).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").
-include("flatland.hrl").

-define(WIDTH, 1440).
-define(HEIGHT, 900).

-define(INIT_SCAPE_TYPE, flatland).

-record(state, {
    window,
    canvas,
    update_rate,
    filter, % {Zoom, PanX, PanY}
    scape,
    scape_type
}).

zo() ->
    visor ! {self(), new_Filter, {0.2, 200, 200}}.

zo(Filter) ->
    visor ! {self(), new_Filter, Filter}.

legend() ->
    visor ! ctlegend_on.

start(UserId) ->
    spawn(visor, go, [UserId]).

stop() ->
    visor ! {self(), terminate}.

go(UserId) ->
    GS = gs:start(),
    Window = gs:create(window, GS, [{title, "Visor"}, {width, ?WIDTH}, {height, ?HEIGHT}]),
    Canvas = gs:create(canvas, Window, [{width, ?WIDTH}, {height, ?HEIGHT}]),
    register(visor, self()),
    put(canvas, Canvas),
    gs:config(Window, {map, true}),
    ScapeType = ?INIT_SCAPE_TYPE,
    PolisName = <<"polis_", (integer_to_binary(UserId))/binary>>,
    Polis_PId = gproc:where({n, l, PolisName}),
    Scape_PId = gen_server:call(Polis_PId, {get_scape, ScapeType}),
    %?DBG("Inside Visor:: Scape_PId:~p~n", [Scape_PId]),
    gen_server:cast(Scape_PId, {self(), subscribe, Canvas}),
    InitState = #state{
        window = Window,
        canvas = Canvas,
        update_rate = 10,
        filter = {1.0, 0, 0},
        scape = Scape_PId,
        scape_type = ScapeType
    },
    ?INFO("Visor started. Scape_PId:~p ScapeType:~p~n", [Scape_PId, ScapeType]),
    loop(InitState).

loop(S) ->
    Scape_PId = S#state.scape,
    receive
        {From, new_Filter, NewFilter} ->
            visor:loop(S#state{filter = NewFilter});
        {From, new_UpdateRate, NewUpdateRate} ->
            visor:loop(S#state{update_rate = NewUpdateRate});
        {From, change_scape, ScapeType} ->
            gen_server:cast(S#state.scape, {self(), unsubscribe}),
            New_ScapePId = gen_server:call(polis, {get_scape, ScapeType}),
            visor:loop(S#state{scape = New_ScapePId});
        {Scape_PId, draw_object, Object} ->
            %?DBG("~p~n", [Object]),
            Id = draw_object(S#state.canvas, Object),
            Scape_PId ! {self(), Id},
            visor:loop(S);
        {Scape_PId, destroy_object, Id} ->
            gs:destroy(Id),
            visor:loop(S);
        ctlegend_on -> % ct表示sensor
            Legend = ct_legend(),
            Canvas = S#state.canvas,
            ?INFO("Legend on:~p~n", [Legend]),
            put(legend, gs:create(text, Canvas, [{fg, red}, {coords, [{50, 50}]}, {text, Legend}])),
            visor:loop(S);
        ctlegend_off ->
            gs:destroy(get(legend)),
            erase(legend),
            ?INFO("Legend off.~n"),
            visor:loop(S);
        {From, terminate} ->
            done = gen_server:call(S#state.scape, {self(), unsubscribe}),
            ?INFO("Visor unsubscribed from:~p and terminated.~n", [S#state.scape])
    after S#state.update_rate ->
        %?DBG("Visor is refreshing.~n"),
        gen_server:cast(S#state.scape, {self(), redraw, S#state.filter}),
        visor:loop(S)
    end.

draw_avatars(Canvas, [Avatar|Avatars], Acc) ->
    U_Avatar = draw_avatar(Canvas, Avatar),
    draw_avatars(Canvas, Avatars, [U_Avatar|Acc]);
draw_avatars(Canvas, [], Acc) ->
    Acc.

draw_avatar(Canvas, Avatar) ->
    Objects = Avatar#avatar.objects,
    U_Objects = draw_objects(Canvas, Objects, []),
    Avatar#avatar{objects = U_Objects}.

draw_objects(Canvas, [Object|Objects], Acc) ->
    Id = draw_object(Canvas, Object),
    U_Object = Object#obj{id = Id},
    draw_objects(Canvas, Objects, [U_Object|Acc]);
draw_objects(_Canvas, [], Acc) ->
    Acc.

draw_object(Canvas, Object) ->
    #obj{type = ObjName, color = Color, coords = Coords, params = Parameter} = Object,
    Id = case ObjName of
        circle ->
            [{Cx, Cy}] = Coords,
            R = Parameter,
            Draw_Coords = [{Cx - R, Cy - R}, {Cx + R, Cy + R}],
            gs:create(oval, Canvas, [{coords, Draw_Coords}, {fill, Color}, {fg, Color}]);
        arrow ->
            gs:create(line, Canvas, [{coords, Coords}, {arrow, last}, {fg, Color}]);
        polygon ->
            gs:create(polygon, Canvas, [{coords, Coords}, {fill, Color}, {fg, Color}]);
        _ ->
            gs:create(ObjName, Canvas, [{coords, Coords}, {fg, Color}])
    end,
    Id.

draw_stats(Canvas, Avatar) when (Avatar#avatar.type =:= predator) orelse (Avatar#avatar.type =:= prey) ->
    CF = Avatar#avatar.actuators,
    CT = Avatar#avatar.sensors,
    Loc = Avatar#avatar.loc,
    CFStatIds = [gs:create(text, Canvas, [{coords, [Loc]}, {text, atom_to_list(Actuator)}]) || #actuator{name = Actuator} <- CF],
    CTStatIds = [gs:create(text, Canvas, [{coords, [Loc]}, {text, atom_to_list(Sensor)}]) || #sensor{name = Sensor} <- CT],
    [gs:destroy(Id) || Id <- CFStatIds],
    [gs:destroy(Id) || Id <- CTStatIds];
draw_stats(_, _) ->
    done.

redraw_avatars(Filter, [Avatar|Avatars]) ->
    Objects = Avatar#avatar.objects,
    redraw_objects(Filter, Objects),
    redraw_avatars(Filter, Avatars);
redraw_avatars(_Filter, []) ->
    done.

redraw_objects({Zoom, PanX, PanY}, [Object|Objects]) ->
    #obj{type = ObjName, id = Id, color = Color, coords = Coords, params = Parameter} = Object,
    Draw_Coords = case ObjName of
        circle ->
            [{Cx, Cy}] = Coords,
            R = Parameter,
            [{Cx - R, Cy - R}, {Cx + R, Cy + R}];
        _ ->
            Coords
    end,
    Filtered_Coords = [{X * Zoom + PanX, Y * Zoom + PanY} || {X, Y} <- Draw_Coords],
    if
        (ObjName =:= circle) ->
            gs:config(Id, [{coords, Filtered_Coords}, {fill, Color}, {fg, Color}]);
        true ->
            gs:config(Id, [{coords, Filtered_Coords}, {fg, Color}])
    end,
    redraw_objects({Zoom, PanX, PanY}, Objects);
redraw_objects(_Filter, []) ->
    done.

redraw_SensorVisualization(Filter, [Avatar|Avatars], AllAvatars) ->
    %color_scanner(Op, {Zoom, PanX, PanY}, Density, Spread, Loc, Direction, Avatars)
    flatland_color_scanner:color_scanner(draw, Filter, 45, math:pi() * 2, Avatar#avatar.loc, Avatar#avatar.direction, AllAvatars -- [Avatar]),
    redraw_SensorVisualization(Filter, Avatars, AllAvatars);
redraw_SensorVisualization(_Filter, [], _AllAvatars) ->
    done.

ct2color(Sensors) ->
    ?DBG("Sensors:~p~n", [Sensors]),
    SensorNames = [Name || #sensor{name = Name} <- Sensors],
    SensorTuple = count_SensorTypes(SensorNames, {0, 0, 0, 0}),
    case SensorTuple of
        {_, 0, 0, 0} -> red; % color sensor
        {0, _, 0, 0} -> yellow; % distance sensor
        {0, 0, _, 0} -> blue; % energy sensor
        {0, 0, 0, _} -> black; % other sensor
        _ -> white % mixed sensors
    end.

% [flatland_color_scanner, flatland_distance_scanner, flatland_energy_scanner]
count_SensorTypes([Sensor|Sensors], {ColorSensors, DistanceSensors, EnergySensors, OtherSensors}) ->
    case Sensor of
        flatland_color_scanner ->
            count_SensorTypes(Sensors, {ColorSensors + 1, DistanceSensors, EnergySensors, OtherSensors});
        flatland_distance_scanner ->
            count_SensorTypes(Sensors, {ColorSensors, DistanceSensors + 1, EnergySensors, OtherSensors});
        flatland_energy_scanner ->
            count_SensorTypes(Sensors, {ColorSensors, DistanceSensors, EnergySensors + 1, OtherSensors});
        _ ->
            count_SensorTypes(Sensors, {ColorSensors, DistanceSensors, EnergySensors, OtherSensors + 1})
    end;
count_SensorTypes([], Tuple) ->
    Tuple.

ct_legend() ->
    ["Red -> color sensor\n",
     "Yellow -> distance sensor\n",
     "Blue -> energy sensor\n",
     "Black -> other sensor\n",
     "White -> mixed sensors\n"].

t() ->
    spawn(visor, test, []).

test() ->
    GS = gs:start(),
    Window = gs:create(window, GS, [{title, "Visor"}, {width, ?WIDTH}, {height, ?HEIGHT}]),
    Canvas = gs:create(canvas, Window, [{width, ?WIDTH}, {height, ?HEIGHT}]),
    register(test, self()),
    put(canvas, Canvas),
    gs:config(Window, {map, true}),
    CT = #sensor{id = {{-1, flatland:gen_id()}, sensor}, vl = 4, name = flatland_color_scanner, parameters = [4]},
    Avatars = [flatland:create_avatar(predator, predator, flatland:gen_id(), {cf, CT, 1}, void) || _ <- lists:duplicate(20, 1)],
    U_Avatars = draw_avatars(Canvas, Avatars, []),
    test(U_Avatars, Window, Canvas, 100000, random).

test(_, _, _, 0, _) ->
    done;
test(Avatars, Window, Canvas, Steps, Mode) ->
    timer:sleep(4),
    U_Avatars = case Mode of
        manual ->
            receive
                {move, Speed} ->
                    [flatland:move(Avatar, Speed) || Avatar <- Avatars];
                {rotate, Angle} ->
                    [flatland:rotate(Avatar, Angle) || Avatar <- Avatars]
            after 4 ->
                Avatars
            end;
        random ->
            case rand:uniform(2) of
                1 ->
                    [flatland:move(Avatar, rand:uniform()) || Avatar <- Avatars];
                2 ->
                    [flatland:rotate(Avatar, (rand:uniform() - 0.5) / 2 * math:pi()) || Avatar <- Avatars]
            end
    end,
    Zoom = 1.0,
    PanX = Zoom * 0,
    PanY = Zoom * 0,
    Filter = {Zoom, PanX, PanY},
    redraw_avatars(Filter, U_Avatars),
    redraw_SensorVisualization(Filter, U_Avatars, U_Avatars),
    visor:test(U_Avatars, Window, Canvas, Steps - 1, Mode).

