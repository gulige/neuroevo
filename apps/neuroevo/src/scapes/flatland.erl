-module(flatland).
-behaviour(gen_server).

-compile(export_all).

%% API
%-export([start_link/1, start_link/0, start/1, start/0, init/2]).

%% gen_server callbacks
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").
-include("flatland.hrl").

-define(LAND_WIDTH, 5000).
-define(LAND_HEIGHT, 5000).
-define(PLANT_AGE_LIMIT, 1000).
-define(PLANT_ENERGY_LIMIT, 1000).
-define(CREATURE_AGE_LIMIT, 20000).
-define(CREATURE_ENERGY_LIMIT, 10000).
-define(SPEAR_LENGTH, 2).
-define(COLLISIONS, on).

-define(PLANT_GROWTH, off).
-define(NEURAL_COST, 100).
-define(SPAWN_LOC, [{1, [0, 0]},
                    {2, [2500, 0]},
                    {3, [5000, 0]},
                    {4, [5000, 2500]},
                    {5, [5000, 5000]},
                    {6, [2500, 5000]},
                    {7, [0, 5000]},
                    {8, [0, 2500]}]).

-record(scape, {
    id, % atom()|float()|{float()::UniqueId,scape}|{atom()::ScapeName,scape}
    type,
    physics,
    metabolics,
    avatars = [],
    plants = [],
    walls = [],
    objects = [],
    scheduler = 0 %
}).


%%--------------------------------------------------------------------
%% Function: start_link() -> {ok, Pid} | ignore | {error, Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Start_Parameters) ->
    gen_server:start_link(?MODULE, Start_Parameters, []).

start(Start_Parameters) ->
    gen_server:start(?MODULE, Start_Parameters, []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start() ->
    gen_server:start(?MODULE, [], []).

init(Pid, InitState) ->
    gen_server:cast(Pid, {init, InitState}).

%%====================================================================
%% gen_server callbacks

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Parameters) ->
    rand:seed(exs64, util:now()),
    process_flag(trap_exit, true),
    ?DBG("Scape Parameters:~p~n", [Parameters]),
    spawn(flatland, heartbeat, [self()]),
    InitState = case Parameters of
        {Polis_PId, Scape_Type, Physics, Metabolics} ->
            Init_Avatars = world_init(Scape_Type, Physics, Metabolics),
            ?DBG("InitAvatars:~p~n", [Init_Avatars]),
            #scape{
                id = self(),
                type = Scape_Type,
                avatars = Init_Avatars,
                physics = Physics,
                metabolics = Metabolics
            }
    end,
    {ok, InitState}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
% CT: sensors, CF: actuators
handle_call({enter, Morphology, Specie_Id, CT, CF, TotNeurons, Exoself_PId}, {From_PId, _Ref}, State) ->
    {Reply, U_State} = case get(Exoself_PId) of
        entered ->
            ?DBG("Already Registered Citizen:~p~n", [Exoself_PId]),
            {undefined, State};
        destroyed ->
            {destroyed, State};
        undefined ->
            Stats = {CF, CT, TotNeurons},
            Avatars = State#scape.avatars,
            put(Exoself_PId, entered),
            Avatar = case get(visor) of
                undefined ->
                    create_avatar(Morphology, Specie_Id, Exoself_PId, Stats, void);
                {Visor_PId, Canvas} ->
                    NewAvatar = create_avatar(Morphology, Specie_Id, Exoself_PId, Stats, void),
                    visor:draw_avatar(Canvas, NewAvatar)
            end,
            ?DBG("Avatar:~p entered, ~p~n", [Exoself_PId, Avatar]),
            {done, State#scape{avatars = [Avatar|Avatars]}}
    end,
    {reply, Reply, U_State};

handle_call({leave, Exoself_PId}, {From_PId, _Ref}, State) ->
    U_State = destroy_avatar(Exoself_PId, State),
    ?DBG("Avatar left:~p~n", [Exoself_PId]),
    {reply, done, U_State};

handle_call({get_all, avatars, Exoself_PId}, {From_PId, _Ref}, State) ->
    Reply = case get(Exoself_PId) of
        destroyed ->
            destroyed;
        _ ->
            State#scape.avatars
    end,
    {reply, Reply, State};

% Command: two_wheels
handle_call({actuator, Exoself_PId, Command, Output}, {From_PId, _Ref}, State) ->
    %?DBG("########################: ~p~n", [util:now()]),
    {FitnessP, U_State} = case get(Exoself_PId) of
        undefined ->
            ?DBG("Unregistered Citizen:~p~n", [Exoself_PId]),
            {{0, 0}, State};
        destroyed ->
            erase(Exoself_PId),
            ?DBG("Avatar:~p destroyed.~n", [Exoself_PId]),
            {{0, 1}, State}; % HaltFlag = 1
        _ ->
            Avatars = State#scape.avatars,
            Avatar = lists:keyfind(Exoself_PId, 2, Avatars),
            U_Avatar = flatland:Command(Avatar#avatar{kills = 0}, Output),
            case (U_Avatar#avatar.energy > 0) andalso (U_Avatar#avatar.age < ?CREATURE_AGE_LIMIT) of
                true ->
                    Age = U_Avatar#avatar.age,
                    Fitness = case Age > 1000 of
                        true ->
                            0.001 + Avatar#avatar.kills * 1;
                        false ->
                            0.001 + Avatar#avatar.kills * 1
                    end,
                    U_Avatars = lists:keyreplace(Exoself_PId, 2, Avatars, U_Avatar),
                    U_Avatars2 = collision_detection(U_Avatar, U_Avatars),
                    {{Fitness, 0}, State#scape{avatars = U_Avatars2}};
                false ->
                    ?DBG("Avatar:~p died at age:~p~n", [U_Avatar#avatar.id, U_Avatar#avatar.age]),
                    {{0, 1}, destroy_avatar(Exoself_PId, State)} % HaltFlag = 1
            end
    end,
    {reply, FitnessP, U_State};

handle_call({multi_agent, update_agents, U_Avatars}, {Exoself_PId, _Ref}, State) ->
    {reply, ok, State#scape{avatars = U_Avatars}};

handle_call(tick, {From_PId, _Ref}, State) ->
    Avatars = State#scape.avatars,
    U_Avatars = flatland:metabolics(Avatars, []),
    {reply, done, State#scape{avatars = U_Avatars}};

handle_call(get_canvas, {From_PId, _Ref}, State) ->
    Reply = case get(visor) of
        {_Visor_PId, Canvas} ->
            Canvas;
        undefined ->
            undefined
    end,
    {reply, Reply, State};

handle_call({Visor_PId, unsubscribe}, {From_PId, _Ref}, State) ->
    erase(visor),
    {reply, done, State};

handle_call({stop, normal}, _From, State) ->
    {stop, normal, State};

handle_call({stop, shutdown}, _From, State) ->
    {stop, shutdown, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({init, InitState}, _State) ->
    {noreply, InitState};

handle_cast({Visor_PId, subscribe, Canvas}, State) ->
    put(visor, {Visor_PId, Canvas}),
    U_Avatars = visor:draw_avatars(Canvas, State#scape.avatars, []),
    U_Objects = visor:draw_objects(Canvas, State#scape.objects, []),
    ?DBG("Visor:~p subscribed with canvas:~p~n", [Visor_PId, Canvas]),
    {noreply, State#scape{avatars = U_Avatars, objects = U_Objects}};

handle_cast({Visor_PId, unsubscribe}, State) ->
    erase(visor),
    {noreply, State};

handle_cast({Visor_PId, redraw, Filter}, State) ->
    %?DBG("redraw: ~p~n", [util:now()]),
    case get(visor) of
        undefined ->
            ?DBG("Scape:~p can't redraw, Visor:~p is not subscribed.~n", [State#scape.type, Visor_PId]);
        {Visor_PId, _Canvas} ->
            visor:redraw_avatars(Filter, State#scape.avatars),
            visor:redraw_objects(Filter, State#scape.objects)
    end,
    {noreply, State};

handle_cast(tick, State) ->
%    Avatars = State#scape.avatars,
%    Scheduler = State#scape.scheduler,
%    ?DBG("Scape Type:~p Scheduler:~p~n", [State#scape.type, Scheduler]),
%    U_Avatars0 = flatland:metabolics(Avatars, []),
%    {U_Avatars, U_Scheduler} = case Scheduler of
%        10 ->
%            Plant = create_avatar(plant, plant, gen_id(), void, no_respawn),
%            case get(visor) of
%                undefined ->
%                    void;
%                {Visor_PId, Canvas} ->
%                    visor:draw_avatar(Canvas, Plant)
%            end,
%            ?DBG("Plant:~p~n", [Plant]),
%            {[Plant|U_Avatars0], 0};
%        _ ->
%            {U_Avatars0, Scheduler + 1}
%    end,
%   #scape{avatars = U_Avatars, scheduler = U_Scheduler}};
    Avatars = State#scape.avatars,
    U_Avatars = flatland:metabolics(Avatars, []),
    {noreply, State#scape{avatars = U_Avatars}};

handle_cast({stop, normal}, State) ->
    {stop, normal, State};

handle_cast({stop, shutdown}, State) ->
    {stop, shutdown, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ?DBG("Scape:~p~n Terminating with reason:~p~n", [self(), Reason]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

heartbeat(Scape_PId) ->
    heartbeat(Scape_PId, 100, 0).

heartbeat(Scape_PId, Tau, Time) ->
    receive
        {update_tau, NewTau} ->
            flatland:heartbeat(Scape_PId, Tau, Time)
    after Tau ->
        gen_server:cast(Scape_PId, tick),
        flatland:heartbeat(Scape_PId, Tau, Time + Tau)
    end.

% 新陈代谢
metabolics([Avatar|Avatars], Acc) ->
    %?DBG("Avatar:~p~n", [Avatar]),
    case Avatar#avatar.type of
        plant -> % 植物
            case Avatar#avatar.state of
                no_respawn ->
                    case Avatar#avatar.age < ?PLANT_AGE_LIMIT of
                        true ->
                            U_Avatar = ripen(Avatar), % 变成熟
                            metabolics(Avatars, [U_Avatar|Acc]);
                        false ->
                            case get(visor) of
                                undefined ->
                                    void;
                                {_Visor_PId, _Canvas} ->
                                    [gs:destroy(Id) || #obj{id = Id} <- Avatar#avatar.objects]
                            end,
                            metabolics(Avatars, Acc)
                    end;
                respawn ->
                    %RespawnedAvatar = respawn_avatar(Avatar),
                    metabolics(Avatars, [Avatar|Acc])
            end;
        predator -> % 捕食者
            Energy = Avatar#avatar.energy,
            %?DBG("Predator:energy=~p~n", [Energy]),
            U_Avatar = Avatar#avatar{energy = Energy - 0.01},
            metabolics(Avatars, [U_Avatar|Acc]);
        prey -> % 被捕食者
            Energy = Avatar#avatar.energy,
            %?DBG("Prey:energy=~p~n", [Energy]),
            U_Avatar = Avatar#avatar{energy = Energy - 0.01},
            metabolics(Avatars, [U_Avatar|Acc]);
        _ ->
            metabolics(Avatars, [Avatar|Acc])
    end;
metabolics([], Acc) ->
    %?DBG("Time:~p Avatar_Count:~p~n", [util:now(), length(Acc)]),
    Acc.

% 变成熟
ripen(Avatar) ->
    Energy = Avatar#avatar.energy,
    Age = Avatar#avatar.age,
    New_Color = case Energy of
        -2000 -> black;
        -500 -> cyan;
        0 -> grey;
        500 -> green;
        1300 -> yellow;
        1500 -> white;
        _ -> no_change
    end,
    ?DBG("Plant:~p~n", [Energy]),
    U_Energy = functions:saturation(Energy + 2, ?PLANT_ENERGY_LIMIT),
    case New_Color of
        no_change ->
            ?DBG("not ripe~n"),
            Avatar#avatar{energy = U_Energy, age = Age + 1};
        _ ->
            ?DBG("ripe~n"),
            Avatar#avatar{energy = U_Energy, age = Age + 1,
                          objects = [Obj#obj{color = New_Color} || (#obj{type = circle} = Obj) <- Avatar#avatar.objects]}
    end.

new_loc() ->
    X = rand:uniform(?LAND_WIDTH),
    Y = rand:uniform(?LAND_HEIGHT),
    {X, Y}.

new_loc(XMin, XMax, YMin, YMax) ->
    X = rand:uniform(XMax - XMin) + XMin,
    Y = rand:uniform(YMax - YMin) + YMin,
    {X, Y}.

% 如果不在指定范围内，将Avatar坐标调整到范围内
check_borders(Avatar, [{XMin, XMax}, {YMin, YMax}]) ->
    {X, Y} = Avatar#avatar.loc,
    R = Avatar#avatar.r,
    DX = if
        (X - R) < XMin -> XMin - (X - R);
        (X + R) > XMax -> XMax - (X + R);
        true -> 0
    end,
    DY = if
        (Y - R) < YMin -> YMin - (Y - R);
        (Y + R) > YMax -> YMax - (Y + R);
        true -> 0
    end,
    case {DX, DY} of
        {0, 0} ->
            Avatar;
        {DX, DY} ->
            U_Objects = [Obj#obj{pivot = {PX + DX, PY + DY}, coords = [{X_ + DX, Y_ + DY} || {X_, Y_} <- Coords]} ||
                         (#obj{pivot = {PX, PY}, coords = Coords} = Obj) <- Avatar#avatar.objects],
            Avatar#avatar{loc = {X + DX, Y + DY}, objects = U_Objects}
    end.

% 自己（OperatorAvatar）是否和目标（Avatar）发生碰撞
check_collision(OperatorAvatar, Avatar) ->
    {X, Y} = OperatorAvatar#avatar.loc,
    {Xav, Yav} = Avatar#avatar.loc,
    Distance = math:sqrt(math:pow(X - Xav, 2) + math:pow(Y - Yav, 2)),
    Distance < (OperatorAvatar#avatar.r + Avatar#avatar.r).

% 自己（OperatorAvatar）是否刺穿目标（Avatar）
check_penetration(OperatorAvatar, Avatar) ->
    {X, Y} = OperatorAvatar#avatar.loc,
    {DX, DY} = OperatorAvatar#avatar.direction,
    case (OperatorAvatar#avatar.type =:= predator) orelse (OperatorAvatar#avatar.spear =:= true) of
        true -> % 是捕食者，或者有矛
            Spear_UnitRay = {DX * math:cos(0) - DY * math:sin(0), DX * math:sin(0) + DY * math:cos(0)},
            {InterDist, _Color} = lib_sensor_flatland:shortest_intrLine({{X, Y}, Spear_UnitRay}, [Avatar], {inf, void}),
            (InterDist =/= -1) andalso (InterDist < (?SPEAR_LENGTH + OperatorAvatar#avatar.r));
        false ->
            false
    end.

collision_detection(OperatorAvatar, Avatars) ->
    collision_detection(OperatorAvatar, 0, 0, Avatars, []).

collision_detection(OperatorAvatar, EnergyAcc, Kills, [Avatar|Avatars], Acc) ->
    %?DBG("OperatorAvatar:~p Avatar:~p~n", [OperatorAvatar, Avatar]),
    if
        (Avatar#avatar.id =:= OperatorAvatar#avatar.id) ->
            collision_detection(OperatorAvatar, EnergyAcc, Kills, Avatars, [Avatar|Acc]);
        (Avatar#avatar.type =:= wall) ->
            U_OperatorAvatar = world_wall_collision(OperatorAvatar, Avatar),
            collision_detection(U_OperatorAvatar, EnergyAcc, Kills, Avatars, [Avatar|Acc]);
        true ->
            Collision = check_collision(OperatorAvatar, Avatar),
            Penetration = check_penetration(OperatorAvatar, Avatar),
            {Energy, Order, U_OperatorAvatar, U_Avatar} =
                case Collision orelse Penetration of
                    true ->
                        world_behavior(Collision, Penetration, OperatorAvatar, Avatar);
                    false ->
                        {0, void, OperatorAvatar, Avatar}
                end,
            case Order of
                destroy ->
                    case get(visor) of
                        undefined ->
                            void;
                        {_Visor_PId, _Canvas} ->
                            [gs:destroy(Id) || #obj{id = Id} <- U_Avatar#avatar.objects]
                    end,
                    put(U_Avatar#avatar.id, destroyed),
                    collision_detection(U_OperatorAvatar, EnergyAcc + Energy, Kills + 1, Avatars, Acc);
                plant_eaten ->
                    Kill_Score = case Energy > 0 of true -> 1; false -> 0 end,
                    case U_Avatar#avatar.state of
                        no_respawn ->
                            case get(visor) of
                                undefined ->
                                    void;
                                {_Visor_PId, _Canvas} ->
                                    [gs:destroy(Id) || #obj{id = Id} <- U_Avatar#avatar.objects]
                            end,
                            collision_detection(U_OperatorAvatar, EnergyAcc + Energy, Kills + Kill_Score, Avatars, Acc);
                        respawn ->
                            RespawnedAvatar = respawn_avatar([Avatar|Avatars] ++ Acc, U_Avatar),
                            collision_detection(U_OperatorAvatar, EnergyAcc + Energy, Kills + Kill_Score, Avatars, [RespawnedAvatar|Acc])
                    end;
                poison_eaten ->
                    case U_Avatar#avatar.state of
                        no_respawn ->
                            case get(visor) of
                                undefined ->
                                    void;
                                {_Visor_PId, _Canvas} ->
                                    [gs:destroy(Id) || #obj{id = Id} <- U_Avatar#avatar.objects]
                            end,
                            % 毒药的Energy应该是负的
                            collision_detection(U_OperatorAvatar, EnergyAcc + Energy, Kills, Avatars, Acc);
                        respawn ->
                            RespawnedAvatar = respawn_avatar([Avatar|Avatars] ++ Acc, U_Avatar),
                            collision_detection(U_OperatorAvatar, EnergyAcc + Energy, Kills, Avatars, [RespawnedAvatar|Acc])
                    end;
                void ->
                    collision_detection(U_OperatorAvatar, EnergyAcc + Energy, Kills, Avatars, [U_Avatar|Acc])
            end
    end;
collision_detection(OperatorAvatar, EnergyAcc, KillsAcc, [], Acc) ->
    case EnergyAcc =/= 0 of
        true ->
            Energy = OperatorAvatar#avatar.energy,
            Kills = OperatorAvatar#avatar.kills,
            U_OperatorAvatar = OperatorAvatar#avatar{energy = functions:saturation(Energy + EnergyAcc, ?CREATURE_ENERGY_LIMIT),
                                                     kills = Kills + KillsAcc},
            lists:keyreplace(U_OperatorAvatar#avatar.id, 2, Acc, U_OperatorAvatar);
        false ->
            lists:keyreplace(OperatorAvatar#avatar.id, 2, Acc, OperatorAvatar)
    end.

create_avatar(Morphology) ->
    create_avatar(Morphology, Morphology, gen_id(), {cf, ct, -1}, respawn, undefined).

create_avatar(Morphology, Specie_Id) ->
    create_avatar(Morphology, Specie_Id, gen_id(), {cf, ct, -1}, respawn, undefined).

create_avatar(Morphology, Specie_Id, Id, Stats, Parameters) ->
    create_avatar(Morphology, Specie_Id, Id, Stats, Parameters, undefined).

% 动物
create_avatar(Morphology, Specie_Id, Id, {CF, CT, TotNeurons}, void, InitEnergy)
  when (Morphology =:= flatland_predator) orelse (Morphology =:= flatland_prey) orelse (Morphology =:= automaton) ->
    case Morphology of
        flatland_predator ->
            ?DBG("Creating Predator:~p~n", [{CF, CT, Id}]),
            Color = red,
            %Color = visor:ct2color(CT),
            Loc = {X, Y} = {rand:uniform(400) + 300, rand:uniform(400) + 300},
            %Loc = {X, Y} = {rand:uniform(round(XMax / 2)) + XMax / 2 - R, rand:uniform(round(YMax / 2)) + YMax / 2 - R},
            Direction = {DX, DY} = {-1 / math:sqrt(2), -1 / math:sqrt(2)},
            Energy = case InitEnergy of
                undefined -> 1000;
                InitEnergy -> InitEnergy
            end,
            Metabolic_Package = static,
            case Metabolic_Package of
                static ->
                    Mass = 6,
                    R = 6;
                _ ->
                    Mass = 6 + Energy / 1000,
                    R = math:sqrt(Mass * 10)
            end,
            Objects = [#obj{type = circle, color = Color, pivot = {X, Y}, coords = [{X, Y}], params = R},
                       #obj{type = line, color = red, pivot = {X, Y}, coords = [{X, Y}, {X + DX * R * 2, Y + DY * R * 2}]}],
            #avatar{
                id = Id,
                type = predator,
                energy = Energy,
                loc = Loc,
                direction = Direction,
                r = R,
                mass = Mass,
                objects = Objects,
                actuators = CF,
                sensors = CT,
                stats = TotNeurons
            };
        flatland_prey ->
            ?DBG("Creating Prey:~p~n", [{CF, CT, Id}]),
            Loc = {X, Y} = {rand:uniform(800), rand:uniform(500)},
            Direction = {DX, DY} = {1 / math:sqrt(2), 1 / math:sqrt(2)},
            Energy = case InitEnergy of
                undefined -> 1000;
                InitEnergy -> InitEnergy
            end,
            Metabolic_Package = static,
            case Metabolic_Package of
                static ->
                    Mass = 10,
                    R = 10;
                _ ->
                    Mass = 10 + Energy / 1000,
                    R = math:sqrt(Mass * 10)
            end,
            case lists:keymember(spear, 2, CF) of
                true ->
                    Color = red,
                    Objects = [#obj{type = circle, color = Color, pivot = {X, Y}, coords = [{X, Y}], params = R},
                               #obj{type = line, color = Color, pivot = {X, Y}, coords = [{X, Y}, {X + DX * R * 2, Y + DY * R * 2}]}];
                false ->
                    Color = blue,
                    Objects = [#obj{type = circle, color = Color, pivot = {X, Y}, coords = [{X, Y}], params = R}]
            end,
            #avatar{
                id = Id,
                type = prey,
                energy = Energy,
                loc = Loc,
                direction = Direction,
                r = R,
                mass = Mass,
                objects = Objects,
                actuators = CF,
                sensors = CT,
                stats = TotNeurons
            };
        automaton ->
            Loc = {X, Y} = {400 + rand:uniform(1120), 200 + rand:uniform(580)},
            Angle = rand:uniform() * 2 * math:pi(),
            Direction = {DX, DY} = {(1 / math:sqrt(2)) * math:cos(Angle) - (1 / math:sqrt(2)) * math:sin(Angle),
                                    (1 / math:sqrt(2)) * math:sin(Angle) + (1 / math:sqrt(2)) * math:cos(Angle)}, % 逆时针旋转Angle角度
            Mass = 10,
            R = 10,
            Color = blue,
            Objects = [#obj{type = circle, color = Color, pivot = {X, Y}, coords = [{X, Y}], params = R}],
            #avatar{
                id = Id,
                type = Morphology,
                loc = Loc,
                direction = Direction,
                r = R,
                mass = Mass,
                objects = Objects
            }
    end;
% 植物
create_avatar(Morphology, Specie_Id, Id, {InitEnergy, InitLoc}, RespawnFlag, Metabolics)
  when (Morphology =:= plant) orelse (Morphology =:= poison) ->
    case Morphology of
        plant ->
            ?DBG("Creating Plant~n"),
            Direction = {1 / math:sqrt(2), 1 / math:sqrt(2)},
            Loc = {X, Y} = case InitLoc of
                undefined -> {rand:uniform(800), rand:uniform(500)};
                Val -> Val
            end,
            Energy = case InitEnergy of
                undefined -> 500;
                Val2 -> Val2
            end,
            case Metabolics of
                static ->
                    Mass = 3,
                    R = 3;
                _ ->
                    Mass = 3 + Energy / 1000,
                    R = math:sqrt(Mass * 3)
            end,
            %Objects = [#obj{type = line, color = green, pivot = {X, Y}, coords = [{-R + X, -R + Y}, {R + X, R + Y}]},
            %           #obj{type = line, color = green, pivot = {X, Y}, coords = [{-R + X, R + Y}, {R + X, -R + Y}]}],
            Objects = [#obj{type = circle, color = green, pivot = {X, Y}, coords = [{X, Y}], params = R}],
            #avatar{
                id = Id,
                type = Morphology,
                energy = Energy,
                food = 0,
                health = 0,
                loc = Loc,
                direction = Direction,
                r = R,
                mass = Mass,
                objects = Objects,
                state = RespawnFlag % [respawn | no_respawn]
            };
        poison ->
            ?DBG("Creating Poison~n"),
            Direction = {1 / math:sqrt(2), 1 / math:sqrt(2)},
            Loc = {X, Y} = case InitLoc of
                undefined -> {rand:uniform(800), rand:uniform(500)};
                Val -> Val
            end,
            Energy = case InitEnergy of
                undefined -> -2000;
                Val2 -> Val2
            end,
            case Metabolics of
                static ->
                    Mass = 3,
                    R = 3;
                _ ->
                    Mass = 3 + abs(Energy) / 1000,
                    R = math:sqrt(Mass * 3)
            end,
            Objects = [#obj{type = circle, color = black, pivot = {X, Y}, coords = [{X, Y}], params = R}],
            #avatar{
                id = Id,
                type = Morphology,
                energy = Energy,
                loc = Loc,
                direction = Direction,
                r = R,
                mass = Mass,
                objects = Objects,
                state = RespawnFlag
            }
    end;
% 其他物体：石头、墙
create_avatar(Morphology, Specie_Id, Id, undefined, Parameters, undefined)
  when (Morphology =:= rock) orelse (Morphology =:= wall) ->
    case Morphology of
        rock ->
            ?DBG("Creating Rock~n"),
            Direction = {1 / math:sqrt(2), 1 / math:sqrt(2)},
            {X, Y, R, Energy} = Parameters,
            Objects = [#obj{type = circle, color = brown, pivot = {X, Y}, coords = [{X, Y}], params = R}],
            #avatar{
                id = Id,
                type = Morphology,
                energy = Energy,
                loc = {X, Y},
                direction = Direction,
                r = R,
                objects = Objects
            };
        wall ->
            ?DBG("Creating Wall~n"),
            case Parameters of
                {x_wall, {Y, XMin, XMax}} ->
                    YMin = YMax = Y;
                {y_wall, {X, YMin, YMax}} ->
                    XMin = XMax = X
            end,
            Pivot = {(XMin + XMax) / 2, (YMin + YMax) / 2},
            Objects = [#obj{type = line, color = brown, pivot = Pivot, coords = [{XMin, YMin}, {XMax, YMax}]}],
            #avatar{
                id = Id,
                type = Morphology,
                energy = 10000,
                loc = void,
                direction = void,
                r = void,
                objects = Objects,
                state = Parameters
            }
    end.

destroy_avatar(ExoSelf_PId, #scape{avatars = Avatars} = State) ->
    case get(ExoSelf_PId) of
        undefined ->
            ?DBG("Destroy avatar associated with:~p, undefined~n", [ExoSelf_PId]);
        entered ->
            ?DBG("Destroying avatar associated with:~p~n", [ExoSelf_PId]),
            Avatar = lists:keyfind(ExoSelf_PId, 2, Avatars),
            erase(ExoSelf_PId),
            case get(visor) of
                undefined ->
                    void;
                {Visor_PId, _Canvas} ->
                    ?DBG("Avatar:~p~n", [Avatar]),
                    [gs:destroy(Id) || #obj{id = Id} <- Avatar#avatar.objects]
            end
    end,
    State#scape{avatars = lists:keydelete(ExoSelf_PId, 2, Avatars)}.

respawn_avatar(SpawnA) ->
    ?DBG("Respawning:~p~n", [SpawnA]),
    {X, Y} = {rand:uniform(800), rand:uniform(500)},
    case SpawnA#avatar.type of
        plant ->
            SpawnA#avatar{
                loc = {X, Y},
                energy = 500,
                objects = [Obj#obj{color = green, pivot = {X, Y}, coords = [{X, Y}]} || (#obj{type = circle} = Obj) <- SpawnA#avatar.objects]
            };
        poison ->
            SpawnA#avatar{
                loc = {X, Y},
                energy = -2000,
                objects = [Obj#obj{color = black, pivot = {X, Y}, coords = [{X, Y}]} || (#obj{type = circle} = Obj) <- SpawnA#avatar.objects]
            }
    end.

respawn_avatar(Avatars, SpawnA) ->
    BlockAvatars = [A_ || A_ <- Avatars, (A_#avatar.type =:= rock)],
    {X, Y} = rand_valid_xy(BlockAvatars), % 避开障碍物选择一个随机有效的位置
    case SpawnA#avatar.type of
        plant ->
            SpawnA#avatar{
                loc = {X, Y},
                energy = 500,
                objects = [Obj#obj{color = green, pivot = {X, Y}, coords = [{X, Y}]} || (#obj{type = circle} = Obj) <- SpawnA#avatar.objects]
            };
        poison ->
            SpawnA#avatar{
                loc = {X, Y},
                energy = -2000,
                objects = [Obj#obj{color = black, pivot = {X, Y}, coords = [{X, Y}]} || (#obj{type = circle} = Obj) <- SpawnA#avatar.objects]
            }
    end.

rand_valid_xy(BlockAvatars) ->
    case rand_valid_xy(BlockAvatars, {rand:uniform(800), rand:uniform(500)}) of
        undefined ->
            rand_valid_xy(BlockAvatars, {rand:uniform(800), rand:uniform(500)});
        Loc ->
            Loc
    end.

rand_valid_xy([A|BlockAvatars], {X, Y}) ->
    {Xav, Yav} = A#avatar.loc,
    Distance = math:sqrt(math:pow(X - Xav, 2) + math:pow(Y - Yav, 2)),
    Collision = (Distance < (A#avatar.r + 2)),
    case Collision of
        true -> undefined;
        false -> rand_valid_xy(BlockAvatars, {X, Y})
    end;
rand_valid_xy([], {X, Y}) ->
    {X, Y}.

% 根据速度更新avatar位移
move(Avatar, Speed0) ->
    {LX, LY} = Avatar#avatar.loc,
    {DX, DY} = Avatar#avatar.direction,
    Speed = case Avatar#avatar.type of
        prey -> Speed0;
        _ -> Speed0 * 0.9 % 肉食动物的速度打个9折
    end,
    Energy = Avatar#avatar.energy,
    % 根据移动的距离消耗能量
    U_Energy = Energy - 0.1 * (math:sqrt(math:pow(DX * Speed, 2) + math:pow(DY * Speed, 2))) - 0.1,
    %?DBG("Avatar:~p Energy burned:~p~n",[self(), U_Energy - Energy]),
    U_Loc = {LX + (DX * Speed), LY + (DY * Speed)},
    U_Objects = [Obj#obj{pivot = {PX + (DX * Speed), PY + (DY * Speed)},
                         coords = [{X + (DX * Speed), Y + (DY * Speed)} || {X, Y} <- Coords]} ||
                 (#obj{pivot = {PX, PY}, coords = Coords} = Obj) <- Avatar#avatar.objects],
    Avatar#avatar{energy = U_Energy, loc = U_Loc, objects = U_Objects}.

% 平移{DX,DY}
translate(Avatar, {DX, DY}) ->
    {LX, LY} = Avatar#avatar.loc,
    Energy = Avatar#avatar.energy,
    U_Energy = Energy - 0.1 * (math:sqrt(math:pow(DX, 2) + math:pow(DY, 2))) - 0.1,
    U_Loc = {LX + DX, LY + DY},
    U_Objects = [Obj#obj{pivot = {PX + DX, PY + DY},
                         coords = [{X + DX, Y + DY} || {X, Y} <- Coords]} ||
                 (#obj{pivot = {PX, PY}, coords = Coords} = Obj) <- Avatar#avatar.objects],
    Avatar#avatar{energy = U_Energy, loc = U_Loc, objects = U_Objects}.

% 逆时针旋转Angle角度
rotate(Avatar, A) ->
    Ratio = math:pi() / 4,
    Angle = A * Ratio,
    {DX, DY} = Avatar#avatar.direction,
    Energy = Avatar#avatar.energy,
    U_Energy = Energy - 0.1 * (abs(Angle)) - 0.1,
    U_Direction = {DX * math:cos(Angle) - DY * math:sin(Angle),
                   DX * math:sin(Angle) + DY * math:cos(Angle)},
    U_Objects = rotate(Avatar#avatar.objects, Angle, []),
    Avatar#avatar{energy = U_Energy, direction = U_Direction, objects = U_Objects}.

rotate([Object|Objects], Angle, Acc) ->
    #obj{pivot = {PX, PY}, coords = Coords} = Object,
    U_Coords = [{PX + (X - PX) * math:cos(Angle) - (Y - PY) * math:sin(Angle),
                 PY + (X - PX) * math:sin(Angle) + (Y - PY) * math:cos(Angle)} || {X, Y} <- Coords],
    U_Object = Object#obj{coords = U_Coords}, % 绕枢轴转，枢轴坐标不变
    rotate(Objects, Angle, [U_Object|Acc]);
rotate([], _Angle, Acc) ->
    Acc.

% 先按指定速度移动，再旋转Angle角度
move_and_rotate(Avatar, [Speed, Angle]) ->
    Moved_Avatar = move(Avatar, Speed),
    Rotated_Avatar = rotate(Moved_Avatar, Angle),
    Rotated_Avatar.

% 先旋转Angle角度，再按指定速度移动
rotate_and_move(Avatar, [Speed, Angle]) ->
    Rotated_Avatar = rotate(Avatar, Angle),
    Moved_Avatar = move(Rotated_Avatar, Speed),
    Moved_Avatar.

% 先旋转Angle角度，再平移指定位移
rotate_and_translate(Avatar, [Translation, Angle]) ->
    Rotated_Avatar = rotate(Avatar, Angle),
    Translated_Avatar = translate(Rotated_Avatar, Translation),
    Translated_Avatar.

% 两轮驱动，age加1
two_wheels(Avatar, [RightWheelSpeed, LeftWheelSpeed]) ->
    {Speed, Angle} = twowheels_to_rotate_and_move(RightWheelSpeed, LeftWheelSpeed),
    Rotated_Avatar = rotate(Avatar, Angle),
    Moved_Avatar = move(Rotated_Avatar, Speed),
    OldAge = Moved_Avatar#avatar.age,
    Moved_Avatar#avatar{age = OldAge + 1}.

twowheels_to_rotate_and_move(RightWheelSpeed, LeftWheelSpeed) ->
    Speed = (RightWheelSpeed + LeftWheelSpeed) / 2,
    Angle = RightWheelSpeed - LeftWheelSpeed,
    {Speed, Angle}.

% 差速驱动
differential_drive(Wr, Wl) ->
    R = 1,
    L = 1,
    differential_drive(R, L, Wr, Wl).

differential_drive(R, L, Wr, Wl) ->
    Uw = (Wr + Wl) / 2,
    Ua = (Wr - Wl),
    DTheta = (R / L) * Ua,
    Theta = DTheta * 1,
    DX = R * Uw * math:cos(Theta),
    DY = R * Uw * math:sin(Theta),
    {DX, DY}.

spear(Avatar, [Val]) ->
    case Val > 0 of
        true ->
            Energy = Avatar#avatar.energy,
            case Energy > 100 of
                true ->
                    Avatar#avatar{energy = Energy - 10, spear = true};
                false ->
                    Avatar#avatar{energy = Energy - 1, spear = false}
            end;
        false ->
            Avatar#avatar{spear = false}
    end.

%%--------------------------------------------------------------------
%% World functions
%%--------------------------------------------------------------------

world_init(World_Type, Physics, Metabolics) ->
    XMin = -5000,
    XMax = 5000,
    YMin = -5000,
    YMax = 5000,
    WorldPivot = {(XMin + XMax) / 2, (YMin + YMax) / 2},
    World_Border = [{XMin, XMax}, {YMin, YMax}],
    case World_Type of
        flatland ->
            Walls = create_walls(),
            Plants = [create_avatar(plant, plant, gen_id(), {undefined, rand_valid_xy([])}, respawn, Metabolics) || _ <- lists:duplicate(10, 1)],
            Poisons = [create_avatar(poison, poison, gen_id(), {undefined, rand_valid_xy([])}, respawn, Metabolics) || _ <- lists:duplicate(10, 1)],
            Plants % ++ Poisons ++ Walls
    end.

% OAvatar：OperatorAvatar，自己；Avatar：目标
% 返回：{Energy, Order, U_OperatorAvatar, U_Avatar}
world_behavior(Collision, Penetration, OAvatar, Avatar) ->
    OAType = OAvatar#avatar.type,
    AType = Avatar#avatar.type,
    if
        (OAType =:= prey) andalso (AType =:= plant) andalso (OAvatar#avatar.spear =/= undefined) ->
            % OAvatar is a predator
            %?DBG("Prey: ~p ate a plant: ~p gained energy:~p~n", [OAvatar#avatar.id, Avatar#avatar.id, Avatar#avatar.energy * 0.2]),
            {Avatar#avatar.energy * 0.2, plant_eaten, OAvatar, Avatar};
        (OAType =:= prey) andalso (AType =:= plant) ->
            %?DBG("Prey: ~p ate a plant: ~p~n", [OAvatar#avatar.id, Avatar#avatar.id]),
            {Avatar#avatar.energy, plant_eaten, OAvatar, Avatar};
        (OAType =:= prey) andalso (AType =:= poison) ->
            %?DBG("Prey: ~p ate a poison: ~p~n", [OAvatar#avatar.id, Avatar#avatar.id]),
            {Avatar#avatar.energy, poison_eaten, OAvatar, Avatar};
        (OAType =:= prey) andalso (AType =:= prey) andalso (Penetration =:= true) andalso (Avatar#avatar.spear =:= undefined) ->
            % OAvatar spears Avatar who is a prey
            {500, destroy, OAvatar, Avatar};
        (OAType =:= prey) andalso (AType =:= prey) andalso (Penetration =:= true) andalso (Avatar#avatar.spear =/= undefined) ->
            % OAvatar/predator spears another predator
            %?DBG("########Predator killed another predator~n"),
            {100, destroy, OAvatar, Avatar};
        (OAType =:= prey) andalso (AType =:= prey) andalso (Collision =:= true) ->
            U_Avatar = case ?COLLISIONS of
                on ->
                    PushStrength = 0.1, % push
                    push(OAvatar, Avatar, PushStrength);
                off ->
                    Avatar
            end,
            {0, void, OAvatar, U_Avatar};
        (OAType =:= predator) andalso (AType =:= prey) andalso (Penetration =:= true) ->
            %?DBG("Hunter: ~p ate a Prey: ~p~n", [OAvatar#avatar.id, Avatar#avatar.id]),
            {500, destroy, OAvatar, Avatar};
        (OAType =:= predator) andalso (AType =:= prey) andalso (Collision =:= true) ->
            U_Avatar = case ?COLLISIONS of
                on ->
                    PushStrength = 1, % push hard
                    push(OAvatar, Avatar, PushStrength);
                off ->
                    Avatar
            end,
            {0, void, OAvatar, U_Avatar};
        (OAType =:= predator) andalso (AType =:= predator) andalso (Penetration =:= true) ->
            U_Avatar = case ?COLLISIONS of
                on ->
                    PushStrength = 1, % push hard
                    % TODO, pushing must be done from the tip of the sword.
                    push(OAvatar, Avatar, PushStrength);
                off ->
                    Avatar
            end,
            {0, void, OAvatar, U_Avatar};
        (OAType =:= predator) andalso (AType =:= predator) andalso (Collision =:= true) ->
            U_Avatar = case ?COLLISIONS of
                on ->
                    PushStrength = 0.1, % push
                    push(OAvatar, Avatar, PushStrength);
                off ->
                    Avatar
            end,
            {0, void, OAvatar, U_Avatar};
        (OAType =:= predator) andalso ((AType =:= plant) orelse (AType =:= poison)) andalso (Collision =:= true) ->
            U_Avatar = case ?COLLISIONS of
                on ->
                    PushStrength = 0, % push aside
                    push(OAvatar, Avatar, PushStrength);
                off ->
                    Avatar
            end,
            {0, void, OAvatar, U_Avatar};
        (AType =:= rock) ->
            case ?COLLISIONS of
                on ->
                    case OAvatar#avatar.energy > Avatar#avatar.energy of
                        true ->
                            PushStrength = 1, % push hard
                            U_Avatar = push(OAvatar, Avatar, PushStrength),
                            {-1, void, OAvatar, U_Avatar};
                        false ->
                            PushStrength = 0, % push aside
                            U_OAvatar = push(Avatar, OAvatar, PushStrength),
                            {-1, void, U_OAvatar, Avatar}
                    end;
                off ->
                    {0, void, OAvatar, Avatar}
            end;
        (AType =:= fire_pit) -> % 火坑
            case ?COLLISIONS of
                on ->
                    case OAvatar#avatar.energy > Avatar#avatar.energy of
                        true ->
                            PushStrength = 1, % push hard
                            U_Avatar = push(OAvatar, Avatar, PushStrength),
                            {-100, void, OAvatar, U_Avatar};
                        false ->
                            PushStrength = 0, % push aside
                            U_OAvatar = push(Avatar, OAvatar, PushStrength),
                            {-100, void, U_OAvatar, Avatar}
                    end;
                off ->
                    {0, void, OAvatar, Avatar}
            end;
        (AType =:= beacon) -> % 信号灯
            case ?COLLISIONS of
                on ->
                    case OAvatar#avatar.energy > Avatar#avatar.energy of
                        true ->
                            PushStrength = 1, % push hard
                            U_Avatar = push(OAvatar, Avatar, PushStrength),
                            {0, void, OAvatar, U_Avatar};
                        false ->
                            PushStrength = 0, % push aside
                            U_OAvatar = push(Avatar, OAvatar, PushStrength),
                            {0, void, U_OAvatar, Avatar}
                    end;
                off ->
                    {0, void, OAvatar, Avatar}
            end;
        true ->
            {0, void, OAvatar, Avatar}
    end.

% 推挤：OAvatar以PushStrength推Avatar
push(OAvatar, Avatar, PushStrength) ->
    OAEnergy = OAvatar#avatar.energy,
    AEnergy = Avatar#avatar.energy,
    case OAEnergy > AEnergy of
        true ->
            {OX, OY} = OAvatar#avatar.loc,
            {X, Y} = Avatar#avatar.loc,
            DX = X - OX,
            DY = Y - OY,
            Distance = math:sqrt(math:pow(DX, 2) + math:pow(DY, 2)),
            %?DBG("OAvatar Type:~p Avatar Type:~p OX:~p OY:~p X:~p Y:~p DX:~p DY:~p Distance:~p~n",
            %     [OAvatar#avatar.type, Avatar#avatar.type, OX, OY, X, Y, DX, DY, Distance]),
            Min_Distance = OAvatar#avatar.r + Avatar#avatar.r,
            case Distance == 0 of
                true ->
                    MinPushX = -DX,
                    MinPushY = -DY;
                false ->
                    % 最小推开距离用于保证两个化身不相交
                    MinPushX = (Min_Distance / Distance) * DX - DX,
                    MinPushY = (Min_Distance / Distance) * DY - DY
            end,
            PushX = MinPushX + case DX == 0 of
                                   true -> 0;
                                   false -> (DX / abs(DX)) * PushStrength
                               end,
            PushY = MinPushY + case DY == 0 of
                                   true -> 0;
                                   false -> (DY / abs(DY)) * PushStrength
                               end,
            U_Loc = {X + PushX, Y + PushY},
            %?DBG("MinPushX:~p MinPushY:~p~n", [MinPushX, MinPushY]),
            %NewDistance = math:sqrt(math:pow(X + PushX - OX, 2) + math:pow(Y + PushY - OY, 2)),
            %?DBG("Pusher:~p Pushee:~p~nPush:~p~nNewLoc:~p~nDistance:~p NewDistance:~p~n",
            %     [{OX, OY}, {X, Y}, {PushX, PushY}, U_Loc, Distance, NewDistance]),
            U_Objects = [Obj#obj{pivot = {PX + PushX, PY + PushY},
                                 coords = [{CX + PushX, CY + PushY} || {CX, CY} <- Coords]} ||
                         (#obj{pivot = {PX, PY}, coords = Coords} = Obj) <- Avatar#avatar.objects],
            Avatar#avatar{loc = U_Loc, objects = U_Objects, energy = AEnergy - 10 * PushStrength};
        false ->
            Avatar
    end.

% OAvatar：OperatorAvatar，自己；Avatar：目标
world_wall_collision(OperatorAvatar, Avatar) ->
    {X, Y} = OperatorAvatar#avatar.loc,
    R = OperatorAvatar#avatar.r,
    {WallType, WallParam} = Avatar#avatar.state,
    case WallType of
        x_wall ->
            {WY, WXMin, WXMax} = WallParam,
            case (WY =< (Y + R)) andalso (WY >= (Y - R)) of
                true ->
                    case (WXMin =< X) andalso (WXMax >= X) of
                        true ->
                            case Y > WY of
                                true ->
                                    DY = R - (Y - WY),
                                    U_Loc = {X, Y + DY},
                                    U_Objects = update_objects_dxy(OperatorAvatar#avatar.objects, 0, DY),
                                    OperatorAvatar#avatar{loc = U_Loc, objects = U_Objects};
                                false ->
                                    DY = -R - (Y - WY),
                                    U_Loc = {X, Y + DY},
                                    U_Objects = update_objects_dxy(OperatorAvatar#avatar.objects, 0, DY),
                                    OperatorAvatar#avatar{loc = U_Loc, objects = U_Objects}
                            end;
                        false ->
                            case X < WXMin of
                                true ->
                                    Distance = math:sqrt(math:pow(X - WXMin, 2) + math:pow(Y - WY, 2)),
                                    case Distance < R of
                                        true -> % 和左墙角挤压，反弹
                                            resist({WXMin, WY}, OperatorAvatar);
                                        false ->
                                            OperatorAvatar
                                    end;
                                false ->
                                    Distance = math:sqrt(math:pow(X - WXMax, 2) + math:pow(Y - WY, 2)),
                                    case Distance < R of
                                        true -> % 和右墙角挤压，反弹
                                            resist({WXMax, WY}, OperatorAvatar);
                                        false ->
                                            OperatorAvatar
                                    end
                            end
                    end;
                false ->
                    OperatorAvatar
            end;
        y_wall ->
            {WX, WYMin, WYMax} = WallParam,
            case (WX =< (X + R)) andalso (WX >= (X - R)) of
                true ->
                    case (WYMin =< Y) andalso (WYMax >= Y) of
                        true ->
                            case X > WX of
                                true ->
                                    DX = R - (X - WX),
                                    U_Loc = {X + DX, Y},
                                    U_Objects = update_objects_dxy(OperatorAvatar#avatar.objects, DX, 0),
                                    OperatorAvatar#avatar{loc = U_Loc, objects = U_Objects};
                                false ->
                                    DX = -R - (X - WX),
                                    U_Loc = {X + DX, Y},
                                    U_Objects = update_objects_dxy(OperatorAvatar#avatar.objects, DX, 0),
                                    OperatorAvatar#avatar{loc = U_Loc, objects = U_Objects}
                            end;
                        false ->
                            case Y < WYMin of
                                true ->
                                    Distance = math:sqrt(math:pow(Y - WYMin, 2) + math:pow(X - WX, 2)),
                                    case Distance < R of
                                        true -> % 和上墙角挤压，反弹
                                            resist({WX, WYMin}, OperatorAvatar);
                                        false ->
                                            OperatorAvatar
                                    end;
                                false ->
                                    Distance = math:sqrt(math:pow(Y - WYMax, 2) + math:pow(X - WX, 2)),
                                    case Distance < R of
                                        true -> % 和下墙角挤压，反弹
                                            resist({WX, WYMax}, OperatorAvatar);
                                        false ->
                                            OperatorAvatar
                                    end
                            end
                    end;
                false ->
                    OperatorAvatar
            end
    end.

% {OX,OY}反弹Avatar
resist({OX, OY}, Avatar) ->
    PushStrength = 0, % push aside
    {X, Y} = Avatar#avatar.loc,
    DX = X - OX,
    DY = Y - OY,
    Distance = math:sqrt(math:pow(DX, 2) + math:pow(DY, 2)),
    Min_Distance = Avatar#avatar.r,
    MinPushX = (Min_Distance / Distance) * DX - DX,
    MinPushY = (Min_Distance / Distance) * DY - DY,
    PushX = MinPushX, % + (DX / abs(DX)) * PushStrength,
    PushY = MinPushY, % + (DY / abs(DY)) * PushStrength,
    U_Loc = {X + PushX, Y + PushY},
    %?DBG("MinPushX:~p MinPushY:~p~n", [MinPushX, MinPushY]),
    %NewDistance = math:sqrt(math:pow(X + PushX - OX, 2) + math:pow(Y + PushY - OY, 2)),
    %?DBG("Pusher:~p Pushee:~p~nPush:~p~nNewLoc:~p~nDistance:~p NewDistance:~p~n",
    %     [{OX, OY}, {X, Y}, {PushX, PushY}, U_Loc, Distance, NewDistance]),
    U_Objects = [Obj#obj{pivot = {PX + PushX, PY + PushY},
                         coords = [{CX + PushX, CY + PushY} || {CX, CY} <- Coords]} ||
                 (#obj{pivot = {PX, PY}, coords = Coords} = Obj) <- Avatar#avatar.objects],
    Avatar#avatar{loc = U_Loc, objects = U_Objects}.

% 更新对象位置
update_objects_dxy(Objects, DX, DY) ->
    [Obj#obj{pivot = {PX + DX, PY + DY}, coords = [{CX + DX, CY + DY} || {CX, CY} <- Coords]} ||
     (#obj{pivot = {PX, PY}, coords = Coords} = Obj) <- Objects].

gen_id() ->
    genotype:generate_UniqueId().

create_rocks() ->
    % {X, Y, R, Energy}
    Rocks = [
        {100, 100, 40, inf},
        {200, 400, 20, inf},
        {300, 500, 20, inf},
        {200, 300, 60, inf},
        {200, 450, 15, inf},
        {300, 100, 50, inf},
        {1000, 400, 20, inf}
        %{400, 500, 20, inf},
        %{450, 400, 20, inf},
        %{400, 200, 100, inf}
    ],
    [create_avatar(rock, rock, gen_id(), undefined, Rock, undefined) || Rock <- Rocks].

create_walls() ->
    % {x_wall, {Y, XMin, XMax}}
    % {y_wall, {X, YMin, YMax}}
    Sections = [
        {x_wall, {300, 100, 200}},
        {y_wall, {250, 100, 500}},
        {x_wall, {200, 400, 450}},
        {y_wall, {400, 200, 300}},
        {y_wall, {500, 400, 500}},
        {x_wall, {500, 450, 500}}
        %{x_wall, {300, 400, 450}},
        %{x_wall, {250, 450, 600}},
        %{y_wall, {700, 200, 400}},
        %{x_wall, {400, 450, 500}}
    ],
    [create_avatar(wall, wall, gen_id(), undefined, Section, undefined) || Section <- Sections].

