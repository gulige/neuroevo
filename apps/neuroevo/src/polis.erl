-module(polis).
-behaviour(gen_server).

%% API
-export([sync/0, start/1, start/2, init/2, stop/1, create/0, reset/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([get_mnesia_tables/0]).
-mnesia_table(get_mnesia_tables).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

%%====================================================================
%% Polis Configuration Options
%%====================================================================

% The MODS list contains the names of the processes, functions, or other databases that also need to be executed and started when we start our
% neuroevolutionary platform.
-define(MODS, []).

% In the same manner, when we have created a new public scape, we can add a scape_summary tuple with this scape's information to the PUBLIC_SCAPES list,
% so that it is initialized and started with the system.
-define(PUBLIC_SCAPES, [ % Public Forum
    #scape_summary{type = flatland, metabolics = static}
]).

-record(scape_summary, {
    address, % scape进程Pid
    type, % scape模块名
    parameters = [],
    metabolics,
    physics
}).

% The state record for the polis has all the elements needed to track the currently active mods and public scapes, which were either present
% during the startup of the neuroevolutionary platform, or latter added while the polis was already online.
-record(state, {
    active_mods = [],
    active_scapes = []
}).

%%====================================================================
%% API
%%====================================================================

% A sync/0 function can compile and reload all the modules pertaining to the project within the folder.
sync() ->
    make:all([load]).

% The start/0 first checks whether a polis process has already been spawned, by checking if one is registered. If it's not, then the start/1 function
% starts up the neuroevolutionary platform.
start(UserId) ->
    PolisName = <<"polis_", (integer_to_binary(UserId))/binary>>,
    case gproc:where({n, l, PolisName}) of
        undefined ->
            gen_server:start(?MODULE, {UserId, {?MODS, ?PUBLIC_SCAPES}}, []);
        Polis_PId ->
            ?INFO("Polis:~p for user=~p is already running on this node.~n", [Polis_PId, UserId])
    end.

start(UserId, Start_Parameters) ->
    gen_server:start(?MODULE, {UserId, Start_Parameters}, []).

init(Pid, InitState) ->
    gen_server:cast(Pid, {init, InitState}).

% The stop/0 function first checks whether a polis process is online. If there is an online polis process running on the node, then the stop function
% sends a signal to it requesting it to stop.
stop(UserId) ->
    PolisName = <<"polis_", (integer_to_binary(UserId))/binary>>,
    case gproc:where({n, l, PolisName}) of
        undefined ->
            ?INFO("Polis for user=~p cannot be stopped, it is not online.~n", [UserId]);
        Polis_PId ->
            gproc:unreg_other({n, l, PolisName}, Polis_PId),
            gen_server:cast(Polis_PId, {stop, normal})
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

% The init/1 function first seeds random with a new seed, in the case a random number generator will be needed. The polis process is then registered,
% the mnesia database is started, and the supporting modules, if any, are then started through the start_supmods/1 function. Then all the specified
% public scapes, if any, are activated. Having called our neuroevolutionary platform polis, we give this polis a name “MATHEMA”, which is a greek word
% for knowledge, and learning. Finally we create the initial state, which contains the Pids of the currently active public scapes, and the names of
% the activated mods. The function then drops into the main gen_server loop.
init({UserId, {Mods, PublicScapes}}) ->
    rand:seed(exs64, util:now()),
    process_flag(trap_exit, true),
    PolisName = <<"polis_", (integer_to_binary(UserId))/binary>>,
    gproc:reg({n, l, PolisName}, self()),
    ?INFO("Parameters:~p~n", [{Mods, PublicScapes}]),
    start_supmods(Mods),
    Active_PublicScapes = start_scapes(UserId, PublicScapes, []),
    ?INFO("******** Polis: ##MATHEMA## is now online.~n"),
    InitState = #state{active_mods = Mods, active_scapes = Active_PublicScapes},
    {ok, InitState}.

% At this point polis only accepts a get_scape call, to which it replies with the Pid or undefined message, and the two standard {stop,normal} and
% {stop,shutdown} calls.
handle_call({get_scape, Type}, _From, State) ->
    Active_PublicScapes = State#state.active_scapes,
    Scape_PId = case lists:keyfind(Type, 3, Active_PublicScapes) of
        false ->
            undefined;
        PS ->
            PS#scape_summary.address
    end,
    {reply, Scape_PId, State};

handle_call({stop, normal}, _From, State) ->
    {stop, normal, State};

handle_call({stop, shutdown}, _From, State) ->
    {stop, shutdown, State}.

% At this point polis allows only for 3 standard casts: {init,InitState}, {stop,normal} and {stop,shutdown}.
handle_cast({init, InitState}, _State) ->
    {noreply, InitState};

handle_cast({stop, normal}, State) ->
    {stop, normal, State};

handle_cast({stop, shutdown}, State) ->
    {stop, shutdown, State}.

% The standard, still unused handle_info/2 function.
handle_info(_Info, State) ->
    {noreply, State}.

% When polis is terminated, it first shuts down all the scapes by calling stop_scapes/1, and then all the supporting mods, by calling stop_supmods/1.
terminate(Reason, S) ->
    Active_Mods = S#state.active_mods,
    stop_scapes(S#state.active_scapes),
    stop_supmods(Active_Mods),
    ?INFO("******** Polis: ##MATHEMA## is now offline, terminated with reason:~p.~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions 
%%====================================================================

% The reset/0 function deletes the schema, and recreates a fresh database from scratch.
reset() ->
    ?INFO("polis resetting..."),
    mnesia:stop(),
    ok = mnesia:delete_schema([node()]),
    polis:create().

% The create/0 function sets up new mnesia databases composed of the agent, cortex, neuron, sensor, actuator, substrate, specie, population,
% and experiment tables.
% 注意：不再使用该接口，而是使用mnesia_cluster的特性，即用模块属性mnesia_table来定义表
create() ->
    ?INFO("polis creating..."),
    mnesia:stop(),
    case mnesia:create_schema([node()]) of
        ok ->
            mnesia:start(),
            [mnesia:create_table(TableName, TableOptions) || {TableName, TableOptions} <- get_mnesia_tables()],
            ?INFO("ok"),
            ok;
        Err ->
            ?INFO("~p", [Err]),
            Err
    end.

get_mnesia_tables() ->
    [
     {population, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, population)}]},
     {specie, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, specie)}]},
     {agent, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, agent)}]},
     {cortex, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, cortex)}]},
     {neuron, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, neuron)}]},
     {sensor, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, sensor)}]},
     {actuator, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, actuator)}]},
     {substrate, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, substrate)}]},
     {experiment, [{disc_copies, [node()]}, {type, set}, {attributes, record_info(fields, experiment)}]}
    ].

% Start/Stop environmental modules: DBs, Environments, Network Access systems and tools...
%
%
% The start_supmods/1 function expects a list of module names of the mods that are to be started with the startup of the neuroevolutionary platform.
% Each module must have a start/0 function that starts up the supporting mod process.
start_supmods([ModName|ActiveMods]) ->
    ModName:start(),
    start_supmods(ActiveMods);
start_supmods([]) ->
    done.

% The stop_supmods/1 expects a list of supporting mod names, the mod's name must be the name of its module, and that module must have a stop/0 function
% that stops the module. stop_supmods/1 goes through the list of the mods, and stops each one.
stop_supmods([ModName|ActiveMods]) ->
    ModName:stop(),
    stop_supmods(ActiveMods);
stop_supmods([]) ->
    done.

% The start_scapes/3 function accepts a list of scape_summary records, which specify the names of the public scapes and any parameters that with which
% those scapes should be started. What specifies what scape that is going to be created by the scape module is the Type that is dropped into the function.
% Of course the scape module should already be able to create the Type of scape that is dropped into the start_link function. Once the scape is started,
% we record the Pid in that scape_summary's record. When all the public scapes have been started, the function outputs a list of updated scape_summary
% records.
start_scapes(UserId, [S|Scapes], Acc) ->
    Type = S#scape_summary.type,
    Parameters = S#scape_summary.parameters,
    Physics = S#scape_summary.physics,
    Metabolics = S#scape_summary.metabolics,
    {ok, PId} = Type:start_link([self(), Type, Physics, Metabolics, UserId]),
    start_scapes(UserId, Scapes, [S#scape_summary{address = PId}|Acc]);
start_scapes(_UserId, [], Acc) ->
    lists:reverse(Acc).

% The stop_scapes/1 function accepts a list of scape_summary records. The function extracts the Pid of the scape from the scape_summary, and requests for
% that scape to terminate itself.
stop_scapes([S|Scapes]) ->
    PId = S#scape_summary.address,
    gen_server:cast(PId, {self(), stop, normal}),
    stop_scapes(Scapes);
stop_scapes([]) ->
    ok.

