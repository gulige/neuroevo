%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(scape).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,prep,[ExoSelf_PId]).

prep(ExoSelf_PId) ->
	receive 
		{ExoSelf_PId,Name} ->
			scape:Name(ExoSelf_PId)
	end.

xor_sim(ExoSelf_PId)->
	XOR = [{[-1,-1],[-1]},{[1,-1],[1]},{[-1,1],[1]},{[1,1],[-1]}],
	xor_sim(ExoSelf_PId,{XOR,XOR},0).
	
xor_sim(ExoSelf_PId,{[{Input,CorrectOutput}|XOR],MXOR},ErrAcc) ->
	receive 
		{From,sense} ->
			From ! {self(),percept,Input},
			xor_sim(ExoSelf_PId,{[{Input,CorrectOutput}|XOR],MXOR},ErrAcc);
		{From,action,Output}->
			Error = sse(Output,CorrectOutput,0),
			case XOR of
				[] ->
					SSE = ErrAcc+Error,
					Fitness = 1/(SSE+0.000001),
					From ! {self(),Fitness,1},
					xor_sim(ExoSelf_PId,{MXOR,MXOR},0);
				_ ->
					From ! {self(),0,0},
					xor_sim(ExoSelf_PId,{XOR,MXOR},ErrAcc+Error)
			end;
		{ExoSelf_PId,terminate}->
			ok
	end.

		
	sse([T|Target],[O|Output],SSEAcc)->
		SSE = math:pow(T-O,2),
		sse(Target,Output,SSE+SSEAcc);
	sse([],[],SSEAcc)->
		SSEAcc.

-record(pb_state,{cpos=0,cvel=0,p1_angle=3.6*(2*math:pi()/360),p1_vel=0,p2_angle=0,p2_vel=0,time_step=0,goal_steps=90000,fitness_acc=0}).
pb_sim(ExoSelf_PId)->
	random:seed(util:now()),
	pb_sim(ExoSelf_PId,#pb_state{}).
	
pb_sim(ExoSelf_PId,S)->
	receive
		{From_PId,sense, [Parameter]}->%io:format("Sense request received:~p~n",[From_PId]),
			SenseSignal=case Parameter of
				cpos -> [S#pb_state.cpos];
				cvel -> [S#pb_state.cvel];
				p1_angle -> [S#pb_state.p1_angle];
				p1_vel -> [S#pb_state.p1_vel];
				p2_angle -> [S#pb_state.p2_angle];
				p2_vel -> [S#pb_state.p2_vel];
				2 -> [S#pb_state.cpos,S#pb_state.p1_angle];
				3 -> [S#pb_state.cpos,S#pb_state.p1_angle,S#pb_state.p2_angle];
				4 -> [S#pb_state.cpos,S#pb_state.cvel,S#pb_state.p1_angle,S#pb_state.p1_vel];
				6 -> [S#pb_state.cpos,S#pb_state.cvel,S#pb_state.p1_angle,S#pb_state.p1_vel,S#pb_state.p2_angle,S#pb_state.p2_vel]
			end,
			From_PId ! {self(),percept,SenseSignal},
			scape:pb_sim(ExoSelf_PId,S);
		{From_PId,push,[Damping_Flag,DPB_Flag],[F]}->
			AL = 2*math:pi()*(36/360),
			U_S=sm_DoublePole(F*10,S,2),
			TimeStep=U_S#pb_state.time_step,
			CPos=U_S#pb_state.cpos,
			CVel=U_S#pb_state.cvel,
			PAngle1=U_S#pb_state.p1_angle,
			PVel1=U_S#pb_state.p1_vel,
			case (abs(PAngle1) > AL) or (abs(U_S#pb_state.p2_angle)*DPB_Flag > AL) or (abs(CPos) > 2.4) or (TimeStep > U_S#pb_state.goal_steps)of
				true ->
					case (TimeStep > U_S#pb_state.goal_steps) of
						true ->%Fitness goal reached.
							From_PId ! {self(),goal_reached,1},
							scape:pb_sim(ExoSelf_PId,#pb_state{});
						false ->
							From_PId ! {self(),0,1},
							scape:pb_sim(ExoSelf_PId,#pb_state{})
					end;
				false ->
					Fitness = case Damping_Flag of
						without_damping ->
							1;
						with_damping ->
							Fitness1 = TimeStep/1000,
							Fitness2 = case TimeStep < 100 of
								true ->
									0;
								false ->
									0.75/(abs(CPos) +abs(CVel) + abs(PAngle1) + abs(PVel1))
							end,
							Fitness1*0.1 + Fitness2*0.9
					end,		
					From_PId ! {self(),Fitness,0},
					scape:pb_sim(ExoSelf_PId,U_S#pb_state{fitness_acc=U_S#pb_state.fitness_acc+Fitness})
			end;
		{ExoSelf_PId,terminate} ->
			ok
	end.
	
sm_DoublePole(_F,S,0)->
	S#pb_state{time_step=S#pb_state.time_step+1};
sm_DoublePole(F,S,SimStepIndex)->
	CPos=S#pb_state.cpos,
	CVel=S#pb_state.cvel,
	PAngle1=S#pb_state.p1_angle,
	PAngle2=S#pb_state.p2_angle,
	PVel1=S#pb_state.p1_vel,
	PVel2=S#pb_state.p2_vel,
	X = CPos, %EdgePositions = [-2.4,2.4],
	PHalfLength1 = 0.5, %Half-length of pole 1
	PHalfLength2 = 0.05, %Half-length of pole 2
	M = 1, %CartMass
	PMass1 = 0.1, %Pole1 mass
	PMass2 = 0.01, %Pole2 mass
	MUc = 0.0005, %Cart-Track Friction Coefficient
	MUp = 0.000002, %Pole-Hinge Friction Coefficient
	G = -9.81, %Gravity
	Delta = 0.01, %Timestep
	EM1 = PMass1*(1-(3/4)*math:pow(math:cos(PAngle1),2)),
	EM2 = PMass2*(1-(3/4)*math:pow(math:cos(PAngle2),2)),
	EF1 = PMass1*PHalfLength1*math:pow(PVel1,2)*math:sin(PAngle1)+(3/4)*PMass1*math:cos(PAngle1)*(((MUp*PVel1)/(PMass1*PHalfLength1))+G*math:sin(PAngle1)),
	EF2 = PMass2*PHalfLength2*math:pow(PVel2,2)*math:sin(PAngle2)+(3/4)*PMass2*math:cos(PAngle2)*(((MUp*PVel2)/(PMass1*PHalfLength2))+G*math:sin(PAngle2)),
	NextCAccel = (F - MUc*functions:sgn(CVel)+EF1+EF2)/(M+EM1+EM2),
	NextPAccel1 = -(3/(4*PHalfLength1))*((NextCAccel*math:cos(PAngle1))+(G*math:sin(PAngle1))+((MUp*PVel1)/(PMass1*PHalfLength1))),
	NextPAccel2 = -(3/(4*PHalfLength2))*((NextCAccel*math:cos(PAngle2))+(G*math:sin(PAngle2))+((MUp*PVel2)/(PMass2*PHalfLength2))),
	
	NextCVel = CVel+(Delta*NextCAccel),
	NextCPos = CPos+(Delta*CVel),
	NextPVel1 = PVel1+(Delta*NextPAccel1),
	NextPAngle1 = PAngle1+(Delta*NextPVel1),
	NextPVel2 = PVel2+(Delta*NextPAccel2),
	NextPAngle2 = PAngle2+(Delta*NextPVel2),
	U_S=S#pb_state{
		cpos=NextCPos,
		cvel=NextCVel,
		p1_angle=NextPAngle1,
		p1_vel=NextPVel1,
		p2_angle=NextPAngle2,
		p2_vel=NextPVel2
	},
	sm_DoublePole(0,U_S,SimStepIndex-1).
	
-record(dtm_sector,{id,description=[],r}).
-record(dtm_state,{agent_position=[0,0],agent_direction=90,sectors=[],tot_runs=60,run_index=1,switch_event,switched=false,step_index=0,fitness_acc=0}).
dtm_sim(ExoSelf_PId)->io:format("Starting dtm_sim~n"),
	random:seed(util:now()),
	%io:format("Starting pb_sim:~p~n",[self()]),
	dtm_sim(ExoSelf_PId,#dtm_state{switch_event=20+random:uniform(20), sectors=set_tmaze_sectors()}).

dtm_sim(ExoSelf_PId,S) when (S#dtm_state.run_index == S#dtm_state.switch_event) and (S#dtm_state.switched==false)->
	%io:format("Switch event:~p~n",[S#dtm_state.switch_event]),
	Sectors=S#dtm_state.sectors,
	SectorA=lists:keyfind([3,3],2,Sectors),
	SectorB=lists:keyfind([-3,3],2,Sectors),
	U_SectorA=SectorA#dtm_sector{r=SectorB#dtm_sector.r},
	U_SectorB=SectorB#dtm_sector{r=SectorA#dtm_sector.r},
	U_Sectors=lists:keyreplace([-3,3],2,lists:keyreplace([3,3],2,Sectors,U_SectorA),U_SectorB),
	scape:dtm_sim(ExoSelf_PId,S#dtm_state{sectors=U_Sectors, switched=true});
dtm_sim(ExoSelf_PId,S)->
	receive
		{From_PId,sense,Parameters}->
			%io:format("Sense:~p~n",[Parameters]),
			APos = S#dtm_state.agent_position,
			ADir = S#dtm_state.agent_direction,
			Sector=lists:keyfind(APos,2,S#dtm_state.sectors),
			{ADir,NextSec,RangeSense,RewardSense} = lists:keyfind(ADir,1,Sector#dtm_sector.description),
			SenseSignal=case Parameters of
				[all] ->
					RangeSense++RewardSense++[Sector#dtm_sector.r];
				[range_sense]->
					RangeSense;
				[reward_sense]->
					RewardSense;
				[reward] ->
					[Sector#dtm_sector.r]
			end,
			%io:format("Position:~p SenseSignal:~p ",[APos,SenseSignal]),
			From_PId ! {self(),percept,SenseSignal},
			scape:dtm_sim(ExoSelf_PId,S);
		{From_PId,move,_Parameters,[Move]}->
			%timer:sleep(1000),
			APos = S#dtm_state.agent_position,
			ADir = S#dtm_state.agent_direction,
			Sector=lists:keyfind(APos,2,S#dtm_state.sectors),
			U_StepIndex = S#dtm_state.step_index+1,
			%io:format("Move:~p StepIndex:~p RunIndex:~p~n",[Move,U_StepIndex,S#dtm_state.run_index]),
			{ADir,NextSec,RangeSense,RewardSense} = lists:keyfind(ADir,1,Sector#dtm_sector.description),
			RewardSector1 = lists:keyfind([3,3],2,S#dtm_state.sectors),
			RewardSector2 = lists:keyfind([-3,3],2,S#dtm_state.sectors),
			LargeRewardDist = case RewardSector1#dtm_sector.r > RewardSector2#dtm_sector.r of
				true ->
					distance([3,3],APos);
				false ->
					distance([-3,3],APos)
			end,
			NavigationReward = 2/(LargeRewardDist+1),
			if
				(U_StepIndex > 50) ->
					Updated_RunIndex=S#dtm_state.run_index+1,
					case Updated_RunIndex > S#dtm_state.tot_runs of
						true ->
							From_PId ! {self(),Sector#dtm_sector.r+NavigationReward,1},
							U_S = #dtm_state{
								switch_event=20+random:uniform(20),
								sectors=set_tmaze_sectors(),
								switched=false
							},
							dtm_sim(ExoSelf_PId,U_S);
						false ->
							From_PId ! {self(),Sector#dtm_sector.r+NavigationReward,0},
							U_S = S#dtm_state{
								agent_position=[0,0],
								agent_direction=90,
								run_index=Updated_RunIndex,
								step_index = 0
							},
							dtm_sim(ExoSelf_PId,U_S)
					end;
				(APos == [3,3]) or (APos == [-3,3]) ->
					Updated_RunIndex=S#dtm_state.run_index+1,
					case Updated_RunIndex > S#dtm_state.tot_runs of
						true ->
							From_PId ! {self(),Sector#dtm_sector.r+NavigationReward,1},
							U_S = #dtm_state{
								switch_event=20+random:uniform(20),
								sectors=set_tmaze_sectors()
							},
							dtm_sim(ExoSelf_PId,U_S);
						false ->
							From_PId ! {self(),Sector#dtm_sector.r+NavigationReward,0},
							U_S = S#dtm_state{
								agent_position=[0,0],
								agent_direction=90,
								run_index=Updated_RunIndex,
								step_index = 0
							},
							dtm_sim(ExoSelf_PId,U_S)
					end;
				Move > 0.33 -> %clockwise
					NewDir=(S#dtm_state.agent_direction + 270) rem 360,
					From_PId ! {self(),0,0},
					U_S = S#dtm_state{
						agent_direction=NewDir,
						step_index = U_StepIndex
					},
					dtm_sim(ExoSelf_PId,U_S);
				Move < -0.33 -> %counterclockwise
					NewDir=(S#dtm_state.agent_direction + 90) rem 360,
					From_PId ! {self(),0,0},
					U_S = S#dtm_state{
						agent_direction=NewDir,
						step_index = U_StepIndex
					},
					dtm_sim(ExoSelf_PId,U_S);
				true -> %forward
					case NextSec of
						[] -> %wall crash/restart_state
							Updated_RunIndex = S#dtm_state.run_index+1,
							Updated_RunIndex=S#dtm_state.run_index+1,
							case Updated_RunIndex > S#dtm_state.tot_runs of
								true ->
									From_PId ! {self(),0+NavigationReward,1},
									U_S = #dtm_state{
										switch_event=20+random:uniform(20),
										sectors=set_tmaze_sectors(),
										switched=false
									},
									dtm_sim(ExoSelf_PId,U_S);
								false ->
									From_PId ! {self(),0+NavigationReward,0},
									U_S = S#dtm_state{
										agent_position=[0,0],
										agent_direction=90,
										run_index=Updated_RunIndex,
										step_index = 0
									},
									dtm_sim(ExoSelf_PId,U_S)
							end;
						_ -> %move
							From_PId ! {self(),0,0},
							U_S = S#dtm_state{
								agent_position=NextSec,
								step_index = U_StepIndex
							},
							dtm_sim(ExoSelf_PId,U_S)
					end
			end;
		{ExoSelf_PId,terminate} ->
			ok
	end.

set_tmaze_sectors()->
	Sectors = [
	#dtm_sector{id=[0,0],description=[{0,[],   [3,0,0],[-1,-1,-1]},{90,[0,1],[0,3,0],[-1,-1,-1]},{180,[],    [0,0,3],[-1,-1,-1]},{270,[],   [0,0,0],[-1,-1,-1]}],r=0},
	#dtm_sector{id=[0,1],description=[{0,[],   [2,0,0],[-1,-1,-1]},{90,[0,2],[0,2,0],[-1,-1,-1]},{180,[],    [0,0,2],[-1,-1,-1]},{270,[],   [0,1,0],[-1,-1,-1]}],r=0},
	#dtm_sector{id=[0,2],description=[{0,[],   [1,0,0],[-1,-1,-1]},{90,[0,3],[0,1,0],[-1,-1,-1]},{180,[],    [0,0,1],[-1,-1,-1]},{270,[],   [0,2,0],[-1,-1,-1]}],r=0},
	#dtm_sector{id=[0,3],description=[{0,[1,3],[0,3,3],[-1, 3,-1]},{90,[],   [3,0,3],[ 3,-1, 3]},{180,[-1,3],[3,3,0],[-1, 3,-1]},{270,[0,2],[3,3,3],[ 3,-1, 3]}],r=0},
	
	#dtm_sector{id=[1,3],description=[{0,[2,3],[0,2,0],[-1, 2,-1]},{90,[],[4,0,2],[4,-1, 2]},{180,[0,3],[0,4,0],[-1,4,-1]},{270,[],[2,0,4],[ 2,-1,4]}],r=0},
	#dtm_sector{id=[2,3],description=[{0,[3,3],[0,1,0],[-1, 1,-1]},{90,[],[5,0,1],[5,-1, 1]},{180,[1,3],[0,5,0],[-1,5,-1]},{270,[],[1,0,5],[ 1,-1,5]}],r=0},
	#dtm_sector{id=[3,3],description=[{0,[],   [0,0,0],[-1, 0,-1]},{90,[],[6,0,0],[6,-1, 0]},{180,[2,3],[0,6,0],[-1,6,-1]},{270,[],[0,0,6],[ 0,-1,6]}],r=2},
	
	#dtm_sector{id=[-1,3],description=[{0,[ 0,3],[0,4,0],[-1,4,-1]},{90,[],[2,0,4],[ 2,-1,4]},{180,[-2,3],[0,2,0],[-1, 2,-1]},{270,[],[4,0,2],[4,-1, 2]}],r=0},
	#dtm_sector{id=[-2,3],description=[{0,[-1,3],[0,5,0],[-1,5,-1]},{90,[],[1,0,5],[ 1,-1,5]},{180,[-3,3],[0,1,0],[-1, 1,-1]},{270,[],[5,0,1],[5,-1, 1]}],r=0},
	#dtm_sector{id=[-3,3],description=[{0,[-2,3],[0,6,0],[-1,6,-1]},{90,[],[0,0,6],[ 0,-1,6]},{180,[],    [0,0,0],[-1, 0,-1]},{270,[],[6,0,0],[6,-1, 0]}],r=1}
	].

distance(Vector1,Vector2)->
	distance(Vector1,Vector2,0).	
distance([Val1|Vector1],[Val2|Vector2],Acc)->
	distance(Vector1,Vector2,Acc+math:pow(Val2-Val1,2));
distance([],[],Acc)->
	math:sqrt(Acc).
