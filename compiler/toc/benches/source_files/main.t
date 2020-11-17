%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    [REDACTED] - ICS3C Culminating    %
%       2019-06-03 -> 2016-06-17       %
%                                      %
%               3L Tankz               %
%    A multiplayer tank game where     %
%      you try to be the last one      %
%              standing                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

import
    % UI Library
    UI in "lib/ui_util.tu",
    % Main classes
    PersistentData in "classes/persistent.t",
    GameStates in "classes/state.t",
    Options in "classes/options.t"


var winID : int := Window.Open("graphics:1024;640;offscreenonly,title:3L Tankz,position:center;middle")
put "3L Tankz: Work in Progress"

% Timestep at which the game is updated at
const UPDATE_QUANTUM : real := 1000 / 60

% Whether the game should run or not (handles graceful exits)
var isRunning : boolean := true

% Performance monitoring variables
var ups, fps : int := 0
var lastUps, lastFps : int := 0
var frametimer : int := 0

% Realtime performance variables
var renderTime : int := 0
var totalUpdateTime : int := 0
var totalUpdates : int := 0
var avgUpdateTime : real

% Current Play state
var currentState : int := STATE_MAIN_MENU
% State container
var stateContainer : ^GameState := nil
% Current game options
var gameOptions : ^Options := nil

%%% Main Methods %%%
% Processes user input
proc processInput ()
    stateContainer -> processInput ()
end processInput

% Updates required things 
proc update (elapsed : int)
    if stateContainer not= nil then
        stateContainer -> update (elapsed)
    end if
    
    if currentState not= GameStates.requestState then
        var lastStateContainer : ^GameState := stateContainer
        
        case GameStates.requestState of
            label STATE_MAIN_MENU:
                var menuMenu : ^MainMenuState
                new MainMenuState, menuMenu
                menuMenu -> initState ()
                
                stateContainer := menuMenu
            label STATE_PLAY_GAME:
                var playPlay : ^PlayState
                new PlayState, playPlay
                playPlay -> initState ()
                
                stateContainer := playPlay
            label STATE_EXIT:
                isRunning := false
                stateContainer := nil
            label :
                % Nothing to do
                return
        end case
        
        currentState := GameStates.requestState
        
        % Alert game state root of a transition
        lastStateContainer -> onStateSwitch (stateContainer)
        
        % Get rid of the old state
        lastStateContainer -> freeState ()
        free lastStateContainer
    end if
end update

% Draws the required things
proc render (pt : real)
    if stateContainer not= nil then
        stateContainer -> render (pt)
    end if
end render

% Initializes the game
proc initGame ()
    % Setup multi-button input
    Mouse.ButtonChoose("multibutton")
    
    % Initialize the base menu state
    new MainMenuState, stateContainer
    MainMenuState (stateContainer).initState()
end initGame

% Main entry point
proc run ()
    initGame ()

    % Game loop from https://gameprogrammingpatterns.com/game-loop.html#play-catch-up
    var lastTime : int := Time.Elapsed
    var catchup : real := 0
    
    % Enter main game loop
    loop
        var now : int := Time.Elapsed
        var elapsed : int := now - lastTime
        lastTime := now
        catchup += elapsed
        
        % Prep for everything
        colourback (23)
        colour (white)
        cls
        
        %% Input Section %%
        % Handle input events
        processInput ()
        
        
        %% Update Section %%
        % Update at a fixed rate
        var nowUpdate : int := Time.Elapsed
        loop
            % Stop once we've caught up with the lag
            exit when catchup < UPDATE_QUANTUM
            
            update (floor(UPDATE_QUANTUM))
            
            % Decrease the catchup duration
            catchup -= UPDATE_QUANTUM
            
            ups += 1
            totalUpdates += 1
        end loop
        totalUpdateTime += (Time.Elapsed - nowUpdate)
        
        
        %% Render Section %%
        % Draw everything (calculate update interpolation also)
        var nowRender : int := Time.Elapsed
        render (catchup / (UPDATE_QUANTUM * 1000))
        fps += 1
        renderTime := Time.Elapsed - nowRender
        
        
        %% PerfMon Section %%
        % Update frames & updates per second
        if Time.Elapsed - frametimer > 1000 then
            lastUps := ups
            lastFps := fps
            
            ups := 0
            fps := 0
            frametimer := Time.Elapsed
            
            totalUpdates := 0
            totalUpdateTime := 0
        end if
        
        % Print out FPS & UPS
        colourback (black)
        colour (white)
        locate (maxrow, 1)
        put lastUps, " ", lastFps..
        
        put " (" ..
        % Print out realtime render time
        if renderTime not= 0 then
            put renderTime : 4 ..
        end if
        
        put ", " ..
        if totalUpdates not= 0 then
            put totalUpdateTime / totalUpdates : 4 : 2 ..
        end if
        put ") " ..
        
        
        % Display everything
        View.Update ()
        
        %loop exit when hasch end loop
        
        % Handle graceful exit
        exit when not isRunning
    end loop
    
    % Free all resources
    if stateContainer not= nil then
        stateContainer -> freeState ()
    end if
end run

run ()
Window.Close (winID)
