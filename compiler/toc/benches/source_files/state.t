unit
% Module containing all of the possible game states
% Also handles transitions between game states
module GameStates
    import
            UI in "../lib/ui_util.tu",
            Constants in "../constants.t",
            PersistentData in "persistent.t",
            Match in "match.t",
            InputControllers in "input.t"
    export
        ~.* var GameState,
        ~.* var PlayState,
        ~.* var MainMenuState,
        requestState,
        ~.* STATE_EXIT,
        ~.* STATE_MAIN_MENU,
        ~.* STATE_PLAY_PREP,
        ~.* STATE_PLAY_GAME,
        ~.* STATE_PLAY_FINI,
        ~.* STATE_OPTIONS,
        ~.* STATE_HELP

    %%% State Identifiers %%%
    % Valid states to be in
    % Game exit state
    const *STATE_EXIT      : int := -1
    % Base main menu state
    const *STATE_MAIN_MENU : int := 0
    % Play state & related sub states
    const *STATE_PLAY_PREP : int := 1
    const *STATE_PLAY_GAME : int := 2
    const *STATE_PLAY_FINI : int := 3
    % Options menu
    const *STATE_OPTIONS   : int := 4
    % Help menu (TODO)
    const *STATE_HELP      : int := 5

    % Indicates the requsted state that the game should switch to
    % Declared up here as there is no forward declerations of anything that
    % isn't a function or a procedure
    var pervasive requestState : int := STATE_MAIN_MENU

    %%% State Management Forward Decleration %%%
    
    
    %%% These are here since Turing doesn't allow for these methods to be used
    %%% as method variables
    %%% Menu Callbacks %%%
    fcn pervasive playCB (self_ :^ UIElement, evt : ^UIEvent) : int
        if evt -> evtType not= EVT_MOUSE_BUTTON or evt -> evtData.bt_state not= MOUSE_CLICKED then
            result EVENT_PROPAGATE
        end if
    
        % Enter play prep state
        requestState := STATE_PLAY_GAME
    
        result EVENT_CONSUME
    end playCB
    
    fcn pervasive helpCB (self_ :^ UIElement, evt : ^UIEvent) : int
        if evt -> evtType not= EVT_MOUSE_BUTTON or evt -> evtData.bt_state not= MOUSE_CLICKED then
            result EVENT_PROPAGATE
        end if
    
        % Enter help state
        requestState := STATE_HELP
    
        result EVENT_CONSUME
    end helpCB
    
    fcn pervasive optionsCB (self_ :^ UIElement, evt : ^UIEvent) : int
        if evt -> evtType not= EVT_MOUSE_BUTTON or evt -> evtData.bt_state not= MOUSE_CLICKED then
            result EVENT_PROPAGATE
        end if
    
        % Enter options state
        requestState := STATE_OPTIONS
    
        result EVENT_CONSUME
    end optionsCB
    
    fcn pervasive exitCB (self_ :^ UIElement, evt : ^UIEvent) : int
        if evt -> evtType not= EVT_MOUSE_BUTTON or evt -> evtData.bt_state not= MOUSE_CLICKED then
            result EVENT_PROPAGATE
        end if
    
        % Exit the game
        requestState := STATE_EXIT
    
        result EVENT_CONSUME
    end exitCB
        

    % Base class for all game states
    class pervasive GameState
        import UI
        export all
        
        % Initializes the GameState's components
        deferred proc initState ()
        % Processes user input
        deferred proc processInput ()
        % Updates the necessary components
        deferred proc update (elapsed : int)
        % Renders the necessary components
        deferred proc render (partialTicks : real)
        % Called when the state is being switched to another one
        deferred proc onStateSwitch (other : ^GameState)
        % Frees resources related to this GameState
        deferred proc freeState ()
        % Gets the UI content root
        deferred fcn getContentRoot () : ^UIElement
        
        %%% Default Implementations %%%
        body fcn getContentRoot
            result nil
        end getContentRoot
    end GameState
    
    % Main menu state
    class MainMenuState
        inherit GameState
        import UI
        
        const TEXT_GAME_TITLE : string := "3L Tankz"
        const TEXT_COPYRIGHT  : string := chr (16#A9) + " 2019 Third Lane Games"
        const TEXT_PLAY       : string := "Play"
        const TEXT_HELP       : string := "Help"
        const TEXT_OPTIONS    : string := "Options"
        const TEXT_EXIT       : string := "Exit"
        
        var contentRoot_ : ^UIElement := nil
        var inputState_ : ^InputState := nil
        
        % Fonts
        var fontTitle_ : int := 0
        var fontSuperText_ : int := 0
        var fontNormal_ : int := 0
        
        
        %%% Method Implementations %%%
        body proc initState ()
            % Setup the fonts
            fontTitle_     := Font.New ("Niagara Engraved:64x32")
            fontSuperText_ := Font.New ("Courier New:28x16")
            fontNormal_    := Font.New ("Courier New:16x8")
            
            % Setup UI stuff
            new inputState_
            new UIEvent, inputState_ -> uiEvt
            new UIElement, contentRoot_
            contentRoot_ -> InitElement (0, 0, maxx, maxy)
            
            inputState_ -> wasPressed := false
            inputState_ -> lastButtonTime := 0
            inputState_ -> lastX := 0
            inputState_ -> lastY := 0
            inputState_ -> lastButtonState := 0
            inputState_ -> mouseX := 0
            inputState_ -> mouseY := 0
            inputState_ -> mouseButtons := 0
            inputState_ -> mouseState := MOUSE_RELEASED
            
            inputState_ -> uiEvt -> ChangeType (EVT_NONE)
            
            % Setup the four buttons (Play, Options, Help, Exit)
            const BTTN_HEIGHT  : int := 60
            const BTTN_WIDTH   : int := 200
            const BTTN_SPACING : int := 10
            
            var button : ^UIButton := nil
            
            
            % Play Button
            new UIButton, button
            button -> Init ((maxx - BTTN_WIDTH)  div 2,
                            (maxy - BTTN_HEIGHT) div 2 + BTTN_HEIGHT + BTTN_SPACING,
                            BTTN_WIDTH,
                            BTTN_HEIGHT,
                            TEXT_PLAY,
                            fontSuperText_)
            button -> SetBackgroundColours (26, 22, 24, 28)
            button -> SetTextColour (18, 20)
            button -> SetEventCallback (playCB)
            contentRoot_ -> AddChild (button)
            
            
            % Help Button
            new UIButton, button
            button -> Init ((maxx - BTTN_WIDTH)  div 2,
                            (maxy - BTTN_HEIGHT) div 2,
                            BTTN_WIDTH,
                            BTTN_HEIGHT,
                            TEXT_HELP,
                            fontSuperText_)
            button -> SetBackgroundColours (26, 22, 24, 28)
            button -> SetTextColour (18, 20)
            button -> SetEventCallback (helpCB)
            contentRoot_ -> AddChild (button)
            
            
            % Options Button
            new UIButton, button
            button -> Init ((maxx - BTTN_WIDTH)  div 2,
                            (maxy - BTTN_HEIGHT) div 2 - (BTTN_HEIGHT + BTTN_SPACING),
                            BTTN_WIDTH,
                            BTTN_HEIGHT,
                            TEXT_OPTIONS,
                            fontSuperText_)
            button -> SetBackgroundColours (26, 22, 24, 28)
            button -> SetTextColour (18, 20)
            button -> SetEventCallback (optionsCB)
            contentRoot_ -> AddChild (button)
            
            
            % Exit Button
            new UIButton, button
            button -> Init ((maxx - BTTN_WIDTH)  div 2,
                            (maxy - BTTN_HEIGHT) div 2 - (BTTN_HEIGHT + BTTN_SPACING) * 2,
                            BTTN_WIDTH,
                            BTTN_HEIGHT,
                            TEXT_EXIT,
                            fontSuperText_)
            button -> SetBackgroundColours (26, 22, 24, 28)
            button -> SetTextColour (18, 20)
            button -> SetEventCallback (exitCB)
            contentRoot_ -> AddChild (button)
        end initState
        
        % Processes user input
        body proc processInput ()
            UI.ProcessEvents (contentRoot_, inputState_)
        end processInput
        
        % Updates the necessary components
        body proc update (elapsed : int)
            % Update the UI elements
            contentRoot_ -> Update ()
        end update
        
        % Renders the necessary components
        body proc render (partialTicks : real)
            % Draw game title
            Font.Draw (TEXT_GAME_TITLE, (maxx - Font.Width (TEXT_GAME_TITLE, fontTitle_)) div 2, (maxy * 3) div 4, fontTitle_, white)
            
            % Draw copyright
            Font.Draw (TEXT_COPYRIGHT, (maxx - Font.Width (TEXT_COPYRIGHT, fontNormal_)) div 2, 10, fontNormal_, white)
            
            % Draw the UI elements
            contentRoot_ -> Render (0, 0)
        end render
        
        % Frees resources related to this GameState
        body proc freeState ()
        
            % Fonts
            Font.Free (fontNormal_)
            Font.Free (fontSuperText_)
            Font.Free (fontTitle_)
        
            % The rest
            free contentRoot_
            free inputState_
        end freeState
        
        % UI Content root
        body fcn getContentRoot
            result contentRoot_
        end getContentRoot
        
        % State switch alert
        body proc onStateSwitch
            if other = nil then
                % Nothing to do
                return
            end if
        
            UI.OnContentRootChange (getContentRoot(), other -> getContentRoot (), inputState_)
        end onStateSwitch
    end MainMenuState
    
    % Play State
    class PlayState
        inherit GameState
        import MatchData, Match, InputControllers, Mouse
        
        % The current game match
        var match : ^Match
        
        % Colours of every player
        const PLAYER_CLR : array 0 .. 3 of int := init (40, 54, 48, 43)
        % Inputs for all players
        var inputs : array 0 .. 63 of ^InputController
        % Persistent match data
        var matchData_ : ^MatchData := nil
        
        % Camera for drawing the entire match
        var cameraX, cameraY : real := 0
        
        % Current round & total rounds to play
        var currentRound : int := 1
        var totalRounds : int := 5
        
        %% Fonts IDs %%
        % Fonts for player info
        var fontPlayerInfo : int
        
        
        % Draws a player tank
        % Starts from the bottom left corner, instead of the centre
        proc drawTank (scale, x, y, angle : real, base_colour : int)
            var effX, effY : real := 0
            effX := x - (cosd (angle) * BASE_LENGTH - BASE_WIDTH * sind (angle)) * scale
            effY := y + (sind (angle) * BASE_LENGTH + BASE_WIDTH * cosd (angle)) * scale
        
            % Points used for polygon drawing
            var polyX, polyY : array 1 .. 4 of int
        
            %% Base %%
            var baseOffX : real := BASE_WIDTH * -sind (angle)
            var baseOffY : real := BASE_WIDTH * +cosd (angle)
            polyX (1) := round (effX + (-cosd (angle) * BASE_LENGTH + baseOffX) * scale)
            polyX (2) := round (effX + (+cosd (angle) * BASE_LENGTH + baseOffX) * scale)
            polyX (3) := round (effX + (+cosd (angle) * BASE_LENGTH - baseOffX) * scale)
            polyX (4) := round (effX + (-cosd (angle) * BASE_LENGTH - baseOffX) * scale)
            polyY (1) := round (effY + (-sind (angle) * BASE_LENGTH + baseOffY) * scale)
            polyY (2) := round (effY + (+sind (angle) * BASE_LENGTH + baseOffY) * scale)
            polyY (3) := round (effY + (+sind (angle) * BASE_LENGTH - baseOffY) * scale)
            polyY (4) := round (effY + (-sind (angle) * BASE_LENGTH - baseOffY) * scale)
        
            drawfillpolygon(polyX, polyY, 4, base_colour + 24 * 3)
            
            
            %% Barrel %%
            var barrelOffX, barrelOffY : real := 0
            barrelOffX := BARREL_RADIUS * -sind (angle)
            barrelOffY := BARREL_RADIUS * +cosd (angle)
            
            polyX (1) := round (effX + (cosd (angle) * BARREL_OFFSET - barrelOffX) * scale)
            polyX (2) := round (effX + (cosd (angle) * BARREL_OFFSET + barrelOffX) * scale)
            polyX (3) := round (effX + (cosd (angle) * BARREL_LENGTH + barrelOffX) * scale)
            polyX (4) := round (effX + (cosd (angle) * BARREL_LENGTH - barrelOffX) * scale)
            polyY (1) := round (effY + (sind (angle) * BARREL_OFFSET - barrelOffY) * scale)
            polyY (2) := round (effY + (sind (angle) * BARREL_OFFSET + barrelOffY) * scale)
            polyY (3) := round (effY + (sind (angle) * BARREL_LENGTH + barrelOffY) * scale)
            polyY (4) := round (effY + (sind (angle) * BARREL_LENGTH - barrelOffY) * scale)
            
            drawfillpolygon(polyX, polyY, 4, 24)
            
            
            %% Head %%
            drawfilloval (round(effX), round(effY), round (HEAD_RADIUS * scale), round(HEAD_RADIUS * scale), base_colour)
        end drawTank
        
        % Begins the game match
        proc beginMatch ()
            var width, height : int
            
            % Have preference for generating big maps
            if Rand.Real > 0.25 then
                width  := Rand.Int (7, 14)
                height := Rand.Int (5, 7)
            else
                width  := Rand.Int (3, 14)
                height := Rand.Int (3, 7)
            end if
            
            % Initialize the match
            cameraX :=  (maxx - width * TILE_SIZE) / 2
            % Have a 100 px region available at the bottom
            cameraY :=  (maxy - height * TILE_SIZE) / 2 + (100 / 2)
            
            new Match, match
            match -> initMatch (width, height)
            match -> setPersistent (matchData_)
            match -> setCamera (cameraX, cameraY)
            
            % Add the players
            match -> addPlayer (0, PLAYER_CLR (0),         0, 0,          inputs (0))
            match -> addPlayer (1, PLAYER_CLR (1), width - 1, 0,          inputs (1))
            match -> addPlayer (2, PLAYER_CLR (2), width - 1, height - 1, inputs (2))
            match -> addPlayer (3, PLAYER_CLR (3),         0, height - 1, inputs (3))
        end beginMatch
        
        body proc initState ()
            % Setup all of the inputs
            for i : 0 .. upper (inputs)
                inputs (i) := nil
            end for
            
            % Setup keyboard input controllers
            for i : 0 .. 2
                new KeyboardController, inputs (i)
                KeyboardController (inputs (i)).initController ()
                KeyboardController (inputs (i)).setScheme (i + 1)
            end for
            
            % Setup mouse controller
            new MouseController, inputs (3)
            MouseController (inputs (3)).initController ()
            MouseController (inputs (3)).setScheme (MOUSE_SCHEME_LEFT)
            
            % Setup match data
            new MatchData, matchData_
            matchData_ -> initData ()
        
            % Setup the match
            beginMatch ()
            
            % Setup the fonts
            fontPlayerInfo := Font.New ("Courier New:32x16:Bold")
        end initState
        
        body proc processInput ()
            for i : 0 .. upper (inputs)
                if inputs (i) not= nil then
                    InputController (inputs (i)).update ()
                end if
            end for
        end processInput
        
        body proc update (elapsed : int)
            match -> update (elapsed)
    
            if match -> matchEnded then            
                % Restart the match
                match -> freeMatch ()
                free Match, match
                
                currentRound += 1
                
                if currentRound <= totalRounds then
                    beginMatch ()
                else
                    requestState := STATE_MAIN_MENU
                end if
            end if
        end update
        
        body proc render (partialTicks : real)
            % Draw the match
            match -> render (partialTicks)
            
            % Draw the player info
            % Width of one tank score
            const SCORE_WIDTH : int := (BASE_WIDTH * 4) + 80
            const SCORE_CENTRE_OFF : int := (maxx - SCORE_WIDTH * 4) div 2
            
            for i : 0 .. 3                
                % Draw the player tanks
                var tankX : int := round(SCORE_WIDTH * i + SCORE_CENTRE_OFF)
                % Hidden muldiv 2 at the end
                var tankY : int := (cameraY - BASE_LENGTH * 4) div 2
                
                % Draw the player tank
                drawTank (2, tankX, tankY, 90, PLAYER_CLR (i))
                
                % Show the player wins
                Font.Draw (intstr (matchData_ -> playerWins (i)),
                           tankX + BASE_WIDTH * 4 + 10,
                           tankY + BASE_LENGTH * 2,
                           fontPlayerInfo,
                           18)
            end for
            
            % Draw the current round & number of rounds
            var roundsDisplay : string := ""
            roundsDisplay += intstr (currentRound)
            roundsDisplay += "/"
            roundsDisplay += intstr (totalRounds)
            
            Font.Draw (roundsDisplay,
                      ((SCORE_CENTRE_OFF + SCORE_WIDTH * 4)),
                       (cameraY - BASE_LENGTH * 4) div 2 + BASE_LENGTH * 2,
                       fontPlayerInfo,
                       18)
        end render
        
        body proc freeState ()
            for i : 0 .. upper (inputs)
                if inputs (i) not= nil then
                    free inputs (i)
                end if
            end for
        
            if match not= nil then
                match -> freeMatch ()
                free match
            end if
            
            if matchData_ not= nil then
                free matchData_
            end if
        end freeState
        
        % State switch alert
        body proc onStateSwitch
            var inputState : ^InputState := nil
            new inputState
            new inputState -> uiEvt
            
            inputState -> wasPressed := false
            inputState -> lastButtonTime := 0
            inputState -> lastX := 0
            inputState -> lastY := 0
            inputState -> lastButtonState := 0
            inputState -> mouseX := 0
            inputState -> mouseY := 0
            inputState -> mouseButtons := 0
            inputState -> mouseState := MOUSE_RELEASED
            inputState -> uiEvt -> ChangeType (EVT_NONE)
            
            Mouse.Where (inputState -> mouseX, inputState -> mouseY, inputState -> mouseButtons)
        
            UI.OnContentRootChange (getContentRoot(), other -> getContentRoot (), inputState)
        end onStateSwitch
    end PlayState
    
    
    %%% State Containers %%%
    %%% State Management %%%
end GameStates