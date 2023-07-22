    processor 6502
    include "vcs.h"
    include "macro.h"

;==================================================================================
; Program Definitions
;==================================================================================
    
SPRITE_HEIGHT = 18
TIMER_LIMIT1 = 5
TIMER_LIMIT2 = 20

;==================================================================================
; Program Variables
;==================================================================================

    SEG.U Variables
    ORG $80

PS_temp ds 1

SpriteAddrPtr ds 2
SpriteYPos ds 1
LastSpriteXPos ds 1
LastSpriteYPos ds 1
PlayerVelocityMask ds 1

SpriteGhostAddrPtr ds 2

SpriteXPos ds 1
GhostSpriteXPos ds 1
CurWaferXPos0 ds 1

GhostSpriteYPos ds 1

WaferXPosIndex ds 1
WaferArray ds 6
PickedUpWaferBuffer ds 1
WaferDrawingArray ds 7

SpriteAnimationIndex ds 1
IsFrameGoingUp ds 1

ScoreHundreds ds 1
ScoreTens ds 1
ScoreOnes ds 1

HundredsOffset ds 1
TensOffset ds 1
OnesOffset ds 1

ScoreDisplayBufferHundreds ds 1
ScoreDisplayBufferTens ds 1
ScoreDisplayBufferOnes ds 1

TimerCounter5 ds 1
TimerCounter20 ds 1

PlayerReflectedBuffer ds 1
GhostReflectedBuffer ds 1

LifeCount ds 1
WaferCount ds 1

WasButtonPressed ds 1

; Game State - 0 = PlayingGame; 1 = PlayingDeathAnimation; 2 = GamePaused;
; 3 = GameOver; 4 = PlayingWinningScreen
GameState ds 1

;==================================================================================
; Program Initialization
;==================================================================================

    SEG Executable
    ORG $F000

Reset
ClearMemory
    LDX #0
    TXS
    PHA
    TXA

ClearMemoryLoop
    PHA
    DEX
    BNE ClearMemoryLoop

    JSR InitVariables

;==================================================================================
; Main Loop
;==================================================================================

Main
    JSR HandleVSync
    JSR HandleVBlank
    JSR MainKernel
    JSR Overscan
    JMP Main

;==================================================================================
; HandleVSync
;==================================================================================

HandleVSync
    LDA #0
    STA VBLANK
    LDA #2
    STA VSYNC

VSyncLoop
    STA WSYNC
    STA WSYNC
    STA WSYNC

    RTS

;==================================================================================
; HandleVBlank
; Handle Part of Game Logic
;==================================================================================

HandleVBlank
    
    ; Initialize TIM64T
    LDA #42
    STA TIM64T

    ; Prepare Registers for VBLANK
    LDA #0
    STA VSYNC
    LDA #%01000010
    STA VBLANK

    ; Processing Tasks
    JSR GetControllerInputsDirectional
    JSR GetControllerInputsButton
    JSR GetController2InputsDEBUG
    JSR UpdateEntities
    JSR PrepareWaferDrawingArray
    
VBlankLoop
    LDA INTIM
    BNE VBlankLoop

    STA WSYNC
    RTS

;==================================================================================
; MainKernel - Main Routine to Display Game
;==================================================================================
    
MainKernel
    LDA #0                 
    STA VBLANK              
    
    ; Position Player, Ghost and Wafer According to XPos
    JSR PositionSpriteX

    LDX #191               
    STA WSYNC

MainFrameLoop
    ; First Line
    ; Load First Playfield
    LDA MainBoard_STRIP_0,x ; 4  4   0
    STA PF0                 ; 3  7   12

    LDA MainBoard_STRIP_1,x ; 4  11   33
    STA PF1                 ; 3  14   42

    LDA MainBoard_STRIP_2,x ; 4  18
    STA PF2                 ; 3  21
    
    ; Check if line has wafer
    LDA CheckLineBytes,x
    BEQ DoNotDrawWafer
    
    ; Check if wafer was picked up
    TAY
    LDA WaferDrawingArray-1,y
    BNE DoNotDrawWafer

DoDrawWafer
    LDA #%10
    STA ENAM0
    JMP AfterDrawingWafer

DoNotDrawWafer
    LDA #0
    STA ENAM0 
    
AfterDrawingWafer
    ; Check Vertical Sprite (P0) Drawing
    TXA                     ; 2
    SEC                     ; 2
    SBC SpriteYPos          ; 3
	ADC #SPRITE_HEIGHT      ; 2
    BCC SkipDrawingP0       ; 3

    ; Load P0 Sprite Data
    TAY                     ; 3
    LDA (SpriteAddrPtr),y   ; 5
    STA GRP0                ; 3

SkipDrawingP0
    ; Decrease X and Go To Next Line
    STA WSYNC               ; 3
    DEX                     ; 2
    BEQ MainFrameLoopEnd    ; 2 
    
    ; Second Line

    ; Load First Playfield
    LDA MainBoard_STRIP_0,x ; 4
    STA PF0                 ; 3

    LDA MainBoard_STRIP_1,x ; 4
    STA PF1                 ; 3

    LDA MainBoard_STRIP_2,x ; 4
    STA PF2                 ; 3

    ; Check Ghost Vertical Drawing
    TXA                     ; 2
    SEC                     ; 2
    SBC GhostSpriteYPos     ; 3
	ADC #SPRITE_HEIGHT      ; 2
    BCC SkipGhostDrawing    ; 3
    
    ; Load Ghost Sprite Data
    TAY                     ; 3
    LDA (SpriteGhostAddrPtr),y   ; 5
    STA GRP1               ; 3
    
SkipGhostDrawing
    ; Decrease X and Go To Next Line
    STA WSYNC               ; 3
    DEX                     ; 2
	BNE MainFrameLoop       ; 2

MainFrameLoopEnd
    ; Clearing Playfield Registers
    LDA #0
    STA PF0
    STA PF1
    STA PF2

    STA GRP0
    STA GRP1

    RTS

;==================================================================================
; Overscan
;==================================================================================

Overscan
    
    ; Prepare Registers for Overscan Drawing
    LDA #0
    STA REFP0
    STA REFP1

    LDA #$0E
    STA COLUP1

    LDA #%011
    
    ; Triplicate P1 (Score)
    STA NUSIZ1

    ; Select Ammount of Icons According to LifeCount
    LDY LifeCount

    ; Select position of Score
    STA RESP1

    LDA LivesNUSIZ0LookUp,y
    STA NUSIZ0
    
    ; Calculate Hundreds Offset
    LDX ScoreHundreds
    LDA MultBy14,x
    STA HundredsOffset
    
    ; Calculate Tens Offset
    LDX ScoreTens
    LDA MultBy14,x
    STA TensOffset
    
    ; Calculate Ones Offset
    LDX ScoreOnes
    LDA MultBy14,x
    STA OnesOffset

    STA WSYNC
    
    ; Select position of Lives
    STA RESP0

    LDX #29
OverscanLoop
    
    ; Is it time to draw?
    CPX #16
    BCC SmallerThan16
    
BiggerThan16
    ; Time to draw
    
    ; Draw Lives
    LDA SpriteLifeData,x
    STA GRP0
    
    ; Prepare draw bits for hundreds
    CLC
    TXA
    ADC HundredsOffset
    TAY
    LDA BottomData,y
    STA GRP1 
    
    ; Prepare draw bits for ones
    CLC
    TXA
    ADC OnesOffset
    TAY
    LDA BottomData,y
    STA ScoreDisplayBufferOnes
    
    ; Prepare draw bits for tens
    CLC
    TXA
    ADC TensOffset
    TAY
    LDA BottomData,y
   
    ; Draw Tens
    STA GRP1
    
    ; Draw Ones
    LDA ScoreDisplayBufferOnes
    STA GRP1
    
SmallerThan16    
    DEX
    STA WSYNC
    BNE OverscanLoop

    ; Clear Player Registers
    LDA #0
    STA GRP0
    STA GRP1

    ; Return NUSIZ0 and NUSIZ1
    LDA #%100000
    STA NUSIZ0
    STA NUSIZ1

    ; Return P0 Register Reflection Value
    LDA PlayerReflectedBuffer
    STA REFP0

    ; Return P1 Register Reflection Value
    LDA GhostReflectedBuffer
    STA REFP1

    ; Return P1 color
    LDA #$58
    STA COLUP1

    RTS

;==================================================================================
; InitVariables
;==================================================================================

InitVariables
    ; Initialize BG Color and PF Color
    LDA #$00
    STA COLUBK
    LDA #$72
    STA COLUPF

    ; Initialize Player Color and Pattern
    LDA #$1E
    STA COLUP0
    LDA #$58
    STA COLUP1

    ; Initialize CTRLPF
    LDA #1
    STA CTRLPF
    
    ; Initialize Missile Graphics
    LDA #%100000
    STA NUSIZ0

    ; Initialize Variables
    
    ; SpriteXPos, LastXPos, YPos and LastYPos
    LDA #77
    STA SpriteXPos
    STA LastSpriteXPos
    LDA #107
    STA SpriteYPos
    STA LastSpriteYPos

    ; SpriteGhostXPos and YPos
    LDA #77
    STA GhostSpriteXPos
    LDA #140
    STA GhostSpriteYPos

    ; LifeCount
    LDA #3
    STA LifeCount

    ; Set SpriteAnimationIndex
    STA SpriteAnimationIndex

    ; GameState
    LDA #4
    STA GameState

    ; TimerCounter20
    LDA #200
    STA TimerCounter20

    ; ScoreHundreds
    LDA #-1
    STA ScoreHundreds

    ; Set Controller Inputs
    LDA #0
    STA SWACNT

    ; Set P0 and P1 Delays
    STA VDELP0

    ; Set Wafer XPos Index
    STA WaferXPosIndex

    ; PacmanSpriteDataPointer
    LDA #<Sprite0Data
    STA SpriteAddrPtr
    LDA #>Sprite0Data
    STA SpriteAddrPtr+1

    ; GhostSpriteDataPointer
    LDA #<SpriteGhostData
    STA SpriteGhostAddrPtr
    LDA #>SpriteGhostData
    STA SpriteGhostAddrPtr+1

    RTS

;==================================================================================
; PositionSpriteX - Subroutine to position all sprites (P0,P1,M0)
;==================================================================================

PositionSpriteX
    STA WSYNC                                   ; 3
    STA HMCLR  ; clear any previous movement    ; 3

    LDX #3     ; sprite index                   ; 2

PosSP1   

    LDA SpriteXPos-1,x                          ; 4
    TAY                                         ; 2

    ; Divide by 16
    LSR                                         ; 2
    LSR                                         ; 2
    LSR                                         ; 2
    LSR                                         ; 2
    STA PS_temp                                 ; 3

    TYA                                         ; 2
    AND #15                                     ; 2

    CLC                                         ; 2

    ADC PS_temp                                 ; 3
    LDY PS_temp                                 ; 3

    CMP #15                                     ; 2
    BCC NH1                                     ; 3
    SBC #15                                     ; 2
    INY                                         ; 2

NH1
    ; Use remainder for fine adjustment
    EOR #7                                      ; 2
    ASL                                         ; 2
    ASL                                         ; 2
    ASL                                         ; 2
    ASL                                         ; 2

    STA HMP0-1,x    ; fine movement             ; 4
    STA WSYNC                                   ; 3

    JSR Ret1         ; just a 12 cycle delay     ; 12
    BIT 0           ; 15 cycles = 3 loops :)    ; 3


Jiggle1  
    DEY                                         ; 2
    BPL Jiggle1                                  ; 3

    STA RESP0-1,x                               ; 4

    DEX                                         ; 2
    BNE PosSP1                                   ; 3

    STA WSYNC                                   ; 3
    STA HMOVE                                   ; 3
    
Ret1
    RTS                                         ; 6

;==================================================================================
; PrepareWaferDrawingArray - Prepare Wafer Drawing Array for MainKernel
;==================================================================================

PrepareWaferDrawingArray
    
    ; Verify if wafer is picked up for each WaferY -> 1 = Picked up (do not draw), 0 = Not picked up (draw)
    LDY WaferXPosIndex
    LDA WaferArray,y
    TAX

    LDY #7
PrepareWaferLoop
    TXA 
    AND FindWaferLookUp,y
    BEQ NotPickedUp

PickedUp
    LDA #1
    STA WaferDrawingArray-1,y
    JMP PrepareWaferDrawingArrayContinue

NotPickedUp
    LDA #0
    STA WaferDrawingArray-1,y

PrepareWaferDrawingArrayContinue
    DEY
    BNE PrepareWaferLoop
    
    RTS

;==================================================================================
; UpdateEntities - Update Game Logic
;==================================================================================

UpdateEntities
    ; Check if Game Is Paused
    LDA GameState
    CMP #2
    BEQ EndUpdateEntities
    
    ; Update Timer
    JSR UpdateTimer
    
    ; Get Out End of Level (If it's the case)
    JSR LevelEndOut

    ; Apply Animation Frame
    JSR ApplyAnimationFrame

    ; Check if it's playing the end of the level
    LDA GameState
    CMP #4
    BEQ EndUpdateEntities

    ; Change Animation Frame
    JSR ChangeAnimationFrame
    
    ; Check Player Collision with PF
    JSR CheckPlayerCollisionPF

    ; Update Sprite Pos According to Velocity Mask
    JSR UpdatePlayerPosition

    ; Check Player Collision with Ghosts
    JSR CheckPlayerCollisionGhosts

    ; Check Player Collision with Wafers
    JSR CheckWaferCollision

    ; Remove Collisions After Checks
    STA CXCLR
    
    ; Increase Score According with Picked Up Wafer Buffer
    JSR UpdateScore

    ; Update Wafer XPos Index
    JSR UpdateWaferIndex

    ; Check Game Over
    LDA LifeCount
    BEQ GoToGameOver

    ; Check End of Level
    JSR CheckLevelEnd

EndUpdateEntities
    RTS

GoToGameOver
    LDA #3
    STA GameState
    JMP HandleGameOver

;==================================================================================
; CheckWaferCollision - Verify player collision with wafer
;==================================================================================

CheckWaferCollision
    ; Check Collision Between Player and Wafer
    BIT CXM0P
    BVC DidntGetWafer

GotWafer
    ; Routine to save wafer as picked up
    
    ; Y = WaferX
    LDY WaferXPosIndex
    
    ; Load WaferArray[WaferX]
    LDA WaferArray,y
    
    ; Find WaferY from SpriteYPos
    LDY SpriteYPos
    LDX ConvertPlayerPosToWaferIndex,y
    
    ; Mark Wafer Picked Up for Frame
    ORA FindWaferLookUp,x

    ; Save on WaferArray[WaferX]
    LDY WaferXPosIndex
    STA WaferArray,y

    ; Mark WaferPickedUp for UpdateScore
    LDA #1
    STA PickedUpWaferBuffer
    
DidntGetWafer
    RTS

;==================================================================================
; LevelEndOut - Get out of level end tune
;==================================================================================

LevelEndOut
    ; Verify if it's in level end
    LDA GameState
    CMP #4
    BNE LevelEndOutRet

    ; Verify Counter
    LDA TimerCounter20
    BNE LevelEndOutRet

    ; Out of Level End
    LDA #0
    STA GameState

    ; Give 100 Points
    INC ScoreHundreds

    ; Give a Free Life (Max 3)
    JSR GiveLife

LevelEndOutRet
    RTS

;==================================================================================
; GiveLife - Give an extra life to the player
;==================================================================================

GiveLife
    INC LifeCount
    LDA LifeCount
    CMP #4
    BNE GiveLifeRet

    LDA #3
    STA LifeCount

GiveLifeRet
    RTS

;==================================================================================
; CheckLevelEnd - Verify if all wafers were collected
;==================================================================================

CheckLevelEnd
    LDA WaferCount
    CMP #42
    BNE NotLevelEnd

LevelEnd
    LDA #0
    LDX #6

    ; Zero All Wafers
ZeroWaferArrayLoop
    DEX
    STA WaferArray,x
    CPX #0
    BNE ZeroWaferArrayLoop

    ; Zero WaferCount
    STA WaferCount

    ; Return Player to Center of Screen
    LDA #80
    STA SpriteXPos
    LDA #107
    STA SpriteYPos

    ; Start Playing Level End Tune
    LDA #4
    STA GameState

    ; Change Character Frame to 3
    LDA #3
    STA SpriteAnimationIndex
    LDA #0
    STA IsFrameGoingUp
    
    ; Prepare Timer For Level End Tune
    LDA #200
    STA TimerCounter20

NotLevelEnd
    RTS

;==================================================================================
; UpdateWaferIndex - increase wafer XPos Index and apply it
;==================================================================================

UpdateWaferIndex
    INC WaferXPosIndex
    
    LDX WaferXPosIndex
    CPX #6
    BNE ReturnWaferIndex

WaferIndexEquals6
    LDX #0
    STX WaferXPosIndex

ReturnWaferIndex
    LDA WaferXPosArray0,x
    STA CurWaferXPos0
    RTS

;==================================================================================
; CheckPlayerCollisionGhosts - Verify for Player Collision with Ghosts
;==================================================================================

CheckPlayerCollisionGhosts
    
    ; Check if death animation is playing
    LDA GameState
    CMP #1
    BEQ NoP0P1Collision
    
    ; Check Collision
    LDA CXPPMM
    AND #%10000000
    BEQ NoP0P1Collision

P0P1Collision
    ; Reset Player Velocity
    LDA #0
    STA PlayerVelocityMask

    ; Set Animation Frame to 4 and IsPlayingDeathAnimation
    LDA #4
    STA SpriteAnimationIndex

    LDA #1
    STA GameState

NoP0P1Collision
    RTS

;==================================================================================
; CheckPlayerCollisionPF - Verify for Player Collision with PF and Update Velocity Mask
;==================================================================================

CheckPlayerCollisionPF
    ; Verify P0PF Collision Bit
    LDA CXP0FB
    AND #%10000000
    BEQ PlayerPFCollisionNotDetected

    ; Check Where Player Was Going
    LDX PlayerVelocityMask
    
    ; Return Player to Previous Spot Before Zeoring Velocity
    TXA
    AND #%1000
    BNE WasGoingRight

    TXA
    AND #%0100
    BNE WasGoingLeft

    TXA
    AND #%0010
    BNE WasGoingDown

    TXA
    AND #%0001
    BNE WasGoingUp

    ; Return X and Y in case Player isnt moving and is still stuck
    LDA LastSpriteXPos
    STA SpriteXPos
    LDA LastSpriteYPos
    STA SpriteYPos

    JMP ZeroPlayerVelocity

WasGoingRight
    LDA LastSpriteXPos
    STA SpriteXPos
    
    DEC SpriteXPos
    DEC SpriteXPos
    
    JMP ZeroPlayerVelocity

WasGoingLeft
    LDA LastSpriteXPos
    STA SpriteXPos
    
    INC SpriteXPos
    INC SpriteXPos
    
    JMP ZeroPlayerVelocity

WasGoingDown
    LDA LastSpriteYPos
    STA SpriteYPos
    
    INC SpriteYPos
    INC SpriteYPos
    
    JMP ZeroPlayerVelocity

WasGoingUp
    LDA LastSpriteYPos
    STA SpriteYPos
    
    DEC SpriteYPos
    DEC SpriteYPos
    
ZeroPlayerVelocity
    LDA #0
    STA PlayerVelocityMask

PlayerPFCollisionNotDetected
    RTS

;==================================================================================
; UpdatePlayerPosition - Update Player Position According to Velocity Mask
;==================================================================================

UpdatePlayerPosition
    ; Verify Collision and Refuse to Move in case is colliding
    LDA CXP0FB
    AND #%10000000
    BNE UpdatePlayerPositionRet
    
    LDX PlayerVelocityMask
    
    TXA
    AND #%1000
    BNE GoRight

    TXA
    AND #%0100
    BNE GoLeft

    TXA
    AND #%0010
    BNE GoDown

    TXA
    AND #%0001
    BNE GoUp

    JMP UpdatePlayerPositionRet

GoRight
    LDA SpriteXPos
    STA LastSpriteXPos
    LDA SpriteYPos
    STA LastSpriteYPos
    
    INC SpriteXPos

    ; Verify If Sprite is in Far Right
    LDA SpriteXPos
    CMP #160
    BCC NotFarRight

    LDA #0
    STA SpriteXPos

NotFarRight
    JMP UpdatePlayerPositionRet

GoLeft
    LDA SpriteXPos
    STA LastSpriteXPos
    LDA SpriteYPos
    STA LastSpriteYPos
    
    DEC SpriteXPos

    ; Verify If Sprite is in Far Left
    LDA SpriteXPos
    CMP #0
    BNE NotFarLeft

    LDA #160
    STA SpriteXPos

NotFarLeft
    JMP UpdatePlayerPositionRet

GoDown
    LDA SpriteXPos
    STA LastSpriteXPos
    LDA SpriteYPos
    STA LastSpriteYPos
    
    DEC SpriteYPos
    JMP UpdatePlayerPositionRet

GoUp
    LDA SpriteXPos
    STA LastSpriteXPos
    LDA SpriteYPos
    STA LastSpriteYPos
    
    INC SpriteYPos

UpdatePlayerPositionRet
    RTS

;==================================================================================
; ChangeAnimationFrame
;==================================================================================

ChangeAnimationFrame
    ; Verify If Is Death Animation
    LDA GameState
    CMP #1
    BEQ FrameIsDeath
    
FrameNotDeath
    ; Verify TimerCounter5
    LDA TimerCounter5
    BNE DontChangeFrame

    ; Load Current Frame Index
    LDY SpriteAnimationIndex

    ; Verify Next Frame
    LDX IsFrameGoingUp
    BNE FrameIsGoingUp

FrameIsGoingDown
    DEY
    JMP FrameChanged

FrameIsGoingUp
    INY

FrameChanged
    ; Check if Index is Either 0 or 3
    STY SpriteAnimationIndex
    CPY #3
    BEQ ChangeDirection
    CPY #0
    BEQ ChangeDirection
    
    ; If not
    JMP FrameChangeReturn

ChangeDirection
    LDA IsFrameGoingUp
    EOR #1
    STA IsFrameGoingUp
    JMP FrameChangeReturn
    
FrameIsDeath
    ; Verify TimerCounter20
    LDA TimerCounter20
    BNE DontChangeFrame

CheckIfLastDeathFrame
    ; Check if Frame is 10 (Last Death Frame)
    LDA SpriteAnimationIndex
    CMP #10
    BNE ChangeDeathFrame

    ; End Death Animation
    LDA #0
    STA GameState
    STA SpriteAnimationIndex

    LDA #1
    STA IsFrameGoingUp

    ; Reset Player Coordinates
    LDA #77
    STA SpriteXPos
    LDA #107
    STA SpriteYPos

    ; Take One Life
    DEC LifeCount

    JMP FrameChangeReturn

ChangeDeathFrame
    INC SpriteAnimationIndex
    JMP FrameChangeReturn
    
DontRestartFrame
DontChangeFrame
FrameChangeReturn
    RTS

;==================================================================================
; ApplyAnimationFrame
;==================================================================================

ApplyAnimationFrame
    ; Check Counter
    LDA TimerCounter5
    BNE AnimationFrameRet
    
    LDA SpriteAnimationIndex
    BEQ Animation0
    CMP #1
    BEQ Animation1
    CMP #2
    BEQ Animation2
    CMP #3
    BEQ Animation3
    CMP #4
    BEQ Animation4
    CMP #5
    BEQ Animation5
    CMP #6
    BEQ Animation6
    CMP #7
    BEQ Animation7
    CMP #8
    BEQ Animation8
    CMP #9
    BEQ Animation9
    CMP #10
    BEQ Animation10

AnimationFrameRet
    JMP ApplyAnimationFrameRet
    
Animation0
    LDA #<Sprite0Data
    STA SpriteAddrPtr
    LDA #>Sprite0Data
    STA SpriteAddrPtr+1
    JMP ApplyAnimationFrameRet

Animation1
    LDA #<Sprite1Data
    STA SpriteAddrPtr
    LDA #>Sprite1Data
    STA SpriteAddrPtr+1
    JMP ApplyAnimationFrameRet

Animation2
    LDA #<Sprite2Data
    STA SpriteAddrPtr
    LDA #>Sprite2Data
    STA SpriteAddrPtr+1
    JMP ApplyAnimationFrameRet

Animation3
    LDA #<Sprite3Data
    STA SpriteAddrPtr
    LDA #>Sprite3Data
    STA SpriteAddrPtr+1
    JMP ApplyAnimationFrameRet

Animation4
    LDA #<Sprite4Data
    STA SpriteAddrPtr
    LDA #>Sprite4Data
    STA SpriteAddrPtr+1
    JMP ApplyAnimationFrameRet

Animation5
    LDA #<Sprite5Data
    STA SpriteAddrPtr
    LDA #>Sprite5Data
    STA SpriteAddrPtr+1
    JMP ApplyAnimationFrameRet

Animation6
    LDA #<Sprite6Data
    STA SpriteAddrPtr
    LDA #>Sprite6Data
    STA SpriteAddrPtr+1
    JMP ApplyAnimationFrameRet

Animation7
    LDA #<Sprite7Data
    STA SpriteAddrPtr
    LDA #>Sprite7Data
    STA SpriteAddrPtr+1
    JMP ApplyAnimationFrameRet

Animation8
    LDA #<Sprite8Data
    STA SpriteAddrPtr
    LDA #>Sprite8Data
    STA SpriteAddrPtr+1
    JMP ApplyAnimationFrameRet

Animation9
    LDA #<Sprite9Data
    STA SpriteAddrPtr
    LDA #>Sprite9Data
    STA SpriteAddrPtr+1
    JMP ApplyAnimationFrameRet

Animation10
    LDA #<Sprite10Data
    STA SpriteAddrPtr
    LDA #>Sprite10Data
    STA SpriteAddrPtr+1

ApplyAnimationFrameRet
    RTS

;==================================================================================
; GetControllerInputsDirectional - Update Player Velocity Mask According to Inputs
;==================================================================================

GetControllerInputsDirectional
    ; Check if game is in death animation
    LDA GameState
    CMP #1
    BEQ ControllerRet
    
    LDX SWCHA
    
    ; Check Right Input
    TXA
    AND #%10000000
    BEQ RightInput
    
    ; Check Left Input
    TXA
    AND #%01000000
    BEQ LeftInput
    
    ; Check Down Input
    TXA
    AND #%00100000
    BEQ DownInput
    
    ; Check Up Input
    TXA
    AND #%00010000
    BEQ UpInput

    ; No Input Detected
    JMP ControllerRet

RightInput
    LDA #%1000
    STA REFP0
    STA PlayerReflectedBuffer

    STA PlayerVelocityMask
    JMP ControllerRet

LeftInput
    LDA #0
    STA REFP0
    STA PlayerReflectedBuffer

    LDA #%0100
    STA PlayerVelocityMask
    JMP ControllerRet

DownInput
    LDA #%0010
    STA PlayerVelocityMask
    JMP ControllerRet

UpInput
    LDA #%0001
    STA PlayerVelocityMask

ControllerRet
    RTS

;==================================================================================
; GetControllerInputsButton - Pause Game
;==================================================================================

GetControllerInputsButton
    ; Verify Button Press
    LDA INPT4
    AND #%10000000
    TAY
    
    ; See if it's Edge
    CMP WasButtonPressed
    BEQ StateNotChanged

StateChanged
    ; Only React to One Edge
    CMP #0
    BNE ButtonNotPushed

    ; Check Expected Behavior if in Game Over Screen
    LDA GameState
    CMP #3
    BNE PauseUnpauseGame

RestartGame
    ; Fully Reset Game
    JMP Reset
    
PauseUnpauseGame
    ; Pause/Unpause Game
    LDA GameState
    CMP #0
    BEQ PauseGame

UnpauseGame
    LDA #0
    STA GameState
    JMP ControllerButtonRet

PauseGame
    LDA #2
    STA GameState

StateNotChanged
ButtonNotPushed
ControllerButtonRet
    STY WasButtonPressed
    RTS

;==================================================================================
; GetController2Inputs - DEBUG - Controls Ghost
;==================================================================================

GetController2InputsDEBUG
    LDX SWCHA
    
    ; Check Right Input
    TXA
    AND #%00001000
    BEQ RightInputGhost
    
    ; Check Left Input
    TXA
    AND #%00000100
    BEQ LeftInputGhost
    
    ; Check Down Input
    TXA
    AND #%00000010
    BEQ DownInputGhost
    
    ; Check Up Input
    TXA
    AND #%00000001
    BEQ UpInputGhost

    ; No Input Detected
    JMP ControllerRetGhost

RightInputGhost
    INC GhostSpriteXPos
    LDA #%1000
    STA REFP1
    STA GhostReflectedBuffer
    JMP ControllerRetGhost

LeftInputGhost
    DEC GhostSpriteXPos
    LDA #0
    STA REFP1
    STA GhostReflectedBuffer
    JMP ControllerRetGhost

DownInputGhost
    DEC GhostSpriteYPos
    JMP ControllerRetGhost

UpInputGhost
    INC GhostSpriteYPos

ControllerRetGhost
    RTS

;==================================================================================
; UpdateTimer - Updates All Timers
;==================================================================================

UpdateTimer
UpdateTimer5
    LDA TimerCounter5
    CMP #0
    BNE DidntReachLimitTimer5

ReachedLimitTimer5
    LDA #TIMER_LIMIT1
    STA TimerCounter5
    JMP UpdateTimer20

DidntReachLimitTimer5
    DEC TimerCounter5

UpdateTimer20
    LDA TimerCounter20
    CMP #0
    BNE DidntReachLimitTimer20

ReachedLimitTimer20
    LDA #TIMER_LIMIT2
    STA TimerCounter20
    JMP TimerRet

DidntReachLimitTimer20
    DEC TimerCounter20
    
TimerRet
    RTS

;==================================================================================
; UpdateScore
;==================================================================================

UpdateScore
    ; Check If Wafer Was Picked Up
    LDA PickedUpWaferBuffer
    CMP #0
    BEQ DidntPickUpWafer
    
    ; Picked Up Wafer - Increase Ones and Add to Wafer Count
    INC ScoreOnes
    INC WaferCount
    
    ; Check if Ones reached 10
    LDX ScoreOnes
    CPX #10
    BNE NotYet10

OnesReached10
    ; Reset Ones and Increase Tens
    LDX #0
    STX ScoreOnes
    INC ScoreTens

    ; Check if Tens reached 10
    LDX ScoreTens
    CPX #10
    BNE NotYet10

TensReached10
    ; Reset Tens
    LDX #0
    STX ScoreTens
    INC ScoreHundreds
    
    ; Check if Hundreds reached 10
    LDX ScoreHundreds
    CPX #10
    BNE NotYet10

HundredsReached10
    ; Reset Hundreds
    LDX #0
    STX ScoreHundreds

DidntPickUpWafer
NotYet10
    ; Flush PickedUpWaferBuffer
    LDA #0
    STA PickedUpWaferBuffer
    
    RTS

;==================================================================================
; HandleGameOver
;==================================================================================

HandleGameOver
    ; Check if Button is Pressed
    JSR GetControllerInputsButton
    
    ; TIM64T Loop
VBlankLoopGameOver
    LDA INTIM
    BNE VBlankLoopGameOver

    ; Show Screen
    JSR ShowGameOverScreen
    JSR GameOverOverscan
    JSR HandleVSync
    
    ; Initialize TIM64T
    LDA #42
    STA TIM64T

    ; Prepare Registers for VBLANK
    LDA #0
    STA VSYNC
    LDA #%01000010
    STA VBLANK

    JMP HandleGameOver

;==================================================================================
; ShowGameOverScreen
;==================================================================================

ShowGameOverScreen
    LDA #0                 
    STA VBLANK

    LDA #$0E
    STA COLUPF

    LDX #60
    STA WSYNC
GameOverLoop1
    STA WSYNC
    DEX
    BNE GameOverLoop1

    LDX #50
GameOverLoop2
    ; Display Game Over Message
    LDA GameOverMessage_STRIP_0,x
    STA PF0
    LDA GameOverMessage_STRIP_1,x
    STA PF1
    LDA GameOverMessage_STRIP_2,x
    STA PF2

    JSR Delay12
    
    LDA #0
    STA PF0
    STA PF1
    STA PF2

    STA WSYNC
    DEX
    BNE GameOverLoop2

    LDX #81
GameOverLoop3
    STA WSYNC
    DEX
    BNE GameOverLoop3

    RTS

;==================================================================================
; GameOverOverscan
;==================================================================================

GameOverOverscan
    
    LDX #30
GameOverOverscanLoop
    STA WSYNC
    DEX
    BNE GameOverOverscanLoop
    
    RTS

;==================================================================================
; Delay12 - Waste 12 cycles of CPU
;==================================================================================

Delay12
    RTS

;==================================================================================
; WaferXPosArray0
;==================================================================================

WaferXPosArray0
    .byte 25,35,60,100,130,140

;==================================================================================
; FindWaferLookUp - Look up table for finding wafer in memory
;==================================================================================

FindWaferLookUp
    .byte 0,%00000010,%00000100,%00001000,%00010000,%00100000,%01000000,%10000000

;==================================================================================
; MultBy14
;==================================================================================

MultBy14
    .byte -16,-2,12,26,40,54,68,82,96,110,124

;==================================================================================
; Lives Icons NUSIZ0 Look up table
;==================================================================================

LivesNUSIZ0LookUp
    .byte 0,0,%001,%011

;==================================================================================
; CheckLineBytes
;==================================================================================

CheckLineBytes
    .byte 0,0,0,0,0,0,0,0,0,0,0
    .byte 0,0,0,0,0,0,0,7,7,0,0
    .byte 0,0,0,0,0,0,0,0,0,0,0
    .byte 0,0,0,0,0,0,0,0,0,0,0
    .byte 6,6,0,0,0,0,0,0,0,0,0
    .byte 0,0,0,0,0,0,0,0,0,0,0
    .byte 0,0,0,0,5,5,0,0,0,0,0
    .byte 0,0,0,0,0,0,0,0,0,0,0
    .byte 0,0,0,0,0,0,4,4,0,0,0
    .byte 0,0,0,0,0,0,0,0,0,0,0
    .byte 0,0,0,0,0,0,0,0,3,3,0
    .byte 0,0,0,0,0,0,0,0,0,0,0
    .byte 0,0,0,0,0,0,0,0,0,0,2
    .byte 2,0,0,0,0,0,0,0,0,0,0
    .byte 0,0,0,0,0,0,0,0,0,0,0
    .byte 0,0,0,0,0,0,0,0,0,1,1
    .byte 0,0,0,0,0,0,0,0,0,0,0
    .byte 0,0,0,0,0,0,0,0,0,0,0

ConvertPlayerPosToWaferIndex
    .byte 7,7,7,7,7,7,7,7,7,7
    .byte 7,7,7,7,7,7,7,7,7,7
    .byte 7,7,7,7,7,7,7,7,7,7
    .byte 7,7,7,7,7,7,7,7,7,6
    .byte 6,6,6,6,6,6,6,6,6,6
    .byte 6,6,6,6,6,6,6,6,6,6
    .byte 6,6,6,6,6,6,6,6,6,5
    .byte 5,5,5,5,5,5,5,5,5,5
    .byte 5,5,5,5,5,5,5,5,5,5
    .byte 5,5,4,4,4,4,4,4,4,4
    .byte 4,4,4,4,4,4,4,4,4,4
    .byte 4,4,4,4,4,4,4,3,3,3
    .byte 3,3,3,3,3,3,3,3,3,3
    .byte 3,3,3,3,3,3,3,3,3,3
    .byte 2,2,2,2,2,2,2,2,2,2
    .byte 2,2,2,2,2,2,2,2,2,2
    .byte 2,2,2,2,2,2,1,1,1,1
    .byte 1,1,1,1,1,1,1,1,1,1
    .byte 1,1,1,1,1,1,1,1,1,1
    .byte 1,1,1,1

;===============================================================================
; free space check before page boundry
;===============================================================================
        
    echo "Start of Sprite0Data is: ", *
    align 256 

;==================================================================================
; Sprite Data - Pacman
;==================================================================================

Sprite0Data
    ; Frame 0
    .byte #%00000000
    .byte #%00000000
	.byte #%01111110
    .byte #%01111110
	.byte #%00111111
    .byte #%00111111
	.byte #%00001111
    .byte #%00001111
	.byte #%00000111
    .byte #%00000111
    .byte #%00001111
    .byte #%00001111
    .byte #%00111111
    .byte #%00111111
    .byte #%01111110
    .byte #%01111110
	.byte #%00000000
    .byte #%00000000

Sprite1Data
	; Frame 1
    .byte #%00000000
    .byte #%00000000
	.byte #%01111110
    .byte #%01111110
	.byte #%11111111
    .byte #%11111111
	.byte #%00111111
    .byte #%00111111
	.byte #%00001111
    .byte #%00001111
    .byte #%00111111
    .byte #%00111111
    .byte #%11111111
    .byte #%11111111
    .byte #%01111110
    .byte #%01111110
	.byte #%00000000
    .byte #%00000000

Sprite2Data
    ; Frame 2
    .byte #%00000000
    .byte #%00000000
	.byte #%01111110
    .byte #%01111110
	.byte #%11111111
    .byte #%11111111
	.byte #%11111111
    .byte #%11111111
	.byte #%00001111
    .byte #%00001111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%01111110
    .byte #%01111110
	.byte #%00000000
    .byte #%00000000 

Sprite3Data
    ; Frame 3
    .byte #%00000000
    .byte #%00000000
	.byte #%01111110
    .byte #%01111110
	.byte #%11111111
    .byte #%11111111
	.byte #%11111111
    .byte #%11111111
	.byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%01111110
    .byte #%01111110
	.byte #%00000000
    .byte #%00000000

Sprite4Data
    ; Frame 4
    .byte #%00000000
    .byte #%00000000
	.byte #%00011110
    .byte #%00011110
	.byte #%00011111
    .byte #%00011111
	.byte #%00001111
    .byte #%00001111
	.byte #%00000111
    .byte #%00000111
    .byte #%00001111
    .byte #%00001111
    .byte #%00011111
    .byte #%00011111
    .byte #%00011110
    .byte #%00011110
	.byte #%00000000
    .byte #%00000000

Sprite5Data
    ; Frame 5
    .byte #%00000000
    .byte #%00000000
	.byte #%00000010
    .byte #%00000010
	.byte #%00000111
    .byte #%00000111
	.byte #%00000111
    .byte #%00000111
	.byte #%00000111
    .byte #%00000111
    .byte #%00000111
    .byte #%00000111
    .byte #%00000111
    .byte #%00000111
    .byte #%00000010
    .byte #%00000010
	.byte #%00000000
    .byte #%00000000

Sprite6Data
    ; Frame 6
    .byte #%00000000
    .byte #%00000000
	.byte #%00000000
    .byte #%00000000
	.byte #%00000000
    .byte #%00000011
	.byte #%00000011
    .byte #%00000111
	.byte #%00000111
    .byte #%00000111
    .byte #%00000111
    .byte #%00000011
    .byte #%00000011
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
	.byte #%00000000
    .byte #%00000000

Sprite7Data
    ; Frame 7
    .byte #%00000000
    .byte #%00000000
	.byte #%00000000
    .byte #%00000000
	.byte #%00000000
    .byte #%00000000
	.byte #%00000000
    .byte #%00000111
	.byte #%00000111
    .byte #%00000111
    .byte #%00000111
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
	.byte #%00000000
    .byte #%00000000

Sprite8Data
    ; Frame 8
    .byte #%00000000
    .byte #%00000000
	.byte #%10000001
    .byte #%10000001
	.byte #%01000100
    .byte #%01000100
    .byte #%00111000
    .byte #%00111000
    .byte #%01000110
    .byte #%01000110
    .byte #%10000001
    .byte #%10000001
    .byte #%00000000
    .byte #%00000000
	.byte #%00000000
    .byte #%00000000

Sprite9Data
    ; Frame 9
    .byte #%00000000
    .byte #%00000000
	.byte #%00010000
    .byte #%00010000
	.byte #%10000001
    .byte #%10000001
    .byte #%00000000
    .byte #%00000000
    .byte #%10000000
    .byte #%10000000
    .byte #%00000001
    .byte #%00000001
    .byte #%00000000
    .byte #%00000000
	.byte #%00000000
    .byte #%00000000

Sprite10Data
    ; Frame 10
    .byte #%00000000
    .byte #%00000000
	.byte #%00000000
    .byte #%00000000
	.byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
	.byte #%00000000
    .byte #%00000000

;==================================================================================
; Sprite Data - Ghost
;==================================================================================

SpriteGhostData
    .byte #%00000000
    .byte #%00000000
    .byte #%01010101
    .byte #%01010101
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%10011001
    .byte #%10011001
    .byte #%10111011
    .byte #%10111011
    .byte #%11111111
    .byte #%01111110
    .byte #%01111110
    .byte #%01111110
    .byte #%01111110
    .byte #%00000000
    .byte #%00000000

;==================================================================================
; Sprite Data - Lives
;==================================================================================

SpriteLifeData
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00111100
	.byte #%01111110
	.byte #%11111111
    .byte #%11111111
	.byte #%00111111
	.byte #%00001111
    .byte #%00111111
    .byte #%11111111
    .byte #%11111111
    .byte #%01111110
    .byte #%00111100
	.byte #%00000000

;==================================================================================
; free space check before page boundry
;==================================================================================
        
    echo "End of Sprite0Data is: ", *
    align 256 

;==================================================================================
; Board Data
;==================================================================================
    
    include "MainBoard.asm"
    include "GameOverMessage.asm"

;===============================================================================
; free space check before page boundry
;===============================================================================
        
    echo "End of MainBoard.asm is: ", *
    align 256 

;==================================================================================
; Bottom Data
;==================================================================================

BottomData

    ; 0
    .byte %00000000
    .byte %01111110
    .byte %01111110
    .byte %11100111
    .byte %11100111
    .byte %11000011
    .byte %11000011
    .byte %11000011
    .byte %11000011
    .byte %11000011
    .byte %11100111
    .byte %11100111
    .byte %01111110
    .byte %01111110
    
    ; 1
    .byte %00000000
    .byte %11111111
    .byte %11111111
    .byte %00011100
    .byte %00011100
    .byte %00011100
    .byte %00011100
    .byte %00011100
    .byte %00011100
    .byte %00011100
    .byte %11111100
    .byte %01111100
    .byte %00111100
    .byte %00011100
    
    ; 2
    .byte %00000000
    .byte %11111111
    .byte %11111111
    .byte %11100000
    .byte %11100000
    .byte %00111000
    .byte %00001110
    .byte %00000111
    .byte %00000011
    .byte %11000011
    .byte %11000011
    .byte %11100111
    .byte %01111110
    .byte %00111100
    
    ; 3
    .byte %00000000
    .byte %00111100
    .byte %01111110
    .byte %11101111
    .byte %11000111
    .byte %00000111
    .byte %00001110
    .byte %00011100
    .byte %00001110
    .byte %00000111
    .byte %11000111
    .byte %11101111
    .byte %01111110
    .byte %00111100
    
    ; 4
    .byte %00000000
    .byte %00000110
    .byte %00000110
    .byte %11111111
    .byte %11111111
    .byte %11000110
    .byte %11000110
    .byte %01100110
    .byte %01100110
    .byte %00110110
    .byte %00110110
    .byte %00011110
    .byte %00001110
    .byte %00001110
    
    ; 5
    .byte %00000000
    .byte %11111100
    .byte %11111110
    .byte %00001111
    .byte %00000111
    .byte %00000111
    .byte %00001111
    .byte %11111110
    .byte %11111100
    .byte %11100000
    .byte %11100000
    .byte %11100000
    .byte %11111111
    .byte %11111111
    
    ; 6
    .byte %00000000
    .byte %00111100
    .byte %01111110
    .byte %11100111
    .byte %11000011
    .byte %11000011
    .byte %11100111
    .byte %11111110
    .byte %11111100
    .byte %11000000
    .byte %11000011
    .byte %11100111
    .byte %11111111
    .byte %01111110
    
    ; 7
    .byte %00000000
    .byte %00011000
    .byte %00011000
    .byte %00011000
    .byte %00011000
    .byte %00011000
    .byte %00001100
    .byte %00001100
    .byte %00001100
    .byte %11000110
    .byte %11000111
    .byte %11111111
    .byte %11111111
    .byte %11111111
    
    ; 8
    .byte %00000000
    .byte %00111100
    .byte %01111110
    .byte %11100111
    .byte %11000011
    .byte %11000011
    .byte %11100111
    .byte %01111110
    .byte %00111100
    .byte %01100110
    .byte %11000011
    .byte %11100111
    .byte %01111110
    .byte %00111100

    ; 9
    .byte %00000000
    .byte %00111100
    .byte %01111110
    .byte %11100111
    .byte %11000011
    .byte %00000011
    .byte %00111111
    .byte %01111111
    .byte %11100111
    .byte %11000011
    .byte %11000011
    .byte %11100111
    .byte %01111110
    .byte %00111100

;==================================================================================
; Interrupt Routines
;==================================================================================

    SEG Interrupt
    ORG $FFFA

InterruptVectors
    .word Reset
    .word Reset
    .word Reset

    END