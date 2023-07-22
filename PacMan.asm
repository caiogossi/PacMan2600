    processor 6502
    include "vcs.h"
    include "macro.h"

;==================================================================================
; Program Definitions
;==================================================================================
    
SPRITE_HEIGHT = 18
TIMER_LIMIT = 5

;==================================================================================
; Program Variables
;==================================================================================

    SEG.U Variables
    ORG $80

PS_temp ds 1

SpriteAddrPtr ds 2
SpriteXPos ds 1
SpriteYPos ds 1
LastSpriteXPos ds 1
LastSpriteYPos ds 1
PlayerVelocityMask ds 1

SpriteGhostAddrPtr ds 2
GhostSpriteXPos ds 1
GhostSpriteYPos ds 1

CurWaferXPos0 ds 1
WaferXPosIndex ds 1
WaferArray ds 6
PickedUpWaferBuffer ds 1

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

TimerCounter ds 1

PlayerReflectedBuffer ds 1
GhostReflectedBuffer ds 1

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
    JSR GetControllerInputs
    JSR GetController2InputsDEBUG
    JSR UpdateEntities
    
VBlankLoop
    LDA INTIM
    BNE VBlankLoop

    STA WSYNC
    RTS

;==================================================================================
; MainKernel
;==================================================================================
    
MainKernel
    LDA #0                 
    STA VBLANK              
    
    ; Position Player and Ghost According to XPos
    JSR PositionSpriteX
    JSR PositionGhostSpriteX
    
    ; Position Wafer in XPos
    JSR PositionWaferX0

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

DoDrawWafer
    ; Verify if water is picked up
    LDY WaferXPosIndex
    LDA WaferArray,y
    
    LDY CheckLineBytes,x
    AND FindWaferLookUp,y
    BNE DoNotDrawWafer
    
    ; Draw wafer
    LDA #%10
    STA ENAM0
    JMP AfterDrawing

DoNotDrawWafer
    LDA #0
    STA ENAM0 
       
AfterDrawing
    ; Check Vertical Sprite (P0) Drawing
    TXA                     ; 2
    SEC                     ; 2
    SBC SpriteYPos          ; 3
	ADC #SPRITE_HEIGHT      ; 2

    ; Load P0 Sprite Data
    BCC SkipDrawingP0       ; 3
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

    ; Check Collision Between Player and Wafer
    BIT CXM0P
    BVC DidntGetWafer

GotWafer
    ; Routine to save wafer as picked up
    LDY WaferXPosIndex
    LDA WaferArray,y
    
    LDY CheckLineBytes,x
    
    ; Mark Wafer Picked Up for Frame
    ORA FindWaferLookUp,y

    LDY WaferXPosIndex
    STA WaferArray,y

    ; Reset Collision Detection
    STA PickedUpWaferBuffer
    STA CXCLR

DidntGetWafer
    ; Check Ghost Vertical Drawing
    TXA                     ; 2
    SEC                     ; 2
    SBC GhostSpriteYPos     ; 3
	ADC #SPRITE_HEIGHT      ; 2

    ; Load Ghost Sprite Data
    BCC SkipGhostDrawing    ; 3
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
    STA COLUP0
    STA COLUP1

    LDA #%011
    STA NUSIZ0

    STA RESP0
    
    ; Calculate Tens Offset
    LDX ScoreTens
    LDA MultBy14,x
    STA TensOffset
    
    ; Calculate Ones Offset
    LDX ScoreOnes
    LDA MultBy14,x
    STA OnesOffset
    
    ;STA RESP1

    LDX #29
OverscanLoop
    
    ; Is it time to draw?
    CPX #15
    BCC SmallerThan17
    
BiggerThan17
    ; Time to draw
    TXA
    SBC #15
    CLC
    ADC OnesOffset
    TAY
    LDA BottomData,y
    STA ScoreDisplayBufferOnes
    
    TXA
    SBC #15
    CLC
    ADC TensOffset
    TAY
    LDA BottomData,y
    STA ScoreDisplayBufferTens  
    
    STA GRP0
    LDA ScoreDisplayBufferOnes
    STA GRP0
    
SmallerThan17    
    STA WSYNC
    DEX
    BNE OverscanLoop

    ; Clear Player Registers
    LDA #0
    STA GRP0
    STA GRP1

    ; Return NUSIZ0
    LDA #%100000
    STA NUSIZ0

    ; Return P0 Register Reflection Value
    LDA PlayerReflectedBuffer
    STA REFP0

    ; Return P1 Register Reflection Value
    LDA GhostReflectedBuffer
    STA REFP1

    ; Return P0 and P1 colors
    LDA #$1E
    STA COLUP0
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
    
    ; SpriteXPos and YPos
    LDA #80
    STA SpriteXPos
    LDA #107
    STA SpriteYPos

    ; SpriteGhostXPos and YPos
    LDA #77
    STA GhostSpriteXPos
    LDA #130
    STA GhostSpriteYPos

    ; Timer Counter
    LDA #0
    STA TimerCounter
    
    ; Score
    STA ScoreTens
    STA ScoreOnes

    ; Set Controller Inputs
    STA SWACNT

    ; Set SpriteAnimationIndex
    STA SpriteAnimationIndex
    STA IsFrameGoingUp

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
; PositionSpriteX - Subroutine to position player sprite
;==================================================================================

PositionSpriteX
    STA WSYNC                                   ; 3
    STA HMCLR  ; clear any previous movement    ; 3

PosSP   

    LDA SpriteXPos                          ; 4
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
    BCC NH                                      ; 3
    SBC #15                                     ; 2
    INY                                         ; 2

NH
    ; Use remainder for fine adjustment
    EOR #7                                      ; 2
    ASL                                         ; 2
    ASL                                         ; 2
    ASL                                         ; 2
    ASL                                         ; 2

    STA HMP0    ; fine movement             ; 4
    STA WSYNC                                   ; 3

    JSR Ret         ; just a 12 cycle delay     ; 12
    BIT 0           ; 15 cycles = 3 loops :)    ; 3


Jiggle  
    DEY                                         ; 2
    BPL Jiggle                                  ; 3

    STA RESP0                               ; 4

    STA WSYNC                                   ; 3
    STA HMOVE                                   ; 3
    
Ret
    RTS                                         ; 6

;==================================================================================
; PositionGhostSpriteX - Subroutine to position ghost sprite
;==================================================================================

PositionGhostSpriteX
    STA WSYNC                                   ; 3
    STA HMCLR  ; clear any previous movement    ; 3

    LDX #1     ; sprite index                   ; 2

PosSP1   

    LDA GhostSpriteXPos-1,x                          ; 4
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

    STA HMP1-1,x    ; fine movement             ; 4
    STA WSYNC                                   ; 3

    JSR Ret1         ; just a 12 cycle delay     ; 12
    BIT 0           ; 15 cycles = 3 loops :)    ; 3


Jiggle1  
    DEY                                         ; 2
    BPL Jiggle1                                  ; 3

    STA RESP1-1,x                               ; 4

    DEX                                         ; 2
    BNE PosSP1                                   ; 3

    STA WSYNC                                   ; 3
    STA HMOVE                                   ; 3
    
Ret1
    RTS                                         ; 6

;==================================================================================
; PositionWaferX0 - Subroutine to position wafer missile
;==================================================================================

PositionWaferX0
    STA WSYNC                                   ; 3
    STA HMCLR  ; clear any previous movement    ; 3

PosSPWafer0 

    LDA CurWaferXPos0                            ; 4
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
    BCC NHWafer0                                      ; 3
    SBC #15                                     ; 2
    INY                                         ; 2

NHWafer0
    ; Use remainder for fine adjustment
    EOR #7                                      ; 2
    ASL                                         ; 2
    ASL                                         ; 2
    ASL                                         ; 2
    ASL                                         ; 2

    STA HMM0        ; fine movement             ; 4
    STA WSYNC                                   ; 3

    JSR RetWafer0         ; just a 12 cycle delay     ; 12
    BIT 0           ; 15 cycles = 3 loops :)    ; 3


JiggleWafer0  
    DEY                                         ; 2
    BPL JiggleWafer0                                 ; 3

    STA RESM0                               ; 4

    STA WSYNC                                   ; 3
    STA HMOVE                                   ; 3
    
RetWafer0
    RTS                                         ; 6

;==================================================================================
; UpdateEntities - Update Game Logic
;==================================================================================

UpdateEntities
    ; Check Player Collision
    JSR CheckPlayerCollision

    ; Update Sprite Pos According to Velocity Mask
    JSR UpdatePlayerPosition

    ; Remove Collisions After Checks
    STA CXCLR

    ; Update Timer
    JSR UpdateTimer
    
    ; Increase Score According with Picked Up Wafer Buffer
    JSR UpdateScore

    ; Change Animation Frame
    JSR ChangeAnimationFrame

    ; Apply Animation Frame
    JSR ApplyAnimationFrame

    ; Update Wafer XPos Index
    JSR UpdateWaferIndex

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
; CheckPlayerCollision - Verify for Player Collision with PF and Update Velocity Mask
;==================================================================================

CheckPlayerCollision
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
    ; Verify Counter
    LDA TimerCounter
    BNE DontChangeFrame
    
    ; Load Current Frame Index
    LDY SpriteAnimationIndex
    
    ; Verify Next Frame
    LDX IsFrameGoingUp
    BEQ FrameIsGoingUp

FrameIsGoingDown
    DEY
    JMP FrameChanged

FrameIsGoingUp
    INY

FrameChanged
    ; Check if Index is Either 0 or 2
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
    
DontRestartFrame
DontChangeFrame
FrameChangeReturn
    RTS

;==================================================================================
; ApplyAnimationFrame
;==================================================================================

ApplyAnimationFrame
    ; Check Counter
    LDA TimerCounter
    BNE ApplyAnimationFrameRet
    
    LDA SpriteAnimationIndex
    BEQ Animation0
    CMP #1
    BEQ Animation1
    CMP #2
    BEQ Animation2
    CMP #3
    BEQ Animation3
    
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

ApplyAnimationFrameRet
    RTS

;==================================================================================
; GetControllerInputs - Update Player Velocity Mask According to Inputs
;==================================================================================

GetControllerInputs
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
; UpdateTimer
;==================================================================================

UpdateTimer
    LDA TimerCounter
    CMP #0
    BNE DidntReachLimit

ReachedLimit
    LDA #TIMER_LIMIT
    STA TimerCounter
    JMP TimerRet

DidntReachLimit
    DEC TimerCounter
    
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
    
    ; Increase Ones
    INC ScoreOnes
    
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

DidntPickUpWafer
NotYet10
    ; Flush PickedUpWaferBuffer
    LDA #0
    STA PickedUpWaferBuffer
    
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
    .byte 0,14,28,42,56,70,84,98,112,126,140

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
    .byte 0,0,0,0,0,0,0,0,0,0,0
    .byte 0,0,0,0,0,0,0,0,0,0,0

;==================================================================================
; Control Page Boundry
;==================================================================================


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
	; Frame 0
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
    ; Frame 1
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
    ; Frame 2
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
; free space check before page boundry
;==================================================================================
        
    echo "End of Sprite0Data is: ", *
    align 256 

;==================================================================================
; Board Data
;==================================================================================
    
    include "MainBoard.asm"

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
    .byte %00000000

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