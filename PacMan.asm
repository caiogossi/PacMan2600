    processor 6502
    include "vcs.h"
    include "macro.h"

;==================================================================================
; Program Definitions
;==================================================================================
    
SPRITE_HEIGHT = 22
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

SpriteAnimationIndex ds 1
IsFrameGoingUp ds 1

ScoreTens ds 1
ScoreOnes ds 1
TensOffset ds 1
OnesOffset ds 1
ScoreDisplayTemp ds 1

TimerCounter ds 1
PlayerReflectedBuffer ds 1

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
    JSR UpdateStuff
    
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

    ; Check Vertical Drawing
    TXA                     ; 2
    SEC                     ; 2
    SBC SpriteYPos          ; 3
	ADC #SPRITE_HEIGHT      ; 2
    
    ; Load Second Playfield
    LDY MainBoard_STRIP_3,x ; 4  25
    STY PF0                 ; 3  28

    LDY MainBoard_STRIP_4,x ; 4  32
    STY PF1
    
    LDY MainBoard_STRIP_5,x ; 4  39
    STY PF2                 ; 3  42

    ; Load P0 Sprite Data
    BCC SkipDrawing         ; 3
    TAY                     ; 3
    LDA (SpriteAddrPtr),y   ; 5
    STA GRP0                ; 3

SkipDrawing
    ; Decrease X and Go To Next Line
    DEX                     ; 2
    STA WSYNC               ; 3
    CPX #0
    BEQ MainFrameLoopEnd    ; 2 
    
    ; Second Line

    ; Load First Playfield
    LDA MainBoard_STRIP_0,x ; 4
    STA PF0                 ; 3

    LDA MainBoard_STRIP_1,x ; 4
    STA PF1                 ; 3

    LDA MainBoard_STRIP_2,x ; 4
    STA PF2                 ; 3
    
    ; Load Second Player
    LDA #0
    STA GRP1
    NOP
    NOP
    
    ; Load Second Playfield
    LDA MainBoard_STRIP_3,x ; 4
    STA PF0                 ; 3

    LDA MainBoard_STRIP_4,x ; 4
    STA PF1                 ; 3

    LDA MainBoard_STRIP_5,x ; 4
    STA PF2                 ; 3
    
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
    STA PF1
    STA REFP0

    LDA #$0E
    STA COLUP0
    STA COLUP1

    ; Calculate Tens Offset
    LDX ScoreTens
    LDA MultBy20,x
    STA TensOffset
    
    ; Calculate Ones Offset
    LDX ScoreOnes
    LDA MultBy20,x
    STA OnesOffset

    ; Adjust Score Placement
    STA WSYNC
    JSR Delay12
    JSR Delay12
    STA RESP0
    STA RESP1

    LDX #28
OverscanLoop
    
    ; Is it time to draw?
    CPX #10
    BCC SmallerThan9
    
    ; Time to draw
    TXA
    SBC #10
    ADC TensOffset
    TAY
    LDA BottomData,y
    STA GRP0
    
    TXA
    SBC #9
    ADC OnesOffset
    TAY
    LDA BottomData,y
    STA GRP1
    
SmallerThan9    
    STA WSYNC
    DEX
    BNE OverscanLoop

    ; Clear Player Registers
    LDA #0
    STA GRP0
    STA GRP1

    ; Return P0 Register Reflection Value
    LDA PlayerReflectedBuffer
    STA REFP0

    ; Return P0 and P1 colors
    LDA #$1E
    STA COLUP0
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

    ; Initialize Variables
    
    ; SpriteXPos
    LDA #125
    STA SpriteXPos
    
    ; SpriteYPos
    LDA #105
    STA SpriteYPos

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
    LDA #1
    STA VDELP0

    ; SpriteDataPointer
    LDA #<Sprite0Data
    STA SpriteAddrPtr
    LDA #>Sprite0Data
    STA SpriteAddrPtr+1

    RTS

;==================================================================================
; PositionSpriteX - Subroutine to position sprite
;==================================================================================

PositionSpriteX
    STA WSYNC                                   ; 3
    STA HMCLR  ; clear any previous movement    ; 3

    LDX #1     ; sprite index                   ; 2

PosSP   

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

    STA HMP0-1,x    ; fine movement             ; 4
    STA WSYNC                                   ; 3

    JSR Ret         ; just a 12 cycle delay     ; 12
    BIT 0           ; 15 cycles = 3 loops :)    ; 3


Jiggle  
    dey                                         ; 2
    bpl Jiggle                                  ; 3

    sta RESP0-1,x                               ; 4

    dex                                         ; 2
    bne PosSP                                   ; 3

    sta WSYNC                                   ; 3
    sta HMOVE                                   ; 3
    sta WSYNC                                   ; 3
    
Ret
    RTS                                         ; 6

;==================================================================================
; UpdateStuff
;==================================================================================

UpdateStuff
    ; Update Sprite X Pos
    ;INC SpriteXPos

    ; Update Sprite Y Pos
    ;INC SpriteYPos

    ; Update Timer
    JSR UpdateTimer
    
    ; Increase Score
    JSR UpdateScore

    ; Change Animation Frame
    JSR ChangeAnimationFrame

    ; Apply Animation Frame
    JSR ApplyAnimationFrame

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
; GetControllerInputs
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
    INC SpriteXPos
    LDA #%1000
    STA REFP0
    STA PlayerReflectedBuffer
    JMP ControllerRet

LeftInput
    DEC SpriteXPos
    LDA #0
    STA REFP0
    STA PlayerReflectedBuffer
    JMP ControllerRet

DownInput
    DEC SpriteYPos
    JMP ControllerRet

UpInput
    INC SpriteYPos

ControllerRet
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
    ; Check If Timer == 0
    LDA TimerCounter
    CMP #0
    BNE NotYet10
    
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

NotYet10
    RTS

;==================================================================================
; Delay12 - Waste 12 cycles of CPU
;==================================================================================

Delay12
    RTS

;==================================================================================
; MultBy20
;==================================================================================

MultBy20
    .byte 0,20,40,60,80,100,120,140,160,180,200

;==================================================================================
; Control Page Boundry
;==================================================================================

    .byte 0,1,2,3,4,5,6,7,8,9,10
    .byte 0,1

;===============================================================================
; free space check before page boundry
;===============================================================================
        
    echo "Start of Sprite0Data is: ", *
    align 256 

;==================================================================================
; Sprite Data
;==================================================================================

Sprite0Data
; Frame 0
    .byte #%00000000
    .byte #%00000000
	.byte #%00011000
    .byte #%00011000
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
    .byte #%00011000
    .byte #%00011000
	.byte #%00000000
    .byte #%00000000

Sprite1Data
	; Frame 0
    .byte #%00000000
    .byte #%00000000
	.byte #%00011000
    .byte #%00011000
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
    .byte #%00011000
    .byte #%00011000
	.byte #%00000000
    .byte #%00000000

Sprite2Data
    ; Frame 1
    .byte #%00000000
    .byte #%00000000
	.byte #%00011000
    .byte #%00011000
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
    .byte #%00011000
    .byte #%00011000
	.byte #%00000000
    .byte #%00000000 

Sprite3Data
    ; Frame 2
    .byte #%00000000
    .byte #%00000000
	.byte #%00011000
    .byte #%00011000
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
    .byte #%00011000
    .byte #%00011000
	.byte #%00000000
    .byte #%00000000 

;===============================================================================
; free space check before page boundry
;===============================================================================
        
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
    .byte %00000000
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
    .byte %11000011
    .byte %11000011
    .byte %11000011
    .byte %11100111
    .byte %11100111
    .byte %01111110
    .byte %01111110
    .byte %00000000
    
    ; 1
    .byte %00000000
    .byte %00000000
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
    .byte %00011100
    .byte %00011100
    .byte %00011100
    .byte %11111100
    .byte %01111100
    .byte %00111100
    .byte %00011100
    .byte %00000000
    
    ; 2
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %11111111
    .byte %11111111
    .byte %11100000
    .byte %11100000
    .byte %01110000
    .byte %00111000
    .byte %00011100
    .byte %00001110
    .byte %00000111
    .byte %00000011
    .byte %00000011
    .byte %11000011
    .byte %11000011
    .byte %11100111
    .byte %01111110
    .byte %00111100
    .byte %00000000
    
    ; 3
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00111100
    .byte %01111110
    .byte %11101111
    .byte %11000111
    .byte %00000111
    .byte %00000111
    .byte %00001110
    .byte %00011100
    .byte %00011100
    .byte %00001110
    .byte %00000111
    .byte %00000111
    .byte %11000111
    .byte %11101111
    .byte %01111110
    .byte %00111100
    .byte %00000000
    
    ; 4
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000110
    .byte %00000110
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
    .byte %00011110
    .byte %00001110
    .byte %00001110
    .byte %00000000
    
    ; 5
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %11111000
    .byte %11111100
    .byte %11111110
    .byte %00001111
    .byte %00000111
    .byte %00000111
    .byte %00000111
    .byte %00001111
    .byte %11111110
    .byte %11111100
    .byte %11100000
    .byte %11100000
    .byte %11100000
    .byte %11100000
    .byte %11111111
    .byte %11111111
    .byte %00000000
    
    ; 6
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00111100
    .byte %01111110
    .byte %11100111
    .byte %11000011
    .byte %11000011
    .byte %11000011
    .byte %11100111
    .byte %11111110
    .byte %11111100
    .byte %11000000
    .byte %11000000
    .byte %11000000
    .byte %11000011
    .byte %11100111
    .byte %01111111
    .byte %00111110
    .byte %00000000
    
    ; 7
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00011000
    .byte %00011000
    .byte %00011000
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
    .byte %00000000
    
    ; 8
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00111100
    .byte %01111110
    .byte %11100111
    .byte %11000011
    .byte %11000011
    .byte %11000011
    .byte %11100111
    .byte %01111110
    .byte %00111100
    .byte %01100110
    .byte %11000011
    .byte %11000011
    .byte %11000011
    .byte %11100111
    .byte %01111110
    .byte %00111100
    .byte %00000000

    ; 9
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00111100
    .byte %01111110
    .byte %11100111
    .byte %11000011
    .byte %00000011
    .byte %00000011
    .byte %00000011
    .byte %00111111
    .byte %01111111
    .byte %11100111
    .byte %11000011
    .byte %11000011
    .byte %11000011
    .byte %11100111
    .byte %11111110
    .byte %01111100
    .byte %00000000
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