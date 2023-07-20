    processor 6502
    include "vcs.h"
    include "macro.h"

;==================================================================================
; Program Definitions
;==================================================================================
    
SPRITE_HEIGHT = 11
TIMER_LIMIT = 20

;==================================================================================
; Program Variables
;==================================================================================

    SEG.U Variables
    ORG $80

PS_temp ds 1

SpriteAddrPtr ds 2
SpriteXPos ds 1
SpriteYPos ds 1

ScoreTens ds 1
ScoreOnes ds 1
TensOffset ds 1
OnesOffset ds 1
ScoreDisplayTemp ds 1

TimerCounter ds 1

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
    LDA #0
    STA VSYNC
    LDA #%01000010
    STA VBLANK

    JSR GetControllerInputs
    JSR UpdateStuff
    
    LDX #37
VBlankLoop
    STA WSYNC
    DEX
    BNE VBlankLoop
    
    RTS

;==================================================================================
; MainKernel
;==================================================================================
    
MainKernel
    LDA #0
    STA VBLANK
    
    JSR PositionSpriteX

    LDX #191

MainFrameLoop
    ; Load First Playfield
    LDA MainBoard_STRIP_0,x
    STA PF0

    LDA MainBoard_STRIP_1,x
    STA PF1

    LDA MainBoard_STRIP_2,x
    STA PF2
    
    ; Load Second Playfield
    LDA MainBoard_STRIP_3,x
    STA PF0

    LDA MainBoard_STRIP_4,x
    STA PF1

    LDA MainBoard_STRIP_5,x
    STA PF2
    
    ; Load Sprite Shape
    TXA                   
 	SEC               
    SBC SpriteYPos         
	ADC #SPRITE_HEIGHT 
	BCC .skipMBDraw3    
	TAY
	LDA (SpriteAddrPtr),y 
	STA GRP0

.skipMBDraw3 	
    
    STA WSYNC
    DEX
	BNE MainFrameLoop

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
    
    LDA #0
    STA PF1

    ; Calculate Tens Offset
    LDX ScoreTens
    LDA MultBy20,x
    STA TensOffset
    
    ; Calculate Ones Offset
    LDX ScoreOnes
    LDA MultBy20,x
    STA OnesOffset

    LDX #29
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
    
    STA RESP0
    STA RESP1
    
SmallerThan9    
    STA WSYNC
    DEX
    BNE OverscanLoop

    ; Clear Player Registers
    LDA #0
    STA GRP0
    STA GRP1
    RTS

;==================================================================================
; InitVariables
;==================================================================================

InitVariables
    ; Initialize BG Color and PF Color
    LDA #$84
    STA COLUBK
    LDA #$1C
    STA COLUPF

    ; Initialize Player Color and Pattern
    LDA #$56
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
    LDA #7
    STA ScoreTens
    STA ScoreOnes

    ; Set Controller Inputs
    LDA #0
    STA SWACNT

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
    STA WSYNC
    STA HMCLR  ; clear any previous movement

    LDX #1     ; sprite index

PosSP   

    LDA SpriteXPos-1,x
    TAY

    ; Divide by 16
    LSR
    LSR
    LSR
    LSR    
    STA PS_temp

    TYA
    AND #15

    CLC

    ADC PS_temp
    LDY PS_temp

    CMP #15
    BCC NH
    SBC #15
    INY

NH
    ; Use remainder for fine adjustment
    EOR #7
    ASL
    ASL
    ASL
    ASL

    STA HMP0-1,x                     ; fine movement
    STA WSYNC

    JSR Ret                       ; just a 12 cycle delay
    BIT 0                         ; 15 cycles = 3 loops :)


Jiggle  
    dey
    bpl Jiggle

    sta RESP0-1,x

    dex
    bne PosSP

    sta WSYNC
    sta HMOVE
    sta WSYNC
    
Ret
    RTS

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
    JMP ControllerRet

LeftInput
    DEC SpriteXPos
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
    RTS
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
; MultBy20
;==================================================================================

MultBy20
    .byte 0,20,40,60,80,100,120,140,160,180,200

;==================================================================================
; Sprite Data
;==================================================================================

Sprite0Data
	.byte #%00000000
	.byte #%00011000
	.byte #%01111110
	.byte #%11111111
	.byte #%00111111
	.byte #%00001111
    .byte #%00111111
    .byte #%11111111
    .byte #%01111110
    .byte #%00011000
	.byte #%00000000

;==================================================================================
; Board Data
;==================================================================================
    
    include "MainBoard.asm"

;==================================================================================
; Bottom Data
;==================================================================================

BottomData

    ; 0
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %01111110
    .byte %11111111
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
    .byte %11111111
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
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000

    ; 9
    .byte %00000000
    .byte %00000000
    .byte %11101110
    .byte %00100010
    .byte %00100010
    .byte %00100010
    .byte %00100010
    .byte %00100010
    .byte %00100010
    .byte %11101110
    .byte %11101110
    .byte %10101010
    .byte %10101010
    .byte %10101010
    .byte %10101010
    .byte %10101010
    .byte %10101010
    .byte %11101110
    .byte %11101110
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