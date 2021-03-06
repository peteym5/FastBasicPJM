        .segment "ALIGNDATA"
CHARSET_GAME:
  .incbin         "BUGINVDE.FNT"
  
PMBANK                                  =$B000                                        
VARBANK                                 =$B800                                                                                                                                       

SCREEN_ADDR                             =$BA00                                         
GAME_SCREEN_ADDR                        =$BA50                                        

RTCLOCK                                 =$12
PMBANK_HI                               = >PMBANK   

M0		        	          	            =$C0
M1		        	          	            =$C1
M2		        	          	            =$C2
M3		        	          	            =$C3
M4		        	          	            =$C4
M5		        	          	            =$C5
M6		        	          	            =$C6
M7		        	          	            =$C7
M8		        	          	            =$C8
M9		        	          	            =$C9
NDX0									                  =$CA
NDX1									                  =$CB
NDX2									                  =$CC
NDX3									                  =$CD
                        
PMSAVELO = PMBANK + $02E0
PMSAVEHI = PMBANK + $02E8
PMSAVESZ = PMBANK + $02F0
PMNUMBER = M9
FROMLO = NDX0
FROMHI = NDX1
PMYPOS = NDX2
PAGEBASE = NDX0
PAGEBSHI = NDX1
                    

SPRITENUM	                                        =PMBANK+$0180
SETSP0COLOR                                       =PMBANK+$0190
SETSP1COLOR                                       =PMBANK+$01A0
SETSPWIDTH                                        =PMBANK+$01B0
SPRITENHOZ                                        =PMBANK+$01C0
SPRITENVRT                                        =PMBANK+$01E0
SPHOZNEXT                                         =PMBANK+$01F0

SPRHZ0	                            	            =PMBANK+$0200 
SPRHZ1	                            	            =PMBANK+$0210 
SPRHZ2	                            	            =PMBANK+$0220 
SPRHZ3	                            	            =PMBANK+$0230 
SPZONT                                            =PMBANK+$0240
SPZONB                                            =PMBANK+$0250
SPSRC0                                            =PMBANK+$0260 
SPSRC1                                            =PMBANK+$0278
SPSRC2                                            =PMBANK+$0290
SPSRC3                                            =PMBANK+$02A8
SPSRC4                                            =PMBANK+$02C0
SPRITEUSE                                         =PMBANK+$02D8
                                            
MIBANK                                            =PMBANK+$0300
PMBNK0                                            =PMBANK+$0400
PMBNK1                             	              =PMBANK+$0500
PMBNK2                                            =PMBANK+$0600
PMBNK3                             	              =PMBANK+$0700
            
shooter_px                                        =VARBANK + $020
shooter_py                                        =VARBANK + $021
shooter_status                                    =VARBANK + $022
prior_x                                           =VARBANK + $024
prior_y                                           =VARBANK + $024
stick_read                                        =VARBANK + $026 
; 
SOUND_COMMANDER_VARIABLE_AREA                     =VARBANK + $060;

DISPLAY_LIST_TITLE:
     .byte $70,$F0,$44
     .byte <SCREEN_ADDR
     .byte >SCREEN_ADDR
     .byte $04,$05,$00,$04,$00,$04,$00,$04,$00,$04,$00,$04,$00,$04,$00
     .byte $04,$00,$04,$00,$04,$00,$04,$00,$04,$00,$04,$00,$04,$00,$04,$00
     .byte $04,$00,$04,$00,$04,$00,$04,$00,$04,$00,$04,$00,$04,$00,$04,$41
     .byte <DISPLAY_LIST_TITLE 
     .byte >DISPLAY_LIST_TITLE
    
DISPLAY_LIST_GAME:
     .byte $70,$50,$C4
     .byte <SCREEN_ADDR
     .byte >SCREEN_ADDR
     .byte $10,$44
     .byte <GAME_SCREEN_ADDR
     .byte >GAME_SCREEN_ADDR
     .byte $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$41
     .byte <DISPLAY_LIST_GAME 
     .byte >DISPLAY_LIST_GAME
            
     .export DISPLAY_LIST_TITLE      
     .export DISPLAY_LIST_GAME


      .export M0									  
      .export M1									  
      .export M2								  
      .export M3									  
      .export M4									  
      .export M5								  
      .export M6									  
      .export M7									  
      .export M8									  
      .export M9									  
      
      .export NDX0									  
      .export NDX1									  
      .export NDX2									  
      .export NDX3									  
      .export SPRITENUM	            
      .export SETSP0COLOR           
      .export SETSP1COLOR           
      .export SETSPWIDTH            
      .export SPRITENHOZ            
      .export SPRITENVRT            
      .export SPHOZNEXT             
      .export SPRHZ0	              
      .export SPRHZ1	              
      .export SPRHZ2	              
      .export SPRHZ3	              
      .export SPZONT                
      .export SPZONB                
      .export SPSRC0                
      .export SPSRC1                
      .export SPSRC2                
      .export SPSRC3                
      .export SPSRC4                
      .export SPRITEUSE                                    
      .export MIBANK                
      .export PMBNK0                          
      .export PMBNK1                    
      .export PMBNK2                    
      .export PMBNK3                

      .export shooter_px     
      .export shooter_py     
      .export shooter_status 
      .export prior_x        
      .export prior_y        
      .export stick_read     

        
      CHARSET_GAME_HI = >CHARSET_GAME
      CHARSET_GAME_ADDR = CHARSET_GAME
      
      .export CHARSET_GAME_HI 
      .export CHARSET_GAME_ADDR
      .export PMBANK
      .export VARBANK                                        
      .export SCREEN_ADDR                                         
      .export GAME_SCREEN_ADDR
      .export PMBANK_HI
      
	
; ------------------------------------------------------------------------

GAME_VBI:
    .export GAME_VBI 
		lda #<GAME00_DLI
		sta VDSLST+0
		lda #>GAME00_DLI
		sta VDSLST+1
;	  lda #$22      
;  	sta COLBAK
    
    JSR PROCESS_SOUNDS
        	
		JMP XITVBV	


TITLE_VBI:
      .export TITLE_VBI 
		lda #<TITLE00_DLI
		sta VDSLST+0
		lda #>TITLE00_DLI
		sta VDSLST+1
		lda #$22
		sta COLBAK
		lda #$36
		sta COLPF3				
		inc RTCLOCK+2
		jmp XITVBV


PAUSE_VBI:
      .export PAUSE_VBI

		jmp XITVBV

TITLE00_DLI:
      .export TITLE00_DLI 
	pha	
	lda #$D8
	sta WSYNC
	sta COLPF0
	lda #$06
	sta COLPF1
	lda #$AA
	sta COLPF1
;lda #>CHARSET_TITLE
;sta CHBASE
  lda #<TITLE01_DLI
  sta VDSLST+0
	lda #>TITLE01_DLI
	sta VDSLST+1
	pla
	rti

TITLE01_DLI:
      .export TITLE01_DLI 
	pha	
	lda #142
	sta WSYNC
	sta COLPF0
	lda #216
	sta COLPF1
	lda #56
	sta COLPF2
; lda #>CHARSET_GAME
; sta CHBASE
	lda #<TITLE00_DLI
	sta VDSLST+0
	lda #>TITLE00_DLI
	sta VDSLST+1
	pla
	rti

TITLE02_DLI:
      .export TITLE02_DLI 
	PHA
  PLA
  RTI


TITLE03_DLI:
      .export TITLE03_DLI 
	PHA
  PLA
  RTI


GAME00_DLI:
      .export GAME00_DLI 
	  PHA
    LDA #$10
    STA COLPF0  
    LDA #$F6
    STA WSYNC
    STA COLPF1  
    LDA #$EC 
    STA COLPF2
    LDA #$5A  
    STA COLPF3
    LDA #$34  
    STA COLBAK
    PLA
  RTI

GAME01_DLI:
      .export GAME01_DLI 
	PHA
  PLA
  RTI

GAME02_DLI:
      .export GAME02_DLI 
	PHA
  PLA
  RTI

GAME03_DLI:
      .export GAME03_DLI 
	PHA
  PLA
  RTI

GAME04_DLI:
      .export GAME04_DLI 
	PHA
  PLA
  RTI




;
;  Title Screen is a Simple Text for Basic Version 


      .include "..\asminc\Atari 8-bit Equates.asm"
      .include "..\asminc\zeropage.inc"      
      .include "atari_sound_commander_bas.asm"

; ------------------------------------------------------------------------
; Actual code
; *** end of main startup code
  
;;        .segment "DATA0"
; ------------------------------------------------------------------------     

MOVE_SHOOTER:
    .export MOVE_SHOOTER
;     PLA
      LDA $0258 
      STA $0256 
      LDA $0259 
      STA $0257 
      LDA STICK0
      TAY       
      AND #$01  
      BNE MS_L2 
      LDA $0259 
      SEC       
      SBC $025B 
      CMP $025E 
      BCS MS_L3 
      LDA $025E 
      STA $0259 
MS_L2:
      TYA       
      AND #$02  
      BNE MS_L4 
      LDA $0259 
      CLC       
      ADC $025B 
      CMP $025F 
      BCC MS_L3 
      LDA $025F 
MS_L3:
      STA $0259
MS_L4:     
      TYA       
      AND #$04  
      BNE MS_L6
      LDA $0258 
      SEC       
      SBC $025A 
      CMP $025C 
      BCS MS_L5
      LDA $025C 
MS_L5:
      STA $0258 
MS_L6:
      TYA       
      AND #$08  
      BNE MS_L8 
      LDA $0258 
      CLC       
      ADC $025A 
      CMP $025D 
      BCC MS_L7
      LDA $025D
MS_L7:
      STA $0258 
MS_L8:
      RTS       


;;;; ***** Need test to see if value can be returned to Basic
GET_SCREEN_BYTE:
      .export GET_SCREEN_BYTE
    PLA 
    PLA 
    CLC
    ADC #10
    LSR
    LSR
    LSR
    TAY
    LDA SCREEN_ROW_LOW,Y
    STA NDX0
    LDA SCREEN_ROW_HIGH,Y
    STA NDX1
    
    STA M6 
    PLA 
    PLA 
    SEC
    SBC #48
    LSR 
    LSR
    TAY
    LDA (NDX0),Y
    BNE Save_Found_Byte
    INY
    LDA (NDX0),Y    
Save_Found_Byte:   
    STA $262 ;610

    RTS
    
; ------------------------------------------------------------------------    
    
PMMOVE:
    
;   PLA
;   PLA
;   PLA
;   PLA
;   PLA
;   PLA
;   PLA     
;   PLA
;   PLA     

;   CMP #$05   
;   BEQ RIGHT_AMOUNT        
;   TAY 
;CLEANSTACK:
;    PLA 
;    PLA 
;    DEY 
;    BPL CLEANSTACK
;    RTS 
    
;RIGHT_AMOUNT:
    PLA 
    PLA 
    STA M6
    PLA 
    PLA 
    STA M7     
    PLA
    PLA
    STA M8
    PLA 
    STA FROMHI
    PLA 
    STA FROMLO    
    PLA
    PLA
    STA PMNUMBER
    TAX    
    LDA M7
    CPX #$05
    BNE COMBOSKIP
    CLC 
    ADC #$08
    STA HPOSM0
    SEC 
    SBC #$02
    STA HPOSM1
    SBC #$02
    STA HPOSM2
    SBC #$02
    LDX #7
COMBOSKIP:
    STA HPOSP0-1,X
    TXA
    CLC 
    ADC #$03
    CMP #$08
    BCC NOCOMBO
    LDA #$03
NOCOMBO:
    CLC
    ADC #PMBANK_HI
    STA PMYPOS+1    
    LDA PMSAVELO,X
    STA PMYPOS+0
    LDA PMSAVESZ,X
    TAY 
    LDA #$00
CLEARLOOP:
    STA (PMYPOS),Y
    DEY 
    BPL CLEARLOOP     
    LDA M6
    STA PMSAVELO,X
    STA PMYPOS+0
    LDA M8
    STA PMSAVESZ,X
    TAY                
    DEY 
DRAWLOOP:
    LDA (FROMLO),Y
    STA (PMYPOS),Y
    DEY 
    BPL DRAWLOOP
    RTS
.export PMMOVE    
     
; ------------------------------------------------------------------------
PMCLEAR:                          
    .export PMCLEAR               
    PLA 
    PLA 
    TAX 
    PLA 
    STA PAGEBSHI
    PLA 
    STA PAGEBASE
NEXTPAGE:
    LDY #$00
    TYA 
PGCLRLOOP:
    STA (PAGEBASE),Y
    DEY 
    BNE PGCLRLOOP
    INC PAGEBSHI
    DEX 
    BNE PGCLRLOOP
    RTS

    




SCREEN_ROW_LOW:
    .byte <(GAME_SCREEN_ADDR +   0),<(GAME_SCREEN_ADDR +   40), <(GAME_SCREEN_ADDR +  80), <(GAME_SCREEN_ADDR + 120), <(GAME_SCREEN_ADDR + 160)
    .byte <(GAME_SCREEN_ADDR + 200), <(GAME_SCREEN_ADDR + 240), <(GAME_SCREEN_ADDR + 280), <(GAME_SCREEN_ADDR + 320), <(GAME_SCREEN_ADDR + 360)  
  	.byte <(GAME_SCREEN_ADDR + 400), <(GAME_SCREEN_ADDR + 440), <(GAME_SCREEN_ADDR + 480), <(GAME_SCREEN_ADDR + 520), <(GAME_SCREEN_ADDR + 560)
  	.byte <(GAME_SCREEN_ADDR + 600), <(GAME_SCREEN_ADDR + 640), <(GAME_SCREEN_ADDR + 680), <(GAME_SCREEN_ADDR + 720), <(GAME_SCREEN_ADDR + 760)
    .byte <(GAME_SCREEN_ADDR + 800), <(GAME_SCREEN_ADDR + 840), <(GAME_SCREEN_ADDR + 880), <(GAME_SCREEN_ADDR + 920), <(GAME_SCREEN_ADDR + 960)
    .byte <(GAME_SCREEN_ADDR + 1000),<(GAME_SCREEN_ADDR + 1040)

SCREEN_ROW_HIGH:
	  .byte >(GAME_SCREEN_ADDR +   0), >(GAME_SCREEN_ADDR +  40), >(GAME_SCREEN_ADDR +  80), >(GAME_SCREEN_ADDR + 120), >(GAME_SCREEN_ADDR + 160)
  	.byte >(GAME_SCREEN_ADDR + 200), >(GAME_SCREEN_ADDR + 240), >(GAME_SCREEN_ADDR + 280), >(GAME_SCREEN_ADDR + 320), >(GAME_SCREEN_ADDR + 360)
  	.byte >(GAME_SCREEN_ADDR + 400), >(GAME_SCREEN_ADDR + 440), >(GAME_SCREEN_ADDR + 480), >(GAME_SCREEN_ADDR + 520), >(GAME_SCREEN_ADDR + 560)
  	.byte >(GAME_SCREEN_ADDR + 600), >(GAME_SCREEN_ADDR + 640), >(GAME_SCREEN_ADDR + 680), >(GAME_SCREEN_ADDR + 720), >(GAME_SCREEN_ADDR + 760)
  	.byte >(GAME_SCREEN_ADDR + 800), >(GAME_SCREEN_ADDR + 840), >(GAME_SCREEN_ADDR + 880), >(GAME_SCREEN_ADDR + 920), >(GAME_SCREEN_ADDR + 960)
  	.byte >(GAME_SCREEN_ADDR + 1000),>(GAME_SCREEN_ADDR + 1040)     


      .export SCREEN_ROW_LOW
      .export SCREEN_ROW_HIGH




P000: .BYTE   $00,$00,$18,$18,$18,$3C,$7E,$DB,$99,$99,$DB,$7E
P001: .BYTE   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
P002: .BYTE   $00,$00,$00,$00,$00,$0F,$7C,$FF,$FF,$55,$55,$00
P003: .BYTE   $00,$00,$00,$00,$00,$0F,$7C,$FF,$FF,$AA,$AA,$00
P004: .BYTE   $00,$00,$00,$00,$00,$F0,$3E,$FF,$FF,$55,$55,$00
P005: .BYTE   $00,$00,$00,$00,$00,$F0,$3E,$FF,$FF,$AA,$AA,$00
P006: .BYTE   $00,$00,$00,$00,$00,$00,$1C,$3C,$3C,$14,$14,$00
P007: .BYTE   $00,$00,$00,$00,$00,$00,$1C,$3C,$3C,$12,$22,$00
P008: .BYTE   $00,$00,$00,$00,$00,$00,$38,$3C,$3C,$28,$28,$00
P009: .BYTE   $00,$00,$00,$00,$00,$00,$38,$3C,$3C,$48,$44,$00
P010: .BYTE   $00,$00,$00,$00,$00,$00,$5B,$FE,$FB,$58,$A8,$A8
P011: .BYTE   $00,$00,$00,$00,$00,$00,$5B,$FE,$FB,$58,$54,$54
P012: .BYTE   $00,$00,$00,$00,$00,$00,$DA,$7F,$DF,$1A,$15,$15
P013: .BYTE   $00,$00,$00,$00,$00,$00,$DA,$7F,$DF,$1A,$2A,$2A
P014: .BYTE   $00,$00,$00,$00,$00,$03,$0E,$3C,$F4,$D0,$40,$00
P015: .BYTE   $00,$00,$00,$00,$00,$03,$0E,$3C,$F2,$C8,$20,$00
P016: .BYTE   $00,$00,$00,$00,$00,$C0,$70,$3C,$2F,$0B,$02,$00
P017: .BYTE   $00,$00,$00,$00,$00,$C0,$70,$3C,$4F,$13,$04,$00
P018: .BYTE   $09,$0A,$1C,$2D,$5A,$1C,$2D,$5A,$1C,$28,$5C,$2A
P019: .BYTE   $48,$28,$1C,$5A,$2D,$1C,$5A,$2D,$1C,$0A,$1D,$2A
P020: .BYTE   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
P021: .BYTE   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
P022: .BYTE   $00,$00,$18,$18,$00,$18,$3C,$7E,$FF,$C3,$00,$00
P023: .BYTE   $00,$00,$00,$18,$18,$00,$18,$7E,$FF,$18,$00,$00
P024: .BYTE   $00,$00,$03,$03,$7C,$FC,$3C,$7C,$6C,$08,$00,$00
P025: .BYTE   $00,$00,$63,$73,$3C,$3C,$3E,$7F,$63,$00,$00,$00
P026: .BYTE   $00,$00,$C0,$E0,$70,$7B,$7B,$70,$E0,$C0,$00,$00
P027: .BYTE   $00,$00,$20,$30,$30,$7B,$7B,$30,$30,$20,$00,$00
P028: .BYTE   $00,$08,$6C,$7C,$3C,$FC,$7C,$03,$03,$00,$00,$00
P029: .BYTE   $00,$00,$63,$7F,$3E,$3C,$3C,$73,$63,$00,$00,$00
P030: .BYTE   $00,$00,$C3,$FF,$7E,$3C,$18,$00,$18,$18,$00,$00
P031: .BYTE   $00,$00,$00,$18,$FF,$7E,$18,$00,$18,$18,$00,$00
P032: .BYTE   $00,$00,$10,$36,$3E,$3C,$3F,$3E,$C0,$C0,$00,$00
P033: .BYTE   $00,$00,$00,$C6,$FE,$7C,$3C,$3C,$CE,$C6,$00,$00
P034: .BYTE   $00,$00,$03,$07,$0E,$DE,$DE,$0E,$07,$03,$00,$00
P035: .BYTE   $00,$00,$04,$0C,$0C,$DE,$DE,$0C,$0C,$04,$00,$00
P036: .BYTE   $00,$00,$C0,$C0,$3E,$3F,$3C,$3E,$36,$10,$00,$00
P037: .BYTE   $00,$00,$C6,$CE,$3C,$3C,$7C,$FE,$C6,$00,$00,$00
P038: .BYTE   $00,$00,$03,$77,$FE,$7C,$1E,$6E,$6E,$04,$00,$00
P039: .BYTE   $00,$00,$13,$3F,$3E,$3E,$1F,$6E,$60,$00,$00,$00
P040: .BYTE   $00,$00,$C0,$EE,$7F,$3E,$78,$76,$76,$20,$00,$00
P041: .BYTE   $00,$00,$C8,$FC,$7C,$7C,$F8,$76,$06,$00,$00,$00
P042: .BYTE   $00,$00,$0F,$0E,$3F,$7F,$FF,$7F,$3F,$0E,$00,$00
P043: .BYTE   $00,$00,$78,$0E,$3F,$7F,$FF,$7F,$3F,$0E,$00,$00
P044: .BYTE   $00,$00,$F0,$70,$FC,$FE,$FF,$FE,$FC,$70,$00,$00
P045: .BYTE   $00,$00,$1E,$70,$FC,$FE,$FF,$FE,$FC,$70,$00,$00



      .export P000    
      .export P001
      .export P002
      .export P003
      .export P004
      .export P005
      .export P006
      .export P007
      .export P008
      .export P009
      .export P010
      .export P011
      .export P012
      .export P013
      .export P014
      .export P015
      .export P016
      .export P017
      .export P018
      .export P019
      .export P020
      .export P021
      .export P022
      .export P023
      .export P024
      .export P025
      .export P026
      .export P027
      .export P028
      .export P029
      .export P030
      .export P031
      .export P032
      .export P033
      .export P034
      .export P035
      .export P036
      .export P037
      .export P038
      .export P039
      .export P040
      .export P041
      .export P042
      .export P043
      .export P044
      .export P045

SPRITEADDR_PLY0LO:
      .byte <P000,<P001,<P002,<P003,<P004,<P005,<P006,<P007,<P008,<P009,<P010,<P011,<P012,<P013,<P014,<P015
      .byte <P016,<P017,<P018,<P019,<P020,<P021,<P022,<P023,<P024,<P025,<P026,<P027,<P028,<P029,<P030,<P031
      .byte <P032,<P033,<P034,<P035,<P036,<P037,<P038,<P039,<P040,<P041,<P042,<P043,<P044,<P045
SPRITEADDR_PLY0HI: 
      .byte >P000,>P001,>P002,>P003,>P004,>P005,>P006,>P007,>P008,>P009,>P010,>P011,>P012,>P013,>P014,>P015
      .byte >P016,>P017,>P018,>P019,>P020,>P021,>P022,>P023,>P024,>P025,>P026,>P027,>P028,>P029,>P030,>P031
      .byte >P032,>P033,>P034,>P035,>P036,>P037,>P038,>P039,>P040,>P041,>P042,>P043,>P044,>P045
SPRITEADDR_PLY0W:
      .word P000,P001,P002,P003,P004,P005,P006,P007,P008,P009,P010,P011,P012,P013,P014,P015
      .word P016,P017,P018,P019,P020,P021,P022,P023,P024,P025,P026,P027,P028,P039,P030,P031
      .word P032,P033,P034,P035,P036,P037,P038,P039,P040,P041,P042,P043,P044,P045

      .export SPRITEADDR_PLY0LO      ; Makes Sprite Address Table available to C compiled program
      .export SPRITEADDR_PLY0HI      ; Makes Sprite Address Table available to C compiled program
      .export SPRITEADDR_PLY0W       ; Makes Sprite Address Table available to C compiled program



SF_SHOOT =       0
SF_BELL_01 =     1
SF_BELL_02 =     2
SF_EXTRA_LIFE =  3
SF_GET_ITEM =    4
SF_EXPLOSION =   5
SF_GAME_OVER =   6

    .export SF_SHOOT
    .export SF_BELL_01 
    .export SF_BELL_02 
    .export SF_EXTRA_LIFE
    .export SF_GET_ITEM 
    .export SF_EXPLOSION 
    .export SF_GAME_OVER 


START_SOUND_ADDR_LO:                                                                                                                                                                                                                                                                       
     .BYTE <SHOOT_SOUND,<SOUND_1,<SOUND_2,<SOUND_4
     .BYTE <SOUND_5
START_SOUND_ADDR_HI:                                                                                                                                                                                                                                                                       
     .BYTE >SHOOT_SOUND,>SOUND_1,>SOUND_2,>SOUND_4
     .BYTE >SOUND_5
START_SOUND_LENGTH_LO:                                                                                                                                                                                                                                                                         
     .BYTE <SHOOT_SOUND_SIZE,<SOUND_1_SIZE,<SOUND_2_SIZE
     .BYTE <SOUND_3_SIZE,<SOUND_4_SIZE,<SOUND_5_SIZE
START_SOUND_LENGTH_HI:                                                                                                                                                                                                                                                                     
     .BYTE >SHOOT_SOUND_SIZE,>SOUND_1_SIZE,>SOUND_2_SIZE
     .BYTE >SOUND_3_SIZE,>SOUND_4_SIZE,>SOUND_5_SIZE

SHOOT_SOUND:
     .BYTE 111,32,110,40,110,48,109,56,108,64,108,72,107,80,106,88,106,96,105,104
     .BYTE 104,112,104,120,103,128,102,136,102,144,101,152,100,160,100,168,99,176
     .BYTE 98,184,98,192,97,200,97,208,97,216,97,224,97,232,97,240,97,248
SHOOT_SOUND_SIZE=(*-SHOOT_SOUND) / 2 + 1
    
SOUND_1:
     .BYTE 15,32,14,32,13,32,11,32,10,32,9,32,7,32,6,32,5,32,3,32,2,32,1,32    
SOUND_1_SIZE=(*-SOUND_1) / 2 + 1
     .BYTE 143,96,143,96,142,96,141,96,140,96,140,96,139,96,138,96,137,96,137,96,136
     .BYTE 96,135,96,134,96,134,96,133,96,132,96,132,96,131,96,131,96,130,96,129,96  
SOUND_2:
     .BYTE 143,255,142,127,140,63,138,31,136,15,134,7,132,3,130,2,129,1
SOUND_2_SIZE=(*-SOUND_2) / 2 + 1
    
SOUND_3:    
     .BYTE 175,1,175,4,175,16,175,64,172,1,172,4,172,16,172,64,169,1,169,4,169,16
     .BYTE 169,64, 166,1,166,4,166,16,166,64,163,1,163,4,163,16,163,64 
SOUND_3_SIZE=(*-SOUND_3) / 2 + 1

SOUND_4:    
     .BYTE 175,64, 175,128,175,192,173,64,173,128,173,192,171,64,171,128,171,192
     .BYTE 169,64, 169,128,169,192,167,64,167,128,167,192,165,64,165,128,165,192
     .BYTE 163,64,163,128,163,192,161,64,161,128,161,192,0,0,0,0
SOUND_4_SIZE=(*-SOUND_4) / 2 + 1

SOUND_5:    
     .BYTE $8F,248,$8F,240,$8E,232,$8E,224,$0D,216,$8D,208,$8C,200,$6C,192
     .BYTE $8B,252,$6B,244,$0A,236,$8A,228,$89,220,$89,212,$88,204,$88,196
     .BYTE $07,248,$87,240,$86,232,$66,224,$85,216,$85,208,$84,200,$84,192
     .BYTE $83,252,$83,244,$83,236,$82,228,$82,220,$62,212,$01,204,$81,196   
SOUND_5_SIZE=(*-SOUND_5) / 2 + 1

