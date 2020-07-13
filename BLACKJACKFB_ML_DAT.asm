
        .segment "ALIGNDATA"
;       .ORG $A000          
     .include "Atari 8-bit Equates.asm"

PMBANK                                  =$1800                                        
VARBANK                                 =$0600                                                                                                                                       
SCREEN_ADDR                             =$BC00                                         
GAME_SCREEN_ADDR                        =$BC00                                        

DLIV0                                   =$BFF8
DLIV1                                   =$BFF9
DLIV2                                   =$BFFA
DLIV3                                   =$BFFB
DLIV4                                   =$BFFC
DLIV5                                   =$BFFD
DLIV6                                   =$BFFE
DLIV7                                   =$BFFF

RTCLOCK                                 =$12
PMBANK_HI                               = >PMBANK   

_M0		        	          	            =$C0
_M1		        	          	            =$C1
_M2		        	          	            =$C2
_M3		        	          	            =$C3
_M4		        	          	            =$C4
_M5		        	          	            =$C5
_M6		        	          	            =$C6
_M7		        	          	            =$C7
_M8		        	          	            =$C8
_M9		        	          	            =$C9
_NDX0									                  =$CA
_NDX1									                  =$CB
_NDX2									                  =$CC
_NDX3									                  =$CD
_HOLDX									                =$CE
_HOLDY									                =$CF

SPRITENUM	                                      =PMBANK+$0180
SETSP0COLOR                                     =PMBANK+$0190
SETSP1COLOR                                     =PMBANK+$01A0
SETSPWIDTH                                      =PMBANK+$01B0
SPRITENHOZ                                      =PMBANK+$01C0
SPRITENVRT                                      =PMBANK+$01E0
SPHOZNEXT                                       =PMBANK+$01F0
                                                
SPRHZ0	                            	          =PMBANK+$0200 
SPRHZ1	                            	          =PMBANK+$0210 
SPRHZ2	                            	          =PMBANK+$0220 
SPRHZ3	                            	          =PMBANK+$0230 
SPZONT                                          =PMBANK+$0240
SPZONB                                          =PMBANK+$0250
SPSRC0                                          =PMBANK+$0260 
SPSRC1                                          =PMBANK+$0278
SPSRC2                                          =PMBANK+$0290
SPSRC3                                          =PMBANK+$02A8
SPSRC4                                          =PMBANK+$02C0
SPRITEUSE                                       =PMBANK+$02D8
                                                
MIBANK                                          =PMBANK+$0300
PMBNK0                                          =PMBANK+$0400
PMBNK1                             	            =PMBANK+$0500
PMBNK2                                          =PMBANK+$0600
PMBNK3                             	            =PMBANK+$0700
            

      .export PMBANK
      .export VARBANK                                        
      .export SCREEN_ADDR                                         
      .export GAME_SCREEN_ADDR
      .export PMBANK_HI
      
                  
CHARSET_GAME:
  .incbin         "Cards.fnt"
CHARSET_TITLE:

TITLE_DATA:


      CHARSET_GAME_HI = > CHARSET_GAME
      CHARSET_TITLE_HI = > CHARSET_TITLE
      CHARSET_GAME_ADDR = CHARSET_GAME
      CHARSET_TITLE_ADDR = CHARSET_TITLE

      
      .export CHARSET_GAME
      .export CHARSET_TITLE
      .export CHARSET_GAME_HI 
      .export CHARSET_TITLE_HI
      .export CHARSET_GAME_ADDR 
      .export CHARSET_TITLE_ADDR
      .export TITLE_DATA
                   
      .export DLIV0  
      .export DLIV1  
      .export DLIV2  
      .export DLIV3  
      .export DLIV4  
      .export DLIV5  
      .export DLIV6  
      .export DLIV7  
      
DISPLAY_LIST_TITLE:
     .byte $70,$F0,$47
     .byte <SCREEN_ADDR
     .byte >SCREEN_ADDR
     .byte $07,$10
     .byte $02,$02,$02,$90,$02,$90,$02,$90,$02,$90,$02,$90,$02,$90,$02,$90,$02,$90,$02,$90,$02,$90,$02,$90,$02,$90,$02,$90,$02,$90,$02,$90,$02,$90,$02,$90,$02,$41
     .byte <DISPLAY_LIST_TITLE 
     .byte >DISPLAY_LIST_TITLE
    
DISPLAY_LIST_GAME:
     .byte $70,$70,$42
     .byte <SCREEN_ADDR
     .byte >SCREEN_ADDR
     .byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$41
     .byte <DISPLAY_LIST_GAME 
     .byte >DISPLAY_LIST_GAME
      
      .export DISPLAY_LIST_TITLE
      .export DISPLAY_LIST_GAME


_SCRREN_ROW_LOW:
    .byte <(SCREEN_ADDR +   0),<(SCREEN_ADDR +   40), <(SCREEN_ADDR +  80), <(SCREEN_ADDR + 120), <(SCREEN_ADDR + 160)
    .byte <(SCREEN_ADDR + 200), <(SCREEN_ADDR + 240), <(SCREEN_ADDR + 280), <(SCREEN_ADDR + 320), <(SCREEN_ADDR + 360)  
  	.byte <(SCREEN_ADDR + 400), <(SCREEN_ADDR + 440), <(SCREEN_ADDR + 480), <(SCREEN_ADDR + 520), <(SCREEN_ADDR + 560)
  	.byte <(SCREEN_ADDR + 600), <(SCREEN_ADDR + 640), <(SCREEN_ADDR + 680), <(SCREEN_ADDR + 720), <(SCREEN_ADDR + 760)
    .byte <(SCREEN_ADDR + 800), <(SCREEN_ADDR + 840), <(SCREEN_ADDR + 880), <(SCREEN_ADDR + 920), <(SCREEN_ADDR + 960)
    .byte <(SCREEN_ADDR + 1000) 
    
_SCRREN_ROW_HIGH:
	  .byte >(SCREEN_ADDR +   0), >(SCREEN_ADDR +  40), >(SCREEN_ADDR +  80), >(SCREEN_ADDR + 120), >(SCREEN_ADDR + 160)
  	.byte >(SCREEN_ADDR + 200), >(SCREEN_ADDR + 240), >(SCREEN_ADDR + 280), >(SCREEN_ADDR + 320), >(SCREEN_ADDR + 360)
  	.byte >(SCREEN_ADDR + 400), >(SCREEN_ADDR + 440), >(SCREEN_ADDR + 480), >(SCREEN_ADDR + 520), >(SCREEN_ADDR + 560)
  	.byte >(SCREEN_ADDR + 600), >(SCREEN_ADDR + 640), >(SCREEN_ADDR + 680), >(SCREEN_ADDR + 720), >(SCREEN_ADDR + 760)
  	.byte >(SCREEN_ADDR + 800), >(SCREEN_ADDR + 840), >(SCREEN_ADDR + 880), >(SCREEN_ADDR + 920), >(SCREEN_ADDR + 960)
  	.byte >(SCREEN_ADDR + 1000)  

      .export _SCRREN_ROW_LOW
      .export _SCRREN_ROW_HIGH




P000:
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255

     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255
     .byte %11111111; $FF  255

     
_SPRITEADDR_PLY0LO:
      .byte <P000
_SPRITEADDR_PLY0HI: 
      .byte >P000
     
_SPRITEADDR_PLY0W:
      .word P000


      .export _SPRITEADDR_PLY0LO      ; Makes Sprite Address Table available to C compiled program
      .export _SPRITEADDR_PLY0HI      ; Makes Sprite Address Table available to C compiled program
      .export _SPRITEADDR_PLY0W       ; Makes Sprite Address Table available to C compiled program

_GAME_VBI:


		jmp XITVBV	


TITLE_VBI:
    LDA #<TITLE00_DLI
    STA VDSLST+0
    LDA #>TITLE00_DLI
    STA VDSLST+1
    LDA #0
    STA DLIV0
    LDA DLIV2
    CLC
    ADC #1
    CMP #60
    BCC No_Title_Effect_60
    LDA #0
 No_Title_Effect_60:
    STA DLIV2
    
		jmp XITVBV
    .export TITLE_VBI

TITLE00_DLI:
	PHA
  TYA
  PHA
  TXA
  PHA
  LDY DLIV2
  LDX #0
TITLE_COLORS_LOOP:
  LDA Title_Text_Colors_Fore,Y
  STY WSYNC
  STA COLPF0
  INY
  CPY #60
  BCC No_Title_Color_Roll_0
  LDY #0
No_Title_Color_Roll_0:
  INX
  CPX #31    
  BCC TITLE_COLORS_LOOP
  LDA #<TITLE01_DLI
  STA VDSLST+0
  LDA #>TITLE01_DLI
  STA VDSLST+1 
  PLA
  TAX

  PLA
  TAY
  PLA
  RTI
      .export TITLE00_DLI 

TITLE01_DLI:
	PHA
  TYA
  PHA
  LDY DLIV0
  CPY DLIV1    
  BNE Not_Selecting_Title_DLI01
  LDA #$76
  STA WSYNC
  STA COLPF2
  LDA #12
  STA COLPF1
  LDA #14
  STA WSYNC
  STA COLPF1
  LDA #12
  STA WSYNC
  STA COLPF1
  LDA #14
  STA WSYNC
  STA COLPF1
  LDA #12
  STA WSYNC
  STA COLPF1
  LDA #14
  STA WSYNC
  STA COLPF1
  LDA #12
  STA WSYNC
  STA COLPF1
  LDA #14         
  STA WSYNC       
  STA COLPF1      
  LDA #12  
  STA WSYNC         
  STA COLPF1        
  LDA #14     
  STA WSYNC   
  STA COLPF1  
  LDA #$D2
  STA COLPF2
  LDA #10
  STA COLPF1
  
  
  
    
Not_Selecting_Title_DLI01:
  INY
  STY DLIV0
  PLA
  TAY
  PLA
  RTI
      .export TITLE01_DLI 

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



Title_Text_Colors_Fore:
  .byte $38,$3A,$3C,$3A,$48,$4A,$4C,$4A,$58,$5A,$5C,$5A,$68,$6A,$6C,$6A,$78,$7A,$7C,$7A,$88,$8A,$8C,$8A,$98,$9A,$9C,$9A,$A8,$AA,$AC,$AA,$B8,$BA,$BC,$BA,$C8,$CA,$CC,$CA,$D8,$DA,$DC,$DA,$E8,$EA,$EC,$EA,$F8,$FA,$FC,$FA,$18,$1A,$1C,$1A,$28,$2A,$2C,$2A
;Title_Text_Colors_Back:
;  .byte $D2,$D0,$00,$D0,$D2,$D0,$00,$D0,$D2,$D0,$00,$D0,$D2,$D0,$00,$D0,$D2,$D0,$00,$D0,$D2,$D0,$00,$D0,$D2,$D0,$00,$D0,$D2,$D4

_GAME00_DLI:
      .export _GAME00_DLI 
	PHA
  PLA
  RTI

_GAME01_DLI:
      .export _GAME01_DLI 
	PHA
  PLA
  RTI

_GAME02_DLI:
      .export _GAME02_DLI 
	PHA
  PLA
  RTI

_GAME03_DLI:
      .export _GAME03_DLI 
	PHA
  PLA
  RTI

_GAME04_DLI:
      .export _GAME04_DLI 
	PHA
  PLA
  RTI





