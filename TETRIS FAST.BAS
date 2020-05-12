DATA Delay_Set() BYTE =  60,52,45,39,34,30,26,23,20,18,16,14,12,10,
DATA             BYTE =  09,08,07,06,05,05,04,04,03,03,03,03,03,02

DATA PETES_TETRIS_DLI() BYTE = 72,169,32,141,10,212,141,24,208,141,26,208,141,27,208,169,12,141,23,208,104,64

DIM GAME_OPTION_SHOW$(9)
GAME_OPTION_SHOW$(1) = "         PLAY FOR HIGH SCORE      "            
GAME_OPTION_SHOW$(2) = "         WIPE OUT 25 LINES        "
GAME_OPTION_SHOW$(3) = "     NEW SCREEN FOR EVERY LEVEL   "
GAME_OPTION_SHOW$(4) = " WIPEOUT MORE LINES FOR NEW LEVEL "
GAME_OPTION_SHOW$(5) = "         WIPE OUT 50 LINES        "
GAME_OPTION_SHOW$(6) = "         WIPE OUT 100 LINES       "
GAME_OPTION_SHOW$(7) = "  PLAY FOR HIGH SCORE WITH HIGHT  "



    HLV=16
    GAME_TYPE=1
    A=0
    B=0
    C=0
    D=0
    H=0
    L=0
    R=0
    W=0
    LS=0
    HLS=0
    NLS=0
    N=0
    M=0    
    Height%=0
    High_Score%=0
    TM=0
    ROT=0
    PROT=0
    Next_Piece=0
    Current_Level%=0
    Puzzle_Piece=0
    Rows_Clear_Remaining = 0
    DIM ArrayString$(32)
  DO
                               
    EXEC TITLE_SCREEN
    EXEC INITIALIZE
    HP=30
    LS=0
    SC=0
    ROWSGOT=0
    IF GAME_TYPE=2 OR GAME_TYPE=4 THEN HP=0
    IF GAME_TYPE=3 OR GAME_TYPE=4 AND Current_Level%>1  
      Height%=INT(Current_Level%/2)-1
      EXEC PUT_RANDOM_SQUARES
    ENDIF
    Next_Piece=1+RAND(7)
    SPD=INT(Current_Level%)
    SPE=SPD
    EXEC S_850
    BRST=4
    IF Current_Level%>7 THEN BRST=12
    IF Current_Level%>15 THEN BRST=16
    Score%=0
    Rows_Clear_Remaining = 10
    SPE=SPD
    EXEC DISPLAY_SCORE
    HOZDLY=0
    
    REPEAT 
      COLOR Puzzle_Piece
      IF Puzzle_Piece = 1
        EXEC DRAW_PIECE_1
      ELIF Puzzle_Piece = 2
        EXEC DRAW_PIECE_2
      ELIF Puzzle_Piece = 3
        EXEC DRAW_PIECE_3
      ELIF Puzzle_Piece = 4
        EXEC DRAW_PIECE_4
      ELIF Puzzle_Piece = 5
        EXEC DRAW_PIECE_5
      ELIF Puzzle_Piece = 6
        EXEC DRAW_PIECE_6
      ELIF Puzzle_Piece = 7
        EXEC DRAW_PIECE_7
      ENDIF
'      ON Puzzle_Piece EXEC DRAW_PIECE_1,DRAW_PIECE_2,DRAW_PIECE_3,DRAW_PIECE_4,DRAW_PIECE_5,DRAW_PIECE_6,DRAW_PIECE_7:PAUSE 2
      REPEAT 
        COLOR 32
        IF Puzzle_Piece = 1
          EXEC DRAW_PIECE_1
        ELIF Puzzle_Piece = 2
          EXEC DRAW_PIECE_2
        ELIF Puzzle_Piece = 3
          EXEC DRAW_PIECE_3
        ELIF Puzzle_Piece = 4
          EXEC DRAW_PIECE_4
        ELIF Puzzle_Piece = 5
          EXEC DRAW_PIECE_5
        ELIF Puzzle_Piece = 6
          EXEC DRAW_PIECE_6
        ELIF Puzzle_Piece = 7
          EXEC DRAW_PIECE_7
        ENDIF              
        Y=Y+1
        SPL=SPL+40
        REPEAT 
          IF PEEK(764)=33 THEN EXEC S_900       
          P=STICK(0)
          EXEC S_500
          IF STRIG(0)=1
            BDLY=0
          ELSE 
            IF BDLY=0 OR BDLY>=20 THEN EXEC S_550
            BDLY=BDLY+1:IF BDLY>=21 THEN BDLY=BRST
           ENDIF 
            IF Puzzle_Piece = 1
              EXEC DRAW_PIECE_1
            ELIF Puzzle_Piece = 2
              EXEC DRAW_PIECE_2
            ELIF Puzzle_Piece = 3
              EXEC DRAW_PIECE_3
            ELIF Puzzle_Piece = 4
              EXEC DRAW_PIECE_4
            ELIF Puzzle_Piece = 5
              EXEC DRAW_PIECE_5
            ELIF Puzzle_Piece = 6
              EXEC DRAW_PIECE_6
            ELIF Puzzle_Piece = 7
              EXEC DRAW_PIECE_7
            ENDIF
         UNTIL PEEK(20)>=TM OR (P&3<>3)
         IF P=14 AND C=0 THEN EXEC FAST_DROP_PIECE
         POKE 20,0
      UNTIL C>0
      EXEC S_700
      EXEC S_850
      'ON Puzzle_Piece EXEC CHECK_PIECE_1,CHECK_PIECE_2,CHECK_PIECE_3,CHECK_PIECE_4,CHECK_PIECE_5,CHECK_PIECE_6,CHECK_PIECE_7

      IF Puzzle_Piece = 1
        EXEC CHECK_PIECE_1
      ELIF Puzzle_Piece = 2
        EXEC CHECK_PIECE_2
      ELIF Puzzle_Piece = 3
        EXEC CHECK_PIECE_3
      ELIF Puzzle_Piece = 4
        EXEC CHECK_PIECE_4
      ELIF Puzzle_Piece = 5
        EXEC CHECK_PIECE_5
      ELIF Puzzle_Piece = 6
        EXEC CHECK_PIECE_6
      ELIF Puzzle_Piece = 7
        EXEC CHECK_PIECE_7
      ENDIF      
      IF PEEK(53279)&1=0 
          C=255
          T=255
      ENDIF
    UNTIL C OR T
      FOR M=W TO W+959
        POKE M,64!(PEEK(53770)&7)
      NEXT M
      PAUSE 30
  LOOP
'------------------------------
PROC FAST_DROP_PIECE
      COLOR 32
      IF Puzzle_Piece = 1
        EXEC DRAW_PIECE_1
      ELIF Puzzle_Piece = 2
        EXEC DRAW_PIECE_2
      ELIF Puzzle_Piece = 3
        EXEC DRAW_PIECE_3
      ELIF Puzzle_Piece = 4
        EXEC DRAW_PIECE_4
      ELIF Puzzle_Piece = 5
        EXEC DRAW_PIECE_5
      ELIF Puzzle_Piece = 6
        EXEC DRAW_PIECE_6
      ELIF Puzzle_Piece = 7
        EXEC DRAW_PIECE_7
      ENDIF

      IF Y<16 AND Y<HP-4
        REPEAT 
          Y=Y+1:SPL=SPL+40
        UNTIL Y>15 OR Y>=HP-4
      ENDIF 
      REPEAT 
        SPL=SPL+40:Y=Y+1
        IF Puzzle_Piece = 1
          EXEC DRAW_PIECE_1
        ELIF Puzzle_Piece = 2
          EXEC DRAW_PIECE_2
        ELIF Puzzle_Piece = 3
          EXEC DRAW_PIECE_3
        ELIF Puzzle_Piece = 4
          EXEC DRAW_PIECE_4
        ELIF Puzzle_Piece = 5
          EXEC DRAW_PIECE_5
        ELIF Puzzle_Piece = 6
          EXEC DRAW_PIECE_6
        ELIF Puzzle_Piece = 7
          EXEC DRAW_PIECE_7
        ENDIF
        
      UNTIL C > 0
ENDPROC 
'------------------------------
PROC S_500
      IF P&12<>12
        IF HOZDLY=0 OR HOZDLY>=20
          COLOR 32
          IF Puzzle_Piece = 1
            EXEC DRAW_PIECE_1
          ELIF Puzzle_Piece = 2
            EXEC DRAW_PIECE_2
          ELIF Puzzle_Piece = 3
            EXEC DRAW_PIECE_3
          ELIF Puzzle_Piece = 4
            EXEC DRAW_PIECE_4
          ELIF Puzzle_Piece = 5
            EXEC DRAW_PIECE_5
          ELIF Puzzle_Piece = 6
            EXEC DRAW_PIECE_6
          ELIF Puzzle_Piece = 7
            EXEC DRAW_PIECE_7
          ENDIF
          IF P&8=0 AND NOT R  
            X=X+1
            SPL=SPL+1
          ENDIF
          IF P&4=0 AND NOT L  
            X=X-1
            SPL=SPL-1
          ENDIF
        ENDIF 
        HOZDLY=HOZDLY+1:IF HOZDLY>=21 THEN HOZDLY=BRST
      ELSE 
        HOZDLY=0
      ENDIF 
      COLOR Puzzle_Piece
ENDPROC 
'------------------------------
PROC S_550
      DPOKE 53762,44806:PROT=ROT:COLOR 32
      IF Puzzle_Piece = 1
        EXEC DRAW_PIECE_1
      ELIF Puzzle_Piece = 2
        EXEC DRAW_PIECE_2
      ELIF Puzzle_Piece = 3
        EXEC DRAW_PIECE_3
      ELIF Puzzle_Piece = 4
        EXEC DRAW_PIECE_4
      ELIF Puzzle_Piece = 5
        EXEC DRAW_PIECE_5
      ELIF Puzzle_Piece = 6
        EXEC DRAW_PIECE_6
      ELIF Puzzle_Piece = 7
        EXEC DRAW_PIECE_7
      ENDIF
      ROT=(ROT+1)&3
      'ON Puzzle_Piece EXEC CHECK_PIECE_1,CHECK_PIECE_2,CHECK_PIECE_3,CHECK_PIECE_4,CHECK_PIECE_5,CHECK_PIECE_6,2240:
      IF Puzzle_Piece = 1
        EXEC CHECK_PIECE_1
      ELIF Puzzle_Piece = 2
        EXEC CHECK_PIECE_2
      ELIF Puzzle_Piece = 3
        EXEC CHECK_PIECE_3
      ELIF Puzzle_Piece = 4
        EXEC CHECK_PIECE_4
      ELIF Puzzle_Piece = 5
        EXEC CHECK_PIECE_5
      ELIF Puzzle_Piece = 6
        EXEC CHECK_PIECE_6
      ELIF Puzzle_Piece = 7
        EXEC CHECK_PIECE_7
      ENDIF
      IF T THEN ROT=PROT
      POKE 53762,0
      COLOR Puzzle_Piece
      H=1
ENDPROC  
'------------------------------


PROC S_600
      Rows_Clear_Remaining = Rows_Clear_Remaining -1 
      LS=LS+ROWSGOT
      IF Rows_Clear_Remaining<=0 
        Rows_Clear_Remaining = 10
        SPD=INT(Current_Level%)
        EXEC SET_UP_LEVEL_FONT
      ENDIF
ENDPROC 
'------------------------------
PROC S_620
      LS=LS-ROWSGOT
      IF LS<=0 THEN LS=0
      Score%=Score%+(Height%^2)*1000+INT(Current_Level%)*1000
      EXEC DISPLAY_SCORE
      PRINT 
      PRINT "		SUCCESS"
ENDPROC 
'------------------------------
PROC S_640
      LS=LS+ROWSGOT
      IF LS<NLS THEN EXIT
      Current_Level%=Current_Level%+1
      Score%=Score%+INT(Current_Level%)*1000
      NLS=NLS+10
      EXEC DISPLAY_SCORE
      COLOR 32
      SPD=INT(Current_Level%)
      EXEC S_655
ENDPROC 
'------------------------------
PROC S_655
      FOR N=1 TO 22 STEP 1
        FOR M=0 TO 1
          IF N<19 THEN PLOT 12+RAND(10),INT(N+3)
          IF N<20 THEN PLOT 12+RAND(10),INT(N+2)
          IF N<18 THEN PLOT 12+RAND(10),INT(N+RAND(5))
          IF N<19 THEN PLOT 12+RAND(10),INT(N+1)
          IF N<18 THEN PLOT 12+RAND(10),INT(N+RAND(5))
          PLOT 12,N:DRAWTO 21,N
        NEXT M             
      NEXT N
      IF Current_Level%>1  
        Height%=(Current_Level%/2)-1
        EXEC PUT_RANDOM_SQUARES
      ENDIF
ENDPROC 
'------------------------------
PROC S_670
      LS=LS+ROWSGOT:IF LS<NLS THEN EXIT 
      IF Current_Level%<10 THEN NLS=NLS+4+INT(Current_Level%*2)
      IF Current_Level%>9 THEN NLS=NLS+25
      Current_Level%=Current_Level%+1
      Score%=Score%+Current_Level%*1000
      EXEC DISPLAY_SCORE
      NLS=NLS+4+INT(Current_Level%*2)
      COLOR 32
      SPD=SPD+1
      EXEC S_655
ENDPROC 
'------------------------------
PROC S_700  
    SOUND 1,RAND(32)+224,0,12
    COLOR Puzzle_Piece
      IF Puzzle_Piece = 1
        EXEC DRAW_PIECE_1
      ELIF Puzzle_Piece = 2
        EXEC DRAW_PIECE_2
      ELIF Puzzle_Piece = 3
        EXEC DRAW_PIECE_3
      ELIF Puzzle_Piece = 4
        EXEC DRAW_PIECE_4
      ELIF Puzzle_Piece = 5
        EXEC DRAW_PIECE_5
      ELIF Puzzle_Piece = 6
        EXEC DRAW_PIECE_6
      ELIF Puzzle_Piece = 7
        EXEC DRAW_PIECE_7
      ENDIF
    ROWSGOT=0
    POKE 77,0
    Q=W+92:Score%=((R>0)+(L>0)+(C>0))*SPE+Score%
    IF Y<HP THEN HP=Y:Q=W-68+HP*40:IF HP<4 THEN HP=4
    SOUND 1,0,10,15
    FOR N=W+892 TO Q STEP -40
      CNT=0
      FOR M=N TO N+9
        IF PEEK(M)>64 THEN CNT=CNT+1
      NEXT M
      IF CNT>=10  
        A=N
        EXEC S_780
        N=N+40
      ENDIF
    NEXT N
    IF ROWSGOT > 0
      IF GAME_TYPE =1 
        EXEC S_600
      ELIF GAME_TYPE = 2       
        EXEC S_620 
      ELIF GAME_TYPE = 3
        EXEC S_640
      ELIF GAME_TYPE = 4 
        EXEC S_670 
      ELIF GAME_TYPE = 5
        EXEC S_620 
      ELIF GAME_TYPE = 6
        EXEC S_620 
      ELIF GAME_TYPE = 7
        EXEC S_600    
      ENDIF
    ENDIF
        IF ROWSGOT=1 
          Score%=Score%+40
          FOR N=90 TO 0 STEP -1
            M=INT(N/6)
            SOUND 1,6+N&30,10,M
          NEXT N
        ELIF ROWSGOT=2 
          Score%=Score%+100
          FOR N=150 TO 0 STEP -1
            M=INT(N/10)
            SOUND 1,4+N&30,10,M
          NEXT N
        ELIF ROWSGOT=3 
          Score%=Score%+300
          FOR N=210 TO 0 STEP -1
            M=INT(N/14)
            SOUND 1,2+N&31,10,M
          NEXT N            
        ELIF ROWSGOT=4 
          Score%=Score%+1200
          FOR N=255 TO 0 STEP -1
            M=INT(N/16)
            SOUND 1,N&31,10,N
          NEXT N
        ENDIF
        HP=HP+ROWSGOT
        Q=Q+40*ROWSGOT
        EXEC DISPLAY_SCORE

ENDPROC 
'------------------------------
PROC S_780
    IF Q<W THEN Q=W
    POKE 53762,32
    FOR D=A TO Q STEP -40
      MOVE D-40,D,10
    NEXT D
    POKE 53762,0
    ROWSGOT=ROWSGOT+1
ENDPROC 
'------------------------------
PROC DISPLAY_SCORE 
    CLS
    PRINT "SCORE:","LINES:","LEVEL:"
    PRINT Score%,LS,INT(Current_Level%);
ENDPROC
'------------------------------
PROC S_850   
    X=29
    Y=9
    SPL=W+X+40*Y
    ROT=0
    COLOR 32
    IF Next_Piece = 1
      EXEC DRAW_PIECE_1
    ELIF Next_Piece = 2
      EXEC DRAW_PIECE_2
    ELIF Next_Piece = 3
      EXEC DRAW_PIECE_3
    ELIF Next_Piece = 4
      EXEC DRAW_PIECE_4
    ELIF Next_Piece = 5
      EXEC DRAW_PIECE_5
    ELIF Next_Piece = 6
      EXEC DRAW_PIECE_6
    ELIF Next_Piece = 7
      EXEC DRAW_PIECE_7
    ENDIF    
    Puzzle_Piece=Next_Piece
    Next_Piece=1+RAND(7)
    COLOR Next_Piece
    IF Next_Piece = 1
      EXEC DRAW_PIECE_1
    ELIF Next_Piece = 2
      EXEC DRAW_PIECE_2
    ELIF Next_Piece = 3
      EXEC DRAW_PIECE_3
    ELIF Next_Piece = 4
      EXEC DRAW_PIECE_4
    ELIF Next_Piece = 5
      EXEC DRAW_PIECE_5
    ELIF Next_Piece = 6
      EXEC DRAW_PIECE_6
    ELIF Next_Piece = 7
      EXEC DRAW_PIECE_7
    ENDIF
    TM=Delay_Set(SPD)
    X=16
    Y=0
    SPL=W+X+40*Y
    IF TM<0 THEN TM=0
    BDLY=5
    IF BDLY>INT(TM/2) THEN BDLY=INT(TM/2)
ENDPROC
'------------------------------ 
PROC S_900
    REPEAT 
      POKE 764,255:PAUSE 1
    UNTIL PEEK(764)=255
    ? "		PAUSE";
    REPEAT 
      POKE 77,128:POKE 78,246
    UNTIL PEEK(764)=33
    POKE 764,255
    EXEC DISPLAY_SCORE
    POKE 77,0
ENDPROC 
'------------------------------
' Draw Piece?'
PROC DRAW_PIECE_1
    IF ROT=0
      PLOT X-1,Y:DRAWTO X+2,Y
      L=PEEK(SPL-2):R=PEEK(SPL+3)
      C=PEEK(SPL+39)!PEEK(SPL+40)!PEEK(SPL+41)!PEEK(SPL+42)
    ELIF ROT=1
      PLOT X,Y-1:DRAWTO X,Y+2
      L=PEEK(SPL-41)+PEEK(SPL-1)+PEEK(SPL+39)+PEEK(SPL+79)
      C=PEEK(SPL+120)
      R=PEEK(SPL-39)+PEEK(SPL+1)+PEEK(SPL+41)+PEEK(SPL+81)
    ELIF ROT=2
      PLOT X-2,Y:DRAWTO X+1,Y
      L=PEEK(SPL-3):R=PEEK(SPL+2)
      C=PEEK(SPL+38)!PEEK(SPL+39)!PEEK(SPL+40)!PEEK(SPL+41)
    ELIF ROT=3
      PLOT X,Y-2:DRAWTO X,Y+1
      L=PEEK(SPL-81)+PEEK(SPL-41)+PEEK(SPL-1)+PEEK(SPL+39)
      C=PEEK(SPL+80)
      R=PEEK(SPL-79)+PEEK(SPL-39)+PEEK(SPL+1)+PEEK(SPL+41)
    ENDIF  
ENDPROC 
'------------------------------
PROC DRAW_PIECE_2
    IF ROT=0 OR ROT=2
      PLOT X-1,Y:PLOT X,Y:PLOT X,Y+1:PLOT X+1,Y+1
      R=PEEK(SPL+1)+PEEK(SPL+42)
      C=PEEK(SPL+39)+DPEEK(SPL+80)
      L=PEEK(SPL-2)+PEEK(SPL+39)
    ELSE 
      PLOT X,Y-1:PLOT X-1,Y:PLOT X,Y:PLOT X-1,Y+1
      L=PEEK(SPL-41)+PEEK(SPL-2)+PEEK(SPL+38)
      R=PEEK(SPL-39)+PEEK(SPL+1)+PEEK(SPL+40)
      C=PEEK(SPL+79)+PEEK(SPL+40)
    ENDIF 
ENDPROC 
'------------------------------
PROC DRAW_PIECE_3
    IF ROT=0 OR ROT=2
      PLOT X+1,Y:PLOT X,Y:PLOT X-1,Y+1:PLOT X,Y+1
      L=PEEK(SPL-1)+PEEK(SPL+38)
      R=PEEK(SPL+2)+PEEK(SPL+41)
      C=PEEK(SPL+41)+DPEEK(SPL+79)
    ELSE 
      PLOT X-1,Y-1:PLOT X-1,Y:PLOT X,Y:PLOT X,Y+1
      L=PEEK(SPL-2)+PEEK(SPL-42)+PEEK(SPL+39)
      R=PEEK(SPL-40)+PEEK(SPL+1)+PEEK(SPL+41)
      C=PEEK(SPL+39)+PEEK(SPL+80)
    ENDIF 
ENDPROC 
'------------------------------
PROC DRAW_PIECE_4
    IF ROT=0
        PLOT X,Y:PLOT X-1,Y+1:DRAWTO X+1,Y+1
        L=PEEK(SPL-1)+PEEK(SPL+38)
        R=PEEK(SPL+1)+PEEK(SPL+42)
        C=PEEK(SPL+79)+DPEEK(SPL+80)
    ELIF ROT=1
        PLOT X,Y:PLOT X-1,Y-1:DRAWTO X-1,Y+1
        L=PEEK(SPL-42)+PEEK(SPL-2)+PEEK(SPL+38)
        R=PEEK(SPL-40)+PEEK(SPL+1)+PEEK(SPL+40)
        C=PEEK(SPL+40)+PEEK(SPL+79)
    ELIF ROT=2
        PLOT X,Y:PLOT X-1,Y-1:DRAWTO X+1,Y-1
        L=PEEK(SPL-1)+PEEK(SPL-42)
        R=PEEK(SPL+1)+PEEK(SPL-38)
        C=PEEK(SPL-1)+PEEK(SPL+40)+PEEK(SPL+1)
    ELIF ROT=3
        PLOT X,Y:PLOT X+1,Y-1:DRAWTO X+1,Y+1
        L=PEEK(SPL-40)+PEEK(SPL-1)+PEEK(SPL+40)
        R=PEEK(SPL-38)+PEEK(SPL+2)+PEEK(SPL+42)
        C=PEEK(SPL+40)+PEEK(SPL+81)
    ENDIF  
ENDPROC
'------------------------------
PROC DRAW_PIECE_5
  IF ROT=0
        PLOT X-1,Y:DRAWTO X+1,Y:PLOT X-1,Y+1
        L=PEEK(SPL-2)+PEEK(SPL+38)
        R=PEEK(SPL+2)+PEEK(SPL+40)
        C=PEEK(SPL+41)+PEEK(SPL+40)+PEEK(SPL+79)
  ELIF ROT=1
        PLOT X-1,Y-1:PLOT X,Y-1:DRAWTO X,Y+1
        L=PEEK(SPL-42)+PEEK(SPL-1)+PEEK(SPL+39)
        R=PEEK(SPL-39)-PEEK(SPL+1)+PEEK(SPL+41)
        C=PEEK(SPL-1)+PEEK(SPL+80)
  ELIF ROT=2
        PLOT X+1,Y-1:PLOT X-1,Y:DRAWTO X+1,Y
        L=PEEK(SPL-40)+PEEK(SPL-2)
        R=PEEK(SPL-38)+PEEK(SPL+2)
        C=PEEK(SPL+39)+DPEEK(SPL+40)
   ELIF ROT=3 
        PLOT X,Y-1:DRAWTO X,Y+1:PLOT X+1,Y+1
        L=PEEK(SPL-41)+PEEK(SPL-1)+PEEK(SPL+39)
        R=PEEK(SPL-39)+PEEK(SPL+1)+PEEK(SPL+42)
        C=PEEK(SPL+80)+PEEK(SPL+81)
    ENDIF 
ENDPROC 
'------------------------------
PROC DRAW_PIECE_6
    IF ROT=0
            PLOT X-1,Y:DRAWTO X+1,Y:PLOT X+1,Y+1
            L=PEEK(SPL-2)+PEEK(SPL+40)
            R=PEEK(SPL+2)+PEEK(SPL+42)
            C=PEEK(SPL+39)+PEEK(SPL+40)+PEEK(SPL+81)
    ELIF ROT=1
            PLOT X,Y-1:DRAWTO X,Y+1:PLOT X+1,Y-1
            L=PEEK(SPL-41)+PEEK(SPL-1)+PEEK(SPL+39)
            R=PEEK(SPL-38)+PEEK(SPL+1)+PEEK(SPL+41)
            C=PEEK(SPL+1)+PEEK(SPL+80)
    ELIF ROT=2
            PLOT X-1,Y-1:PLOT X-1,Y:DRAWTO X+1,Y
            L=PEEK(SPL-42)+PEEK(SPL-2)
            R=PEEK(SPL-40)+PEEK(SPL+2)
            C=PEEK(SPL+39)+PEEK(SPL+40)+PEEK(SPL+41)
    ELIF ROT=3 
            PLOT X,Y-1:DRAWTO X,Y+1:PLOT X-1,Y+1
            L=PEEK(SPL-41)+PEEK(SPL-1)+PEEK(SPL+38)
            R=PEEK(SPL-39)+PEEK(SPL+1)+PEEK(SPL+41)
            C=PEEK(SPL+79)+PEEK(SPL+80)
    ENDIF 
ENDPROC 
'------------------------------
PROC DRAW_PIECE_7
      PLOT X,Y:PLOT X+1,Y:PLOT X+1,Y+1:PLOT X,Y+1
      L=PEEK(SPL-1)+PEEK(SPL+39)
      C=PEEK(SPL+80)+PEEK(SPL+81)
      R=PEEK(SPL+2)+PEEK(SPL+42)
ENDPROC 
'------------------------------
' Check Piece?'

PROC CHECK_PIECE_1
      IF ROT=0
          T=PEEK(SPL-1)!PEEK(SPL)!PEEK(SPL+1)!PEEK(SPL+2)
      ELIF ROT=1
          T=PEEK(SPL-40)!PEEK(SPL)!PEEK(SPL+80)!PEEK(SPL+120)
      ELIF ROT = 2
          T=PEEK(SPL-2)!PEEK(SPL-1)!PEEK(SPL)!PEEK(SPL+1)
      ELIF ROT = 3
          T=PEEK(SPL-80)!PEEK(SPL-40)!PEEK(SPL)!PEEK(SPL+40)
      ENDIF  
ENDPROC 
'------------------------------
PROC CHECK_PIECE_2
      IF ROT=0
        T=PEEK(SPL-1)!PEEK(SPL)+DPEEK(SPL+40)
      ELIF ROT = 1
          T=PEEK(SPL-40)+DPEEK(SPL-1)!PEEK(SPL+39)
      ELIF ROT = 2
          T=PEEK(SPL-1)!PEEK(SPL)+DPEEK(SPL+40)
      ELIF ROT = 3 
          T=PEEK(SPL-40)+DPEEK(SPL-1)!PEEK(SPL+39)
      ENDIF  
ENDPROC 
'------------------------------
PROC CHECK_PIECE_3
      IF ROT=0
          T=PEEK(SPL+1)+DPEEK(SPL-1)!PEEK(SPL+40)
      ELIF ROT = 1
          T=PEEK(SPL-41)+DPEEK(SPL-1)!PEEK(SPL+40)
      ELIF ROT = 2
          T=PEEK(SPL+1)+DPEEK(SPL-1)!PEEK(SPL+40)
      ELIF ROT = 3 
          T=PEEK(SPL-41)+DPEEK(SPL-1)!PEEK(SPL+40)
      ENDIF  
ENDPROC 
'------------------------------
PROC CHECK_PIECE_4
      IF ROT=0
          T=PEEK(SPL)+DPEEK(SPL+39)!PEEK(SPL+41)
      ELIF ROT = 1
          T=DPEEK(SPL)!PEEK(SPL-41)!PEEK(SPL+39)
      ELIF ROT = 2
          T=PEEK(SPL)+DPEEK(SPL-40)!PEEK(SPL-41)
      ELIF ROT = 3 
          T=DPEEK(SPL)!PEEK(SPL-39)!PEEK(SPL+41)
      ENDIF 
ENDPROC 
'------------------------------
PROC CHECK_PIECE_5
      IF ROT=0
          T=DPEEK(SPL-1)!PEEK(SPL)!PEEK(SPL+39)
      ELIF ROT = 1
          T=DPEEK(SPL-41)!PEEK(SPL)!PEEK(SPL+40)
      ELIF ROT = 2
          T=PEEK(SPL-39)+DPEEK(SPL-1)!PEEK(SPL+1)
      ELIF ROT = 3 
          T=PEEK(SPL-40)!PEEK(SPL)+DPEEK(SPL+40)
      ENDIF 
ENDPROC 
'------------------------------
PROC CHECK_PIECE_6
      IF ROT=0
          T=PEEK(SPL-1)+DPEEK(SPL)!PEEK(SPL+41)
      ELIF ROT = 1
          T=DPEEK(SPL-40)!PEEK(SPL)!PEEK(SPL+40)
      ELIF ROT = 2
          T=PEEK(SPL-41)+DPEEK(SPL-1)!PEEK(SPL+1)
      ELIF ROT = 3 
          T=PEEK(SPL-40)!PEEK(SPL)+DPEEK(SPL+39)
      ENDIF  
ENDPROC 
'------------------------------
PROC CHECK_PIECE_7
    T=PEEK(SPL)!PEEK(SPL+1)!PEEK(SPL+40)!PEEK(SPL+41)
ENDPROC 
'------------------------------
PROC TITLE_SCREEN
      GRAPHICS 0
      Current_Level%=INT(Current_Level%):Height%=INT(Height%)
      POKE 82,0
      POKE 710,32:POKE 709,8:POKE 712,32
      POSITION 0,3
      PRINT "             T E T R I S"
      PRINT      
      PRINT "             DEVELOPER: ALEXEY PAJITNOV"
      PRINT " FAST BASIC CONVERSION: PETER J. MEYER"     
      PRINT "ORIGINAL BASIC VERSION: DAVID MASON"      

      IF Score%>High_Score% THEN High_Score%=Score%
      IF LS>HLS AND (GAME_TYPE=1 OR GAME_TYPE=3 OR GAME_TYPE=4 OR GAME_TYPE=7) THEN HLS=LS
      IF Current_Level%>HLV THEN HLV=INT(Current_Level%)
      POSITION 0,10
      ? ,"LAST","HIGHEST"
      ? "SCORE:",Score%,High_Score%
      ? "LINES:",LS,HLS
      POSITION 2,16
      PRINT "SELECT LEVEL:";Current_Level%;"   "
      POSITION 20,16
      PRINT "OPTION TYPE:";
      IF GAME_TYPE=1 THEN ? "A"
      IF GAME_TYPE=2 THEN ? "B":POSITION 17,19:? "HIGHT:";Height%;"  "
      IF GAME_TYPE=3 THEN ? "C":NLS=10
      IF GAME_TYPE=4 THEN ? "D":NLS=5
      IF GAME_TYPE=5 THEN ? "E":POSITION 17,19:? "HIGHT:";Height%;"  "
      IF GAME_TYPE=6 THEN ? "F":POSITION 17,19:? "HIGHT:";Height%;"  "
      IF GAME_TYPE=7 THEN ? "G":POSITION 17,19:? "HIGHT:";Height%;"  "
      POSITION 2,21
      PRINT GAME_OPTION_SHOW$(GAME_TYPE);
      POSITION 0,0
      REPEAT 
        REPEAT 
          P=PEEK(53279)&7
        UNTIL P<7 OR (GAME_TYPE=2 OR GAME_TYPE=5 OR GAME_TYPE=6 OR GAME_TYPE=7) AND PEEK(732)=17
        IF P&2=0 
          Current_Level%=Current_Level%+1
          IF Current_Level% >= HLV THEN Current_Level% = 0
        ENDIF          
        IF P&4=0 THEN GAME_TYPE=GAME_TYPE MOD 7+1
        POSITION 2,21
        PRINT GAME_OPTION_SHOW$(GAME_TYPE);
        POSITION 2,16
        PRINT "SELECT LEVEL:";Current_Level%;"   "
        POSITION 20,16
        PRINT "OPTION GAME_TYPE:";
        IF GAME_TYPE=1 THEN ? "A":LS=0
        IF GAME_TYPE=2 THEN ? "B":LS=25
        IF GAME_TYPE=3 THEN ? "C":LS=0:NLS=10
        IF GAME_TYPE=4 THEN ? "D":LS=0:NLS=4
        IF GAME_TYPE=5 THEN ? "E":LS=50
        IF GAME_TYPE=6 THEN ? "F":LS=100
        IF GAME_TYPE=7 THEN ? "G":LS=0
        IF GAME_TYPE=2 OR GAME_TYPE=5 OR GAME_TYPE=6 OR GAME_TYPE=7
          IF PEEK(732)=17  
            Height%=Height%+1
            REPEAT 
              POKE 732,0
              UNTIL PEEK(732)=0
              IF Height%>6 THEN Height%=0
          ENDIF
          POSITION 12,19:? "HELP HIGHT:";Height%;"  "
        ELSE 
          POSITION 12,19:? "               "
        ENDIF 
        PAUSE 5
      UNTIL STRIG(0)=0 or P&1=0
ENDPROC
 '------------------------------
PROC PUT_RANDOM_SQUARES
      IF Height%>6.5 THEN Height%=6.5
      IF HP>21-INT(Height%*2) THEN HP=21-INT(Height%*2):Q=W-68+HP*40
      FOR N=22 TO 22-INT(Height%*2) STEP -1
        FOR M=0 TO 6
          COLOR RAND(7)+1:PLOT 12+RAND(10),INT(N)
        NEXT M
      NEXT N
      Score%=Score%+(Height%^2)*1000
      EXEC DISPLAY_SCORE
      EXEC SET_UP_LEVEL_FONT
ENDPROC 
'------------------------------
PROC INITIALIZE
      GRAPHICS 0:DL=DPEEK(560)-256:W=DL+40:DPOKE 560,DL:DPOKE 88,W
      POKE DL,32:POKE DL+1,66:DPOKE DL+2,W+920:POKE DL+4,66:DPOKE DL+5,W+40
      FOR N=DL+7 TO DL+31:POKE N,2:NEXT N
      POKE DL+32,65:DPOKE DL+33,DL
      POKE DL+28,130:POKE 623,128
      DPOKE 512,ADR(PETES_TETRIS_DLI):POKE 54286,192
      DPOKE 660,W+960
      POKE 703,4
      MOVE $E000,$B000,$0400
      POKE 756,176
      FOR N=0 TO 15
        A=N+16*N
        FOR M=$B200+N*8 TO $B207+N*8
            POKE M,A
        NEXT M
      NEXT N
      EXEC SET_UP_LEVEL_FONT
      POKE 87,12
      POKE W,0:MOVE W,W+1,1020
      POKE 704,0
      POKE 705,68
      POKE 706,102
      POKE 707,180
      POKE 708,230
      POKE 709,28
      POKE 710,58
      POKE 711,130
      POKE 752,1
      COLOR 8:PLOT 22,0:DRAWTO 22,23:DRAWTO 11,23:DRAWTO 11,0:PLOT 26,7:DRAWTO 32,7:DRAWTO 32,12
      DRAWTO 26,12:DRAWTO 26,7
      COLOR 17:PLOT 23,0:DRAWTO 23,23
      COLOR 16:PLOT 33,7:COLOR 17:PLOT 33,8:DRAWTO 33,12:COLOR 18:PLOT 33,13
      COLOR 19:PLOT 26,13:COLOR 20:PLOT 27,13:DRAWTO 32,13
      COLOR 15:FCOLOR 15 
      PLOT 0,0:FILLTO 0,23
      PLOT 24,0:FILLTO 24,23 
      PLOT 34,6:FILLTO 34,13
ENDPROC

'------------------------------
PROC SET_UP_LEVEL_FONT
        POKE $B278,0:MOVE $B278,$B279,48
        C=RAND(2)
        D=(INT(Current_Level%+1) MOD 10)
        
        IF D=0 
          POKE 712,228
          EXIT
        ENDIF
        IF D<>6           
          C=0:IF D>5 THEN C=RAND(3)+1
          FOR M=1 TO 8    
             REPEAT       
                 A=RAND(8)+1
                 N=M
                 IF N=C THEN N=RAND(8)+1
                 'ON D EXEC BACKGROUND_SET_1,BACKGROUND_SET_2,S_5870,S_5880,S_5890,BACKGROUND_SET_1,BACKGROUND_SET_1,S_5870,S_5890

                 IF D=1
                    EXEC BACKGROUND_SET_1 
                 ELIF D=2
                    EXEC BACKGROUND_SET_2
                 ELIF D=3
                    EXEC BACKGROUND_SET_3
                 ELIF D=4                    
                    EXEC BACKGROUND_SET_4
                 ELIF D=5
                    EXEC BACKGROUND_SET_5
                 ELIF D=6
                    EXEC BACKGROUND_SET_1
                 ELIF D=7                    
                    EXEC BACKGROUND_SET_1
                 ELIF D=8                    
                    EXEC BACKGROUND_SET_3
                 ELIF D=9                    
                    EXEC BACKGROUND_SET_5              
                ENDIF
             UNTIL PEEK($B277+A)=0
             POKE $B277+A,B
          NEXT M          
        ELSE              
          IF D=6          
            A=1+RAND(8):C=1+RAND(3):POKE 712,22:IF A=C THEN C=1+RAND(8)
            FOR N=1 TO 8  
              POKE 45687+N,A+A*16
              IF N=3 THEN POKE 45687+N,C+A*16
              IF N=7 THEN POKE 45687+N,A+C*16
            NEXT N        
          ENDIF           
        ENDIF           
        MOVE $B278,$B280,40
        DPOKE 45728,0
        DPOKE 45730,0
        FOR N=45700 TO 45715
          POKE N,PEEK(N)&15
        NEXT N
        FOR N=45720 TO 45723
          POKE N,PEEK(N)&240
        NEXT N
ENDPROC 
'------------------------------
PROC BACKGROUND_SET_1 
  POKE 712,158
  B=N+C*16
  IF A&1=1 THEN B=N*16+C
ENDPROC  
'------------------------------ 
PROC BACKGROUND_SET_2
    POKE 712,220
    B=N+N*16
ENDPROC
'------------------------------
PROC BACKGROUND_SET_3   
    POKE 712,86
    B=N+16*C
    IF A>3 THEN B=N*16+C
ENDPROC
'------------------------------
PROC BACKGROUND_SET_4   
    POKE 712,40
    B=N+((4+N MOD 8)+1)*16
ENDPROC
'------------------------------
PROC BACKGROUND_SET_5
    POKE 712,158
    B=N+16*C
    IF A&2=2 THEN B=N*16+C
ENDPROC 
'------------------------------                        

