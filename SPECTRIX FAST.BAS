        DIM Room_Screen_Data$(15)
        Room_Screen_Data$(0)="                    0001                    "
        Room_Screen_Data$(1)="0010 0111 0100 0011 1110 0101 0010 0111 0101"
        Room_Screen_Data$(2)="0001 1011 0101 1011 0100 1011 0100 1001 1001"
        Room_Screen_Data$(3)="1001 1001 1010 1110 0100 1001 0010 1100 1001"
        Room_Screen_Data$(4)="1001 1010 0111 0100 0001 1011 0110 0111 1101"
        Room_Screen_Data$(5)="1010 0101 1001 0011 1100 1001 0001 1001 1000"
        Room_Screen_Data$(6)="0001 1011 1110 1111 0110 1111 1101 1011 0101"
        Room_Screen_Data$(7)="1011 1101 0001 1011 0101 1001 1000 1001 1001"
        Room_Screen_Data$(8)="1001 1000 1001 1000 1000 1000 0010 1101 1000"
        Room_Screen_Data$(9)="1010 0110 1110 0100 0010 0110 0110 1110 0100"
        DIM CX(32)
        DIM CY(32)
        DIM C1(32)
        HISC=0
        SC=0
        CURRENT_ROOM$=""        
      DO
          EXEC TITLE_SCREEN
          GRAPHICS 27          
          A=20
          B=20
          RN=0
          RX=5
          RY=0
          TR=0
          TE$="0"
          LE$="0"
          RE$="0"
          BE$="0"
          ENEMY_TYPE=0
          C=0
          D=0
          E=0
          F=0
          CM=0
          MX=0
          CDX=0
          CDY=0
          CS%=0.2
          
          ' Instead of fractional enemy movement, 
          ' enemy will when a countdown reaches 0, and resets. 
          ' This is to allow the enemy variables to be integers
          
          
          ENEMY_MOVE_NOW = 0
          ENEMY_MOVE_RESET = 4
          ENEMY_MOVE_MULTIPLY = 0
          EXEC AT_2000
          EXIT_FLAG = 0          
          EXEC DRAW_ROOM_SCREEN
          REPEAT

              IF EXIT_FLAG>=4 AND EXIT_FLAG<=8 
                EXEC DRAW_ROOM_SCREEN
                EXIT_FLAG = 0
              ENDIF
              REPEAT
                  X=(-(STICK(0)=11)+(STICK(0)=7))
                  Y=(-(STICK(0)=14)+(STICK(0)=13))
                  COLOR 0
                  PLOT A,B-4
                  DRAWTO A,B+5
                  IF RN=1
                    EXEC AT_220
                  ENDIF
                  A=A+X*5
                  B=B+Y*12
                  
                  IF B<15 AND TE$="1" THEN EXIT_FLAG = 4
                  IF A<8 AND LE$="1" THEN EXIT_FLAG = 5 
                  IF A>72 AND RE$="1" THEN EXIT_FLAG = 6
                  IF B>166 AND BE$="1" THEN EXIT_FLAG = 7
                                    
                  IF A<8 OR A>72 THEN A=A-X*5
                  IF B<15 OR B>166 THEN B=B-Y*12
                  COLOR 1
                  PLOT A,B-4
                  PLOT A,B-3
                  COLOR 11
                  DRAWTO A,B
                  COLOR 8
                  DRAWTO A,B+5
                  IF ENEMY_TYPE=1 
                    EXEC AT_250
                  ELIF ENEMY_TYPE=2 
                    EXEC AT_300
                  ELIF ENEMY_TYPE=3 
                    EXEC AT_350
                  ELIF ENEMY_TYPE=4 
                    EXEC AT_400
                  ELIF ENEMY_TYPE=5 
                    EXEC AT_450
                  ENDIF
    
                IF STRIG(0)=0 THEN EXEC AT_500                
                IF X<>0 OR Y<>0  
                    SOUND 1,240,4,8
                    EXEC AT_200
                    SOUND 1,0,0,0
                ENDIF
                IF TR=2 AND A>32 AND A<48 AND B>80 AND B<100 
                  EXIG_FLAG = 3
                ENDIF
              UNTIL EXIT_FLAG > 0
              IF EXIT_FLAG = 1
                EXEC SHOOT_ENEMY_LASER              
              ENDIF
              IF EXIT_FLAG = 2 
                EXEC CHARACTER_DIE
              ENDIF
              IF EXIT_FLAG = 3
                EXEC GOT_SPECTRIX
              ENDIF
              IF EXIT_FLAG = 11
                EXEC SHOW_GAME_OVER
              ENDIF
          UNTIL EXIT_FLAG>0 AND EXIT_FLAG<=3 
      LOOP
      
PROC AT_200
    COLOR 11:PLOT A,B-2:DRAWTO A-2,B+2:PLOT A,B-2:DRAWTO A+2,B+2:RN=1:COLOR 0:PLOT A,B+5:DRAWTO A,B+1:COLOR 8
    DRAWTO A+1,B+5:PLOT A,B+1:DRAWTO A-1,B+5
ENDPROC

PROC AT_220 
    COLOR 0
    PLOT A,B-2
    DRAWTO A-2,B+2
    PLOT A,B-2
    DRAWTO A+2,B+2
    RN=0
    PLOT A,B+5
    DRAWTO A,B+1
    DRAWTO A+1,B+5
    PLOT A,B+1:DRAWTO A-1,B+5
ENDPROC 

PROC AT_250
    COLOR 0
    PLOT C,D:DRAWTO C+2,D-1
    PLOT C,D:DRAWTO C-2,D-1
    PLOT C,D+1:DRAWTO C+4,D+2+CM
    PLOT C,D+1:DRAWTO C-4,D+2+CM
    CM=CM+MX
    IF CM<0 OR CM>5 THEN MX=-MX
    IF C>A-4 AND C<A+4 AND D>B-8 AND D<B+8 THEN EXIT_FLAG = 2
    IF C<A THEN C=C+INT(2*CS%)
    IF C>A THEN C=C-INT(2*CS%)
    IF D>B THEN D=D-INT(4*CS%)
    IF D<B THEN D=D+INT(4*CS%)
    COLOR 5
    PLOT C,D:DRAWTO C+2,D-1
    PLOT C,D:DRAWTO C-2,D-1
    COLOR 14
    PLOT C,D+1:DRAWTO C+4,D+2+CM
    PLOT C,D+1:DRAWTO C-4,D+2+CM 
ENDPROC

PROC AT_300
    COLOR 0
    PLOT C-1,D-3:DRAWTO C+1,D-3
    PLOT C-1,D-2:DRAWTO C+1,D-2
    COLOR 0
    PLOT C,D:DRAWTO C,D+4
    COLOR 0
    PLOT C+1,D+1:DRAWTO C+4,D+4
    PLOT C-1,D+1:DRAWTO C-4,D+4:DRAWTO C+4,D+4
    IF C<A THEN C=C+INT(1*CS%)
    IF C>A THEN C=C-INT(1*CS%)
    IF D>B THEN D=D-INT(3*CS%)
    IF D<B THEN D=D+INT(3*CS%)
    COLOR 4
    PLOT C-1,D-3:DRAWTO C+1,D-3
    PLOT C-1,D-2:DRAWTO C+1,D-2
    COLOR 1
    PLOT C,D:DRAWTO C,D+4
    COLOR 13
    PLOT C+1,D+1:DRAWTO C+4,D+4
    PLOT C-1,D+1:DRAWTO C-4,D+4
    COLOR 7
    DRAWTO C+4,D+4
    IF C-8<A AND C+8>A AND D-24<B AND D+24>B 
        EXIT_FLAG = 2
    ENDIF
ENDPROC 


PROC AT_350
     COLOR 3
     PLOT C,D:DRAWTO C,D+6
     COLOR 14
     PLOT C+2,D+2:DRAWTO C+6,D+6
     PLOT C-2,D+2:DRAWTO C-6,D+6
     COLOR 4
     PLOT C-2,D-5:DRAWTO C+2,D-5
     COLOR 6
     PLOT C-3,D-4:DRAWTO C+3,D-4
     COLOR 8
     PLOT C-3,D-3:DRAWTO C+3,D-3
     PLOT C-3,D-2:DRAWTO C+3,D-2
     COLOR 6
     PLOT C-3,D-1:DRAWTO C+3,D-1
     COLOR 4
     PLOT C-2,D:PLOT C-1,D:PLOT C+1,D:PLOT C+2,D
     COLOR 15
     PLOT C+6,D+6:DRAWTO C-6,D+6

     IF X=0 AND Y=0 THEN MX=MX+1
     IF MX>10-INT(CS%*4.5) THEN EXIT_FLAG=1
     IF X<>0 OR Y<>0 THEN MX=0
ENDPROC 

PROC AT_400
    COLOR 0
    PLOT C,D:DRAWTO C+5,D+10:DRAWTO C,D+4:DRAWTO C-5,D+10:DRAWTO C,D
    PLOT C,D+1:DRAWTO C+3,D+12
    PLOT C,D+1:DRAWTO C-3,D+12
    IF C<A THEN C=C+INT(4*CS%)
    IF C>A THEN C=C-INT(4*CS%)
    IF D+8>B THEN D=D-INT(8*CS%)
    IF D+8<B THEN D=D+INT(8*CS%)
    IF C>A-4 AND C<A+4 AND D>B-10 AND D<B+10    
      EXIT_FLAG = 2
    ENDIF    
    COLOR 12:PLOT C,D:DRAWTO C+5,D+10:DRAWTO C,D+4:DRAWTO C-5,D+10:DRAWTO C,D:COLOR 9:PLOT C,D+1:DRAWTO C+3,D+12
    PLOT C,D+1:DRAWTO C-3,D+12
ENDPROC      

PROC AT_450
      COLOR 0
      PLOT C-1,D-3:DRAWTO C+1,D-3
      PLOT C-2,D-2:DRAWTO C+2,D-2
      PLOT C-2,D-1:DRAWTO C+2,D-1
      PLOT C-2,D:DRAWTO C+2,D
      PLOT C-2,D+1:DRAWTO C+2,D+1
      PLOT C-1,D+2:DRAWTO C+1,D+2
      C=C+CDX*INT(2+CS%*4)
      D=D+CDY
      IF D<20 OR D>160 
          D=D-CDY:CDY=-CDY
          FOR N=15 TO 0 STEP -1
            SOUND 0,8,10,N
          NEXT N
      ENDIF
      IF C<15 OR C>65 
        C=C-CDX
        CDX=-CDX
        FOR N=14 TO 0 STEP -2
            SOUND 0,16,14,N
        NEXT N
        SOUND 0,0,0,0
      ENDIF
      COLOR 7
      PLOT C-1,D-3:DRAWTO C+1,D-3
      COLOR 9
      PLOT C-2,D-2:DRAWTO C+2,D-2
      COLOR 11
      PLOT C-2,D-1:DRAWTO C+2,D-1
      COLOR 11
      PLOT C-2,D:DRAWTO C+2,D
      COLOR 9
      PLOT C-2,D+1:DRAWTO C+2,D+1
      COLOR 7
      PLOT C-1,D+2:DRAWTO C+1,D+2
      IF C>A-4 AND C<A+4 AND D>B-10 AND D<B+10    
        EXIT_FLAG = 2
      ENDIF    

ENDPROC 

PROC AT_500
      FOR N=15 TO 0 STEP -1
          SOUND 1,255-N*16,10,N
      NEXT N
      SOUND 1,0,0,0
      IF X=0 AND Y=0 THEN EXIT
      CF=0
      FOR M=0 TO 3
          COLOR 11
          IF X<>0
            PLOT A,B-1
            DRAWTO A+X*2,B-1
          ENDIF
          IF Y<>0 
            PLOT A+1,B-1
            DRAWTO A+1,B-1+Y*3
          ENDIF
          COLOR 2+M*4
          IF A+5>C AND A-5<C AND Y<>0 AND ENEMY_TYPE>0 
              DRAWTO A+1,INT(D):CF=1
              EXEC AT_570
          ELIF B+9>D AND B-9<D AND X<>0 AND ENEMY_TYPE>0
              DRAWTO C,B-1
              CF=1
              EXEC AT_570
          ELSE
            IF Y<>0 THEN DRAWTO A+1,90+Y*86
            IF X<>0 THEN DRAWTO 40+X*36,B-1
          ENDIF
      NEXT M
      SETCOLOR 4,0,0
      SETCOLOR 4,0,8
      EXEC AT_570
ENDPROC

PROC AT_570
    COLOR 0
    IF Y<>0
      PLOT A+1,B-1
      DRAWTO A+1,90+Y*86
    ENDIF    
    IF X<>0
      PLOT A+X,B-1
      DRAWTO 40+X*36,B-1
    ENDIF
    IF CF=1  
        CF=0
        EXEC AT_600
    ENDIF
    X=0:Y=0
ENDPROC 


PROC AT_600
    COLOR 4
    PLOT C,D
    FOR N=0 TO 63
        COLOR INT(RND()*16)
        PLOT C,D
        DRAWTO C+INT(RND()*16)-8,D+INT(RND()*14)-7
        SOUND 3,255,4,INT(15-N/4)
    NEXT N
    COLOR 0
    FOR N=-14 TO 14
        PLOT C-8,D+N
        DRAWTO C+8,D+N
      NEXT N
      ENEMY_TYPE=0
ENDPROC

PROC SHOOT_ENEMY_LASER
    FOR N=15 TO 0 STEP -1
      SOUND 1,N*10,2,N
    NEXT N
    COLOR 8
    PLOT C,D
    DRAWTO A,B
    PAUSE 10
ENDPROC

PROC GOT_SPECTRIX
    GRAPHICS 0
    SETCOLOR 2,12,4
    PRINT "   YOU HAVE FOUND THE SPECTRIX!     "
    FOR N=0 TO 255 STEP 4:SOUND 0,N,6,10:SOUND 1,N,10,10:SOUND 2,N,14,10:NEXT N:? :? :? :SOUND 2,0,0,0:SOUND 1,0,0,0
    CLS
    PRINT "   NOW RETURN HOME TO RECIEVE CREDIT      AND TO GO ON TO THE NEXT ROUND."
    ? :? :? "   MOVE JOYSTICK TO CONTINUE PLAY."
    REPEAT
    UNTIL NOT STICK(0)=15 AND NOT STRIG(0)=1 
    EXIT_FLAG=0
    TR=3
ENDPROC 


PROC RETURN_WITH_SPECTRIX
    GRAPHICS 0
    SETCOLOR 2,14,6
    PRINT "    YOU HAVE RETURNED HOME SAFELY               WITH THE SPECTRIX!        ":? 
    SC=SC+1
    ? :? :? :? " YOU HAVE FOUND ";SC;" SPECTRIXES."
    ? :? :? :? :? :? "  PRESS FIRE BUTTON TO GO ON TO THE           NEXT ROUND OF PLAY."    
    REPEAT
    UNTIL STRIG(0)=1 
ENDPROC 

PROC CHARACTER_DIE
    FOR N=12 TO 0 STEP -1
        COLOR 0
        SETCOLOR 4,4,N
        PLOT A-4,B-12+N*2:DRAWTO A+4,B-6+4
        SOUND 3,N*5,10,14-INT(N/4)
        PAUSE 1
    NEXT N
ENDPROC


PROC DRAW_ROOM_SCREEN
      REPEAT
          IF EXIT_FLAG = 5 
            A=70
            RX=RX-1
          ELIF EXIT_FLAG = 6 
            A=8
            RX=RX+1
          ELIF EXIT_FLAG = 4 
            B=155
            RY=RY-1
          ELIF EXIT_FLAG = 7 
            B=15
            RY=RY+1
          ENDIF
          IF TR=3 AND RX=5 AND RY=0 
              EXEC RETURN_WITH_SPECTRIX
              EXEC AT_2000
              TR=1
          ENDIF
        UNTIL TR<=2 OR RX<>5 OR RY>0
        CURRENT_ROOM$=Room_Screen_Data$(RY)
        RM=(RX-1) * 5
        TE$=CURRENT_ROOM$[RM+1,1]
        LE$=CURRENT_ROOM$[RM+2,1]
        RE$=CURRENT_ROOM$[RM+3,1]
        BE$=CURRENT_ROOM$[RM+4,1]
        
        GRAPHICS 27
        SETCOLOR 4,0,0
        COLOR 10
  
        IF TE$="0" 
            PLOT 0,0
            DRAWTO 79,0
            PLOT 1,2
            DRAWTO 79,2
        ENDIF
        IF BE$="0" 
            PLOT 0,179
            DRAWTO 79,179
            PLOT 1,177
            DRAWTO 79,177
        ENDIF
        IF LE$="0" 
            PLOT 0,0
            DRAWTO 0,179
            PLOT 2,0
            DRAWTO 2,179
        ENDIF
        IF RE$="0" 
          PLOT 79,0
          DRAWTO 79,179
          PLOT 77,0
          DRAWTO 77,179
        ENDIF
        
        COLOR 3
        PLOT 0,0
        DRAWTO 3,3
        PLOT 79,0
        DRAWTO 77,3
        PLOT 0,179
        DRAWTO 3,177
        PLOT 79,179
        DRAWTO 77,177
        
        ENEMY_TYPE=0
        FOR N=0 TO 32
            IF RX=CX(N) AND RY=CY(N)
                ENEMY_TYPE=C1(N)
                C=40
                D=90
            ENDIF
        NEXT N
  
       IF ENEMY_TYPE=1 THEN MX=2:CM=2
       IF ENEMY_TYPE=2 THEN MX=0
       IF ENEMY_TYPE=3 THEN MX=0
       IF ENEMY_TYPE=5 
          CDX=1
          CDY=INT(RND()*5)+1
       ENDIF
       IF TR<=2 
         IF TR=2 THEN TR=1
         IF RX=E AND RY=F  
            FOR N=0 TO 15
              COLOR N
              PLOT 48-N,82
              DRAWTO 32+N,98
              PLOT 32,82+N
              DRAWTO 48,98-N
            NEXT N
         ENDIF
         IF RX=E AND RY=F 
            TR=2
            ENEMY_TYPE=0
         ENDIF
       ENDIF        

    IF RX=5 AND RY=0  
      COLOR 15
      PLOT 3,1
      DRAWTO 77,1
    ENDIF
    
   FOR N=0 TO 240 STEP 16
      SOUND 0,255-N,10,12+INT(N/64)
      SETCOLOR 4,0,INT(N/32)
    NEXT N
ENDPROC 

PROC AT_2000
    
    FOR N=0 TO 32      
      REPEAT
        PRIOR_SAME=0
        CX(N)=RAND(9)+1
        CY(N)=RAND(9)+1
        C1(N)=RAND(5)+1
        IF N>2 AND CX(N)=CX(N-1) AND CY(N)=CY(N-1) THEN PRIOR_SAME=1
      UNTIL PRIOR_SAME=0         
    NEXT N
    POKE 712,202
    E=RAND(9)+1
    F=RAND(7)+3
    TR=1
    CS%=CS%+0.2
    IF CS%>2 THEN C=2
ENDPROC 

PROC SHOW_GAME_OVER
    GRAPHICS 18
    POSITION 15,4
    PRINT #6,"GAME OVER"
    FOR M=0 TO 9
      FOR N=15 TO 0 STEP -1
        SETCOLOR 2,13,N
      NEXT N
      FOR N=0 TO 15 STEP 1
        SETCOLOR 2,13,N
      NEXT N
    NEXT M
    POSITION 10,8
    SETCOLOR 3,0,0
    PRINT #6,"thankyou for playing"
    FOR N=0 TO 15
      SETCOLOR 3,10,N
      PAUSE 1
    NEXT N
    IF SC>HISC THEN HISC=SC
    PAUSE 120
ENDPROC
'------------------------------
PROC TITLE_SCREEN
  REPEAT
        P=0
        IF P<16
          GRAPHICS 17
          SETCOLOR 0,11,8
          SETCOLOR 2,13,6
          SETCOLOR 3,12,10
          POSITION 1,6
          PRINT #6,"    SEARCH OF THE         SPECTRIX "
          POSITION 1,14
          PRINT #6,"     CREATED BY        peter j. meyer"
          POSITION 1,16
          PRINT #6,"     copyright           1986,2020   "
          EXEC TITLE_SCREEN_HOLD
        ENDIF
        IF P<16 
        GRAPHICS 0
        SETCOLOR 2,0,0
        SETCOLOR 1,0,12
        SETCOLOR 4,0,4   
        POKE 82,0
          CLS
          ? :? :? :PRINT "     YOUR SCORE - ";SC
          ? :PRINT "     HIGH SCORE - ";HISC
          ? :? :? "PRESS BUTTON ON JOYSTICK TO START GAME."
          ' ? :? :? :? :? :? :? :? :? :? :? "     (C) Copyright 1986 __________"
          EXEC TITLE_SCREEN_HOLD
        ENDIF
        IF P<16
          GRAPHICS 27
          FOR N=0 TO 15:COLOR N:PLOT 48-N,82:DRAWTO 32+N,98:PLOT 32,82+N:DRAWTO 48,98-N:NEXT N
          EXEC TITLE_SCREEN_HOLD
        ENDIF
    UNTIL P=255
ENDPROC
'------------------------------
PROC TITLE_SCREEN_HOLD
  N=120
  REPEAT    
    P=PEEK(53279)&7
    IF STRIG(0)=0 or P&1=0 THEN P=255
    PAUSE 1
    N=N-1
  UNTIL P=255 or P&1=0 or n<0
   
ENDPROC


