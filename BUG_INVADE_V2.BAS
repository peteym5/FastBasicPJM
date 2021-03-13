' TOP LOGIC LOOP'
DATA DIR_X_ADD()       BYTE = 000,001,001,001,000,255,255,255
DATA DIR_Y_ADD()       BYTE = 255,255,000,001,001,001,000,255



        DL_ADDR_SET = 0
        PMBASE = 0
'Define Variables first before using inside the program'
        I = 0
        J = 0
        K = 0
        L = 0
        O = 0
        SCR_ADDR=0
        Current_Level=0
        POKE $D301,PEEK($D301)!2 'Disable Internal Basic
        DPOKE $D407,@PMBANK_HI
        POKE @GRACTL,3
        POKE 623,1
        SCR_ADDR=@GAME_SCREEN_ADDR
        CLS
        POKE 600,122
        POKE 601,197
        POKE 602,2
        POKE 603,2
        POKE 604,46
        POKE 605,202
        POKE 606,183
        POKE 607,229             
        H1=0
        H2=0
        H3=0
        PMV=@PMMOVE   
        Add_New_Bug_CountDown = 24
        Add_New_Bug_Reset = 24
        'Begin the Top Logic Loop
  DO
        'Game Type Remain on screen until user Presses Start or Trigger
        EXEC DISPLAY_TITLE        
        'Array to contain the Moving Bugs Data, Index 0, 1, 2 for the 3 different bugs
        
        DIM BUG_TYPE(2) BYTE
        DIM BUG_XPOS(2) WORD
        DIM BUG_YPOS(2) WORD
        DIM BUG_XDIR(2) WORD
        DIM BUG_YDIR(2) WORD
        DIM BUG_DATA1(2) BYTE
        DIM BUG_DATA2(2) BYTE
        DIM BUG_MOVE_FRACTION(2) WORD
        DIM BUG_MOVE_DELAY(2) WORD
        BUG_INDEX = 0
        SPRITE_2FRAME = 0
        DPOKE 560,@display_list_game
        DPOKE 88,@SCREEN_ADDR
        POKE 756,@CHARSET_GAME_HI    
        Z=0
        Z=USR(@SILENCE)
        DPOKE @VVBLKD,@GAME_VBI

               
        BULLET_X_POS = 0
        BULLET_X_SCREEN = 0
        BULLET_Y_POS = 0
        BULLET_Y_SCREEN = 0
        BULLET_STATUS=0
        DEG 
        Current_Level = 1        
        O=1
        SC=0
        Lives = 7
        Next_Extra_Life_Score = 1000
        REPEAT
            Z=USR(@PMCLEAR,@PMBANK+768,5)
            Z=USR(@SILENCE)
            CLS                
            EXEC DRAW_FIELD
            ' Set the Defaults for the Shooter'
            POKE 600,124
            POKE 601,197
            POKE 704,30
            POKE @PMBANK+512,3
            MOVE @PMBANK+512,@PMBANK+513,7
            POKE @PMBANK+520,0
            MOVE @PMBANK+520,@PMBANK+521,8
            EXEC PRINT_SCORE
            Exit_Flag = 0
            
            '*** Main Game Loop Begins here'
            REPEAT
                    Z=USR(@MOVE_SHOOTER)
                    Z=USR(PMV,1,@P000,12,PEEK(600),PEEK(601))
                    O=NOT O
                    SPRITE_2FRAME = 12 - SPRITE_2FRAME
                    FOR BUG_INDEX = 0 TO 2
                      'Since Fast Basic does not have a case structure, 
                      'Use Block If instead.
                      IF BUG_TYPE(BUG_INDEX) = 1 
                           EXEC BUG_TYPE_BEETLE
                      ELIF BUG_TYPE(BUG_INDEX) = 2
                           EXEC BUG_TYPE_FLEA
                      ELIF BUG_TYPE(BUG_INDEX) = 3
                           EXEC BUG_TYPE_ANT
                      ELIF BUG_TYPE(BUG_INDEX) = 4
                           EXEC BUG_TYPE_TERMITE
                      ELIF BUG_TYPE(BUG_INDEX) = 5
                           EXEC BUG_TYPE_BEE
                      ELIF BUG_TYPE(BUG_INDEX) = 6
                           EXEC BUG_TYPE_ROACH
                      ELIF BUG_TYPE(BUG_INDEX) = 7
                           EXEC BUG_TYPE_FLY
                      ELIF BUG_TYPE(BUG_INDEX) = 8
                           EXEC BUG_TYPE_WASP
                      ELIF BUG_TYPE(BUG_INDEX) = 15
                           EXEC BUG_DIE      
                      ENDIF
                    NEXT BUG_INDEX
                     
                    'Bullet Status. 0 = Not on screen. 1 in Motion 
                    IF BULLET_STATUS = 0
                        IF STRIG(0) = 0                                               
                            'Start the Bullet at the shooter
                            BULLET_STATUS=1
                            BULLET_X_POS =      PEEK(600) +3 
                            BULLET_X_SCREEN =   SCR_ADDR+((PEEK(600)-46) / 4)
                            BULLET_Y_POS =      PEEK(601) - 29
                            BULLET_Y_SCREEN =   ((PEEK(601)-29) / 8)*40                            
                            Z=USR(@START_SOUND,0,1)
                            POKE 53252,BULLET_X_POS
                            MOVE @PMBANK+512,@PMBANK+800+BULLET_Y_POS,16
                            POKE 77,0
                        ENDIF
                    ELSE
                        BULLET_Y_POS = BULLET_Y_POS - 8
                        BULLET_Y_SCREEN = BULLET_Y_SCREEN - 40
                        IF BULLET_Y_SCREEN<0
                            BULLET_STATUS=0
                            MOVE @CHARSET_GAME_ADDR,@PMBANK+800+BULLET_Y_POS,8
                            MOVE @CHARSET_GAME_ADDR,@PMBANK+808+BULLET_Y_POS,8
                        ELSE     
                            POKE 53252,BULLET_X_POS
                            MOVE @PMBANK+512,@PMBANK+800+BULLET_Y_POS,16
                            IF PEEK(BULLET_X_SCREEN+BULLET_Y_SCREEN)>0 THEN EXEC BULLET_PLAYFIELD
                            IF PEEK(53256)&14>0 THEN EXEC BULLET_HIT_BUG
                        ENDIF
                    ENDIF
                    Add_New_Bug_CountDown = Add_New_Bug_CountDown - 1
                    IF Add_New_Bug_CountDown < 0
                      Add_New_Bug_CountDown = Add_New_Bug_Reset 
                    ELIF Add_New_Bug_CountDown < 3
                        IF BUG_TYPE(Add_New_Bug_CountDown) = 0
                          BUG_INDEX = Add_New_Bug_CountDown
                          EXEC DEFINE_NEW_BUG
                          Add_New_Bug_CountDown = 0
                        ENDIF
                    ENDIF                    
            UNTIL Exit_Flag > 0 
      UNTIL Exit_Flag = 2 or Exit_Flag = 3
      IF Exit_Flag = 3
            EXEC DISPLAY_GAME_OVER      
      ENDIF
  LOOP 
'------------------------------
PROC BUG_TYPE_BEETLE ';1  
      BUG_MOVE_DELAY(BUG_INDEX) = BUG_MOVE_DELAY(BUG_INDEX) + BUG_MOVE_FRACTION(BUG_INDEX)
      IF BUG_YPOS(BUG_INDEX)>200 THEN EXEC BUG_GOT_PASSED
      IF BUG_MOVE_DELAY(BUG_INDEX) > 256
          J = 1
          BUG_MOVE_DELAY(BUG_INDEX) = 0          
      ELSE
          J = 0
      ENDIF
      FOR N = 0 TO J
        BUG_XPOS(BUG_INDEX) = BUG_XPOS(BUG_INDEX)+ BUG_XDIR(BUG_INDEX)
      NEXT J          
      IF BUG_XPOS(BUG_INDEX) < 48
        BUG_XPOS(BUG_INDEX) = 48
        BUG_XDIR(BUG_INDEX) = 1
        BUG_YPOS(BUG_INDEX) = BUG_YPOS(BUG_INDEX) + BUG_YDIR(BUG_INDEX) 
      ENDIF
      IF BUG_XPOS(BUG_INDEX) > 200
        BUG_XPOS(BUG_INDEX) = 200
        BUG_XDIR(BUG_INDEX) = -1
        BUG_YPOS(BUG_INDEX) = BUG_YPOS(BUG_INDEX) + BUG_YDIR(BUG_INDEX) 
      ENDIF           
      IF BUG_XDIR(BUG_INDEX) < 0 
          Z=USR(PMV,2+BUG_INDEX,@P004+SPRITE_2FRAME,12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))
      ELSE
          Z=USR(PMV,2+BUG_INDEX,@P002+SPRITE_2FRAME,12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))          
      ENDIF 
ENDPROC
'------------------------------
PROC BUG_TYPE_FLEA 
      IF  BUG_MOVE_DELAY(BUG_INDEX) > 0 AND BUG_YDIR(BUG_INDEX) = 0
          BUG_MOVE_DELAY(BUG_INDEX) = BUG_MOVE_DELAY(BUG_INDEX) - 1
          IF BUG_MOVE_DELAY(BUG_INDEX) = 0 
             BUG_YDIR(BUG_INDEX) = -6
             IF RAND(4) = 0 THEN BUG_XDIR(BUG_INDEX) = RAND(2)*2-1
          ENDIF
          IF BUG_XDIR(BUG_INDEX) < 0 
              Z=USR(PMV,2+BUG_INDEX,@P006,12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))
          ELSE
              Z=USR(PMV,2+BUG_INDEX,@P008,12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))          
          ENDIF 

      ELIF BUG_MOVE_DELAY(BUG_INDEX) = 0 
          IF BUG_YPOS(BUG_INDEX)>200 THEN EXEC BUG_GOT_PASSED 
          BUG_MOVE_DELAY(BUG_INDEX) = 0
          BUG_XPOS(BUG_INDEX) = BUG_XPOS(BUG_INDEX)+ BUG_XDIR(BUG_INDEX) 
          BUG_YPOS(BUG_INDEX) = BUG_YPOS(BUG_INDEX)+ (BUG_YDIR(BUG_INDEX) / 2)
          BUG_YDIR(BUG_INDEX) = BUG_YDIR(BUG_INDEX) + 1
          IF BUG_XPOS(BUG_INDEX) < 48
            BUG_XPOS(BUG_INDEX) = 48
            BUG_XDIR(BUG_INDEX) = 1
            BUG_YPOS(BUG_INDEX) = BUG_YPOS(BUG_INDEX) + 1
          ENDIF
          IF BUG_XPOS(BUG_INDEX) > 200
            BUG_XPOS(BUG_INDEX) = 200
            BUG_XDIR(BUG_INDEX) = -1
            BUG_YPOS(BUG_INDEX) = BUG_YPOS(BUG_INDEX) + 1 
          ENDIF           

          IF BUG_YDIR(BUG_INDEX) > 7 
              BUG_YDIR(BUG_INDEX) = 0 
              BUG_MOVE_DELAY(BUG_INDEX) = BUG_MOVE_FRACTION(BUG_INDEX) + PEEK(53770)&7                               
          ENDIF          

          IF BUG_XDIR(BUG_INDEX) < 0 
              Z=USR(PMV,2+BUG_INDEX,@P007,12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))
          ELSE
              Z=USR(PMV,2+BUG_INDEX,@P009,12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))          
          ENDIF 
      ENDIF
ENDPROC
'------------------------------
PROC BUG_TYPE_ANT
      BUG_MOVE_DELAY(BUG_INDEX) = BUG_MOVE_DELAY(BUG_INDEX) + BUG_MOVE_FRACTION(BUG_INDEX)
      IF BUG_YPOS(BUG_INDEX)>200 THEN EXEC BUG_GOT_PASSED
      IF BUG_MOVE_DELAY(BUG_INDEX) > 256
          J = 1
          BUG_MOVE_DELAY(BUG_INDEX) = 0          
      ELSE
          J = 0
      ENDIF
      FOR N = 0 TO J
        BUG_XPOS(BUG_INDEX) = BUG_XPOS(BUG_INDEX)+ BUG_XDIR(BUG_INDEX)
      NEXT J          
           
      IF BUG_XPOS(BUG_INDEX) < 48
        BUG_XPOS(BUG_INDEX) = 48
        BUG_XDIR(BUG_INDEX) = 1
        BUG_YPOS(BUG_INDEX) = BUG_YPOS(BUG_INDEX) + BUG_YDIR(BUG_INDEX) 
      ENDIF
      IF BUG_XPOS(BUG_INDEX) > 200
        BUG_XPOS(BUG_INDEX) = 200
        BUG_XDIR(BUG_INDEX) = -1
        BUG_YPOS(BUG_INDEX) = BUG_YPOS(BUG_INDEX) + BUG_YDIR(BUG_INDEX) 
      ENDIF
         P=USR(@GET_SCREEN_BYTE,BUG_XPOS(BUG_INDEX),BUG_YPOS(BUG_INDEX))
         IF P > 0
           BUG_XPOS(BUG_INDEX) = BUG_XPOS(BUG_INDEX) - BUG_XDIR(BUG_INDEX)
           BUG_XDIR(BUG_INDEX) =- BUG_XDIR(BUG_INDEX)
           IF RAND(3) = 0
             IF BUG_YPOS(BUG_INDEX) > 2 THEN BUG_YPOS(BUG_INDEX) = BUG_YPOS(BUG_INDEX) - 1             
           ELSE
             BUG_YPOS(BUG_INDEX) = BUG_YPOS(BUG_INDEX) + BUG_YDIR(BUG_INDEX)
           ENDIF            
         ENDIF  
          
      IF BUG_XDIR(BUG_INDEX) < 0 
          Z=USR(PMV,2+BUG_INDEX,@P012+SPRITE_2FRAME,12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))
      ELSE
          Z=USR(PMV,2+BUG_INDEX,@P010+SPRITE_2FRAME,12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))          
      ENDIF 

ENDPROC
'------------------------------
PROC BUG_TYPE_TERMITE
      BUG_MOVE_DELAY(BUG_INDEX) = BUG_MOVE_DELAY(BUG_INDEX) + BUG_MOVE_FRACTION(BUG_INDEX)
      IF BUG_YPOS(BUG_INDEX)>200 THEN EXEC BUG_GOT_PASSED
      IF BUG_MOVE_DELAY(BUG_INDEX) > 256
          J = 1
          BUG_MOVE_DELAY(BUG_INDEX) = 0          
      ELSE
          J = 0
      ENDIF
      FOR N = 0 TO J
          BUG_XPOS(BUG_INDEX) = BUG_XPOS(BUG_INDEX)+ BUG_XDIR(BUG_INDEX) 
          IF BUG_XPOS(BUG_INDEX)&4=0 THEN  
          P=USR(@GET_SCREEN_BYTE,BUG_XPOS(BUG_INDEX),BUG_YPOS(BUG_INDEX))
          IF P > 0
                IF RAND(2) = 1 THEN BUG_XPOS(BUG_INDEX) = BUG_XPOS(BUG_INDEX) - BUG_XDIR(BUG_INDEX)
                IF BUG_YPOS(BUG_INDEX) > 2 THEN BUG_YPOS(BUG_INDEX) = BUG_YPOS(BUG_INDEX)-1-RAND(2)
          ELSE
                I=2+RAND(3)
                IF BUG_XPOS(BUG_INDEX) & 3 > 0 THEN BUG_YPOS(BUG_INDEX) = BUG_YPOS(BUG_INDEX)+RAND(I)
          ENDIF  
      NEXT N
      IF BUG_XPOS(BUG_INDEX) < 48
        BUG_XPOS(BUG_INDEX) = 48
        BUG_XDIR(BUG_INDEX) = 1 
      ENDIF
      IF BUG_XPOS(BUG_INDEX) > 200
        BUG_XPOS(BUG_INDEX) = 200
        BUG_XDIR(BUG_INDEX) = -1 
      ENDIF
      IF BUG_XDIR(BUG_INDEX) < 0 
          Z=USR(PMV,2+BUG_INDEX,@P014+SPRITE_2FRAME,12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))
      ELSE
          Z=USR(PMV,2+BUG_INDEX,@P016+SPRITE_2FRAME,12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))          
      ENDIF 

ENDPROC 
'------------------------------
PROC BUG_TYPE_BEE
      BUG_MOVE_DELAY(BUG_INDEX) = BUG_MOVE_DELAY(BUG_INDEX) + BUG_MOVE_FRACTION(BUG_INDEX)
      IF BUG_YPOS(BUG_INDEX)>200 THEN EXEC BUG_GOT_PASSED
      IF BUG_MOVE_DELAY(BUG_INDEX) > 256  
          BUG_MOVE_DELAY(BUG_INDEX) = 0
          J = 1
      ELSE
          J = 0
      ENDIF
      FOR N = 0 TO J
          BUG_XPOS(BUG_INDEX) = BUG_XPOS(BUG_INDEX) + BUG_XDIR(BUG_INDEX)
      NEXT N
      IF BUG_XPOS(BUG_INDEX) < 28 OR BUG_XPOS(BUG_INDEX) > 228
          POKE 53249+BUG_INDEX,0
          BUG_TYPE(BUG_INDEX) = 0
      ELSE
          BUG_YDIR(BUG_INDEX) = RAND(3) - 1
          BUG_YPOS(BUG_INDEX) = BUG_YPOS(BUG_INDEX) + BUG_YDIR(BUG_INDEX)
          IF BUG_YPOS(BUG_INDEX) < 20 THEN BUG_YPOS(BUG_INDEX) = 20
          IF BUG_YPOS(BUG_INDEX) > 156 THEN BUG_YPOS(BUG_INDEX) = 156          
      ENDIF

      IF BUG_XDIR(BUG_INDEX) < 0 
          Z=USR(PMV,2+BUG_INDEX,@P044+SPRITE_2FRAME,12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))
      ELSE
          Z=USR(PMV,2+BUG_INDEX,@P042+SPRITE_2FRAME,12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))          
      ENDIF 
    
ENDPROC 
'------------------------------
PROC BUG_TYPE_ROACH
      BUG_MOVE_DELAY(BUG_INDEX) = BUG_MOVE_DELAY(BUG_INDEX) + BUG_MOVE_FRACTION(BUG_INDEX)
      IF BUG_YPOS(BUG_INDEX)>200 THEN EXEC BUG_GOT_PASSED
       IF BUG_MOVE_DELAY(BUG_INDEX) > 256  
           BUG_MOVE_DELAY(BUG_INDEX) = 0
           J = 2
       ELSE
           J = 1
      ENDIF
      BUG_YPOS(BUG_INDEX) = BUG_YPOS(BUG_INDEX)+J         
     IF PEEK(53770)>127 
       BUG_XDIR(BUG_INDEX) = -1 + RAND(3)
       BUG_XPOS(BUG_INDEX) = BUG_XPOS(BUG_INDEX)  + BUG_XDIR(BUG_INDEX) 
     ENDIF
     IF BUG_XPOS(BUG_INDEX) < 48
       BUG_XPOS(BUG_INDEX) = 48
     ENDIF
     IF BUG_XPOS(BUG_INDEX) > 200
       BUG_XPOS(BUG_INDEX) = 200
     ENDIF
     Z=USR(PMV,2+BUG_INDEX,@P018+SPRITE_2FRAME,12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))
ENDPROC
'------------------------------
PROC BUG_TYPE_FLY 
      BUG_MOVE_DELAY(BUG_INDEX) = BUG_MOVE_DELAY(BUG_INDEX) + BUG_MOVE_FRACTION(BUG_INDEX)
      IF BUG_YPOS(BUG_INDEX)>200 THEN EXEC BUG_GOT_PASSED
      I = BUG_XDIR(BUG_INDEX)
      IF BUG_MOVE_DELAY(BUG_INDEX) > 256
          J = 1
          BUG_MOVE_DELAY(BUG_INDEX) = 0          
      ELSE
          J = 0
      ENDIF
      FOR N = 0 TO J
        BUG_XPOS(BUG_INDEX) = (BUG_XPOS(BUG_INDEX) + DIR_X_ADD(I))&255
        BUG_YPOS(BUG_INDEX) = (BUG_YPOS(BUG_INDEX) + DIR_Y_ADD(I))&255
      NEXT N      
      Z=USR(PMV,2+BUG_INDEX,@P022+SPRITE_2FRAME+24*BUG_XDIR(BUG_INDEX),12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))
            
      IF SPRITE_2FRAME = 0
            BUG_XDIR(BUG_INDEX) = (BUG_XDIR(BUG_INDEX) + BUG_YDIR(BUG_INDEX)) & 7
      ELSE     
            I = BUG_XDIR(BUG_INDEX)
            IF I = 0  
                  IF BUG_YDIR(BUG_INDEX) = 0 THEN BUG_YDIR(BUG_INDEX) = - 1 +RAND(2) * 2
            ELSE
                  IF BUG_YPOS(BUG_INDEX) < 4 + RAND(8)
                      IF BUG_YPOS(BUG_INDEX) < 3 THEN BUG_YPOS(BUG_INDEX) = BUG_YPOS(BUG_INDEX) + 1
                      IF I=6 OR I=7 THEN BUG_YDIR(BUG_INDEX) = -1 
                      IF I=1 OR I=2 THEN BUG_YDIR(BUG_INDEX) = 1
                  ELIF BUG_XPOS(BUG_INDEX) > 55 AND BUG_XPOS(BUG_INDEX) < 192  
                      IF RAND(3) = 0 
                          IF BUG_YDIR(BUG_INDEX) = 0 THEN BUG_YDIR(BUG_INDEX) = - 1 + RAND(2) * 2                  
                      ELSE   
                          BUG_YDIR(BUG_INDEX) = 0
                      ENDIF
                  ELIF BUG_XPOS(BUG_INDEX) < 56 
                      IF I = 6 
                        BUG_YDIR(BUG_INDEX) = - 1 + RAND(2) * 2
                      ELIF I = 7 
                        BUG_YDIR(BUG_INDEX) = 1
                      ELIF I = 5            
                        BUG_YDIR(BUG_INDEX) = - 1
                      ENDIF
                  ELIF BUG_XPOS(BUG_INDEX) > 191
                      IF I = 2 
                        BUG_YDIR(BUG_INDEX) = - 1 + RAND(2) * 2
                      ELIF I = 3 
                        BUG_YDIR(BUG_INDEX) = 1
                      ELIF I = 1           
                        BUG_YDIR(BUG_INDEX) = - 1
                      ENDIF               
                  ENDIF 
            ENDIF           
      ENDIF
ENDPROC 
'------------------------------
PROC BUG_TYPE_WASP
      BUG_MOVE_DELAY(BUG_INDEX) = BUG_MOVE_DELAY(BUG_INDEX) + BUG_MOVE_FRACTION(BUG_INDEX)
      IF BUG_YPOS(BUG_INDEX)>200 THEN EXEC BUG_GOT_PASSED
      IF BUG_MOVE_DELAY(BUG_INDEX) > 256
          J = 1
          BUG_MOVE_DELAY(BUG_INDEX) = 0          
      ELSE
          J = 0
      ENDIF
      FOR N = 0 TO J
          BUG_XPOS(BUG_INDEX) = BUG_XPOS(BUG_INDEX) + BUG_XDIR(BUG_INDEX)
          BUG_YPOS(BUG_INDEX) = BUG_YPOS(BUG_INDEX) + 1 
      NEXT N        
      IF BUG_XPOS(BUG_INDEX) < 48
        BUG_XPOS(BUG_INDEX) = 48
        BUG_XDIR(BUG_INDEX) = 1 
      ENDIF
      IF BUG_XPOS(BUG_INDEX) > 200
        BUG_XPOS(BUG_INDEX) = 200
        BUG_XDIR(BUG_INDEX) = -1 
      ENDIF
      IF RAND(8)=0 AND SPRITE_2FRAME = 0
        BUG_XDIR(BUG_INDEX) = -BUG_XDIR(BUG_INDEX) 
      ENDIF  
      IF BUG_XDIR(BUG_INDEX) < 0 
          Z=USR(PMV,2 + BUG_INDEX,@P038 + SPRITE_2FRAME,12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))
      ELSE
          Z=USR(PMV,2 + BUG_INDEX,@P040 + SPRITE_2FRAME,12,BUG_XPOS(BUG_INDEX),32+BUG_YPOS(BUG_INDEX))          
      ENDIF 
ENDPROC
'------------------------------
PROC BUG_DIE
    POKE 705+BUG_INDEX,0
    POKE 53249+BUG_INDEX,0
    Z=USR(PMV,2 + BUG_INDEX,@CHARSET_GAME_ADDR,8,0,0)
    BUG_TYPE(BUG_INDEX) = 0
ENDPROC
'------------------------------
'When Bullet hits Flower or Log, it changes the playfield graphics.'

PROC BULLET_PLAYFIELD
    MOVE @CHARSET_GAME_ADDR,@PMBANK+800+BULLET_Y_POS,8
    MOVE @CHARSET_GAME_ADDR,@PMBANK+808+BULLET_Y_POS,8
    G1=PEEK(BULLET_X_SCREEN+BULLET_Y_SCREEN)&127
    G2=PEEK(BULLET_X_SCREEN+BULLET_Y_SCREEN-40)&127
    G3=PEEK(BULLET_X_SCREEN+BULLET_Y_SCREEN-80)&127
    G4=PEEK(BULLET_X_SCREEN+BULLET_Y_SCREEN+1)&127
    G5=PEEK(BULLET_X_SCREEN+BULLET_Y_SCREEN-39)&127
    G6=PEEK(BULLET_X_SCREEN+BULLET_Y_SCREEN-1)&127
    G7=PEEK(BULLET_X_SCREEN+BULLET_Y_SCREEN-41)&127
   Z=USR(@START_SOUND,1,1)
        
    IF G1=67 AND (G2=65 OR G2=66) AND G3<>65 AND G3<>194
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN-40,0
    ENDIF
    IF G1=67 AND (G2=65 OR G2=66) AND (G3=65 OR G3=66)
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN-40,67
    ENDIF
    IF G1=67 AND ((G4>68 AND G4<72) OR (G6>67 AND G6<71)) 
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,69+RAND(2)
    ENDIF        
    IF G1=67 AND G4>68 AND G4<72 AND G6=0 
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,68
    ENDIF
    IF G1=67 AND G4=0 AND G6>67 AND G6<71 
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,71
    ENDIF    
    IF G1=71 AND G6=0
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
    ENDIF
    IF G1=71 AND G4=0
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
    ENDIF
    IF G1=68 AND G6=0
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
    ENDIF
    IF G1=68 AND G4=0
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
    ENDIF
    IF G1=68 AND (G4=69 OR G4=70) 
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
        POKE BULLET_X_SCREEN+1+BULLET_Y_SCREEN,68
    ENDIF
    IF G1=71 AND (G6=69 OR G6=70) 
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
        POKE BULLET_X_SCREEN-1+BULLET_Y_SCREEN,71
    ENDIF
    IF G1=68 AND G4=71 
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
        POKE BULLET_X_SCREEN+1+BULLET_Y_SCREEN,0
    ENDIF
    IF G1=71 AND G6=68 
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
        POKE BULLET_X_SCREEN-1+BULLET_Y_SCREEN,0
    ENDIF
    IF (G2=65 OR G2=66) AND ((G5>68 AND G5<72) OR (G7>67 AND G7<71)) 
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN-40,69+RAND(2)
    ENDIF         
    IF (G2=65 OR G2=66) AND (G5>68 AND G5<72) AND G7=0 THEN POKE BULLET_X_SCREEN+BULLET_Y_SCREEN-40,68
    IF (G2=65 OR G2=66) AND (G7>67 AND G7<71) AND G5=0 THEN POKE BULLET_X_SCREEN+BULLET_Y_SCREEN-40,71
    IF (G1=69 OR G1=70) AND (G4=69 OR G4=70) AND (G6=69 OR G6=70)
      POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
      POKE BULLET_X_SCREEN-1+BULLET_Y_SCREEN,71
      POKE BULLET_X_SCREEN+1+BULLET_Y_SCREEN,68
    ENDIF 
    IF (G1=69 OR G1=70) AND (G4=69 OR G4=70) AND G6=68  
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
        POKE BULLET_X_SCREEN-1+BULLET_Y_SCREEN,0
        POKE BULLET_X_SCREEN+1+BULLET_Y_SCREEN,68
    ENDIF
    IF (G1=69 OR G1=70) AND G4=71 AND (G6=69 OR G6=70)  
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
        POKE BULLET_X_SCREEN-1+BULLET_Y_SCREEN,71
        POKE BULLET_X_SCREEN+1+BULLET_Y_SCREEN,0
    ENDIF        
    IF (G1=69 OR G1=70) AND G4=71 AND G6=68  
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
        POKE BULLET_X_SCREEN+1+BULLET_Y_SCREEN,0
        POKE BULLET_X_SCREEN-1+BULLET_Y_SCREEN,0
    ENDIF
    IF G4&127>63 AND G4&127<68 AND G1>67 THEN POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
    IF G6&127>63 AND G6&127<68 AND G1>67 THEN POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
    IF G4&127>63 AND G4&127<68 AND G1>67 AND (G6=68 OR G6=71) 
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
        POKE BULLET_X_SCREEN-1+BULLET_Y_SCREEN,0
    ENDIF
    IF G6&127>63 AND G6&127<68 AND G1>67 AND (G4=68 OR G4=71) 
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
        POKE BULLET_X_SCREEN+1+BULLET_Y_SCREEN,0
    ENDIF
    IF G4>63 AND G4<68 AND G1>67 AND (G6=69 OR G6=70) 
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
        POKE BULLET_X_SCREEN-1+BULLET_Y_SCREEN,71
    ENDIF
    IF G6>63 AND G6<68 AND G1>67 AND (G4=69 OR G4=70) 
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,0
        POKE BULLET_X_SCREEN+1+BULLET_Y_SCREEN,68
    ENDIF
    IF (G2=65 OR G2=66) AND G1>67 
        POKE BULLET_X_SCREEN+BULLET_Y_SCREEN,67
    ENDIF
    POKE 53278,0
    BULLET_STATUS=0
ENDPROC
'------------------------------
' Bullet hit bug and uses hardware collision detection'
PROC BULLET_HIT_BUG
    IF PEEK(53256)&2=2 AND BUG_TYPE(0) > 0 AND BUG_TYPE(0) < 14
      POKE 705,15
      Z=USR(@START_SOUND,3,1)
      I=0
      EXEC  Score_For_Hit_Bug
      BUG_TYPE(0) = 15
    ENDIF
    IF PEEK(53256)&4=4 AND BUG_TYPE(1) > 0 AND BUG_TYPE(1) < 14
      POKE 706,15
      Z=USR(@START_SOUND,3,1)
      I=1
      EXEC  Score_For_Hit_Bug
      BUG_TYPE(1) = 15
    ENDIF
    IF PEEK(53256)&8=8 AND BUG_TYPE(2) > 0 AND BUG_TYPE(2) < 14
      POKE 707,15
      Z=USR(@START_SOUND,3,1)
      I=2
      EXEC  Score_For_Hit_Bug
      BUG_TYPE(2) = 15
    ENDIF
     
    POKE 53278,0
    POKE 53252,0:G1=0
    Bugs_Remaining = Bugs_Remaining - 1
    IF Bugs_Remaining <= 0  
        IF Current_Level < 40 THEN Current_Level = Current_Level + 1
        if Exit_Flag = 0 then Exit_Flag = 1
    ENDIF
    IF SC > Next_Extra_Life_Score  
        Next_Extra_Life_Score = Next_Extra_Life_Score + 1000
        Lives = Lives + 1
        Z=USR(@START_SOUND,5,1)
    ENDIF
    EXEC PRINT_SCORE
    POKE 53278,0
    G1=0
    MOVE @CHARSET_GAME_ADDR,@PMBANK+800+BULLET_Y_POS,8
    MOVE @CHARSET_GAME_ADDR,@PMBANK+808+BULLET_Y_POS,8
ENDPROC
'------------------------------
PROC Score_For_Hit_Bug
      IF BUG_TYPE(i) = 1 
        SC = SC + 7 
      ELIF BUG_TYPE(i) = 2
        SC = SC + 10
      ELIF BUG_TYPE(i) = 3
        SC = SC + 15
      ELIF BUG_TYPE(i) = 4
        SC = SC + 12
      ELIF BUG_TYPE(i) = 5
        SC = SC + 30
      ELIF BUG_TYPE(i) = 6
        SC = SC + 20
      ELIF BUG_TYPE(i) = 7
        SC = SC + 40
      ELIF BUG_TYPE(i) = 8
        SC = SC + 50
      ENDIF
ENDPROC

PROC BUG_GOT_PASSED
    Z=USR(@START_SOUND,4,1)
    Lives = Lives - 1
    IF Lives < 0 
        Exit_Flag = 3
    ELSE
        EXEC PRINT_SCORE
        BUG_TYPE(BUG_INDEX) = 0
    ENDIF
ENDPROC
'------------------------------
PROC PRINT_SCORE
    POSITION 0,0
    PRINT "SCORE:";SC
    POSITION 20,0
    PRINT "PASS:";Lives
    POSITION 30,0
    PRINT "LEVEL:";Current_Level
ENDPROC 
'------------------------------
PROC DRAW_FIELD
    POKE @NMIEN,64
    DPOKE @VDSLST,@GAME00_DLI
    POKE 708,16
    POKE 709,66
    POKE 710,60
    POKE 711,156
    POKE 712,224    
    CLS
    P = 3
    FOR N=SCR_ADDR + 120 TO SCR_ADDR + 880 STEP 40        
        FOR M=0 TO 2
            X=RAND(40)
            Y=RAND(P)
            IF Y = 0 
                EXEC DRAW_PIECE_0
            ELIF Y = 1
                EXEC DRAW_PIECE_1
            ELIF Y = 2
                EXEC DRAW_PIECE_2
            ENDIF
        NEXT M
        P = 3
    NEXT N
    IF Current_Level < 9 
        Bugs_Remaining = 10
        Add_New_Bug_CountDown = 24
        Add_New_Bug_Reset = 24
    ELIF Current_Level < 17
        Bugs_Remaining = 12
        Add_New_Bug_CountDown = 20
        Add_New_Bug_Reset = 22
    ELIF Current_Level < 25
        Bugs_Remaining = 14
        Add_New_Bug_CountDown = 16
        Add_New_Bug_Reset = 20
    ELSE
        Bugs_Remaining = 16   
        Add_New_Bug_CountDown = 12
        Add_New_Bug_Reset = 18                   
    ENDIF
    FOR BUG_INDEX = 0 TO 2
        EXEC DEFINE_NEW_BUG
    NEXT BUG_INDEX
    
    POKE @SDMCTL,62
    POKE @NMIEN,192
ENDPROC 
'------------------------------
 
PROC DRAW_PIECE_0
    POKE X+N,67:POKE X+(N-40),65
ENDPROC 
'------------------------------
PROC DRAW_PIECE_1
    POKE X+N,67:POKE X+(N-40),194 
ENDPROC
'------------------------------
PROC DRAW_PIECE_2
    X=RAND(30)
    Y=RAND(8) + 2
    IF PEEK(X+N)<>67 THEN POKE X+N,68
    IF PEEK(X+Y+N)<>67 THEN POKE X+Y+N,71
    FOR O = N + X + 1 TO N + X + Y - 1
        IF PEEK(O)<>67 THEN POKE O,69+RAND(2)
    NEXT O
    P=2
    O=0
ENDPROC
'------------------------------
PROC DEFINE_NEW_BUG
    BUG_TYPE(BUG_INDEX) = RAND(8)+1
    IF BUG_TYPE(BUG_INDEX) = 1 ' *** Beetle
        BUG_XPOS(BUG_INDEX) = 48 + RAND(152)
        BUG_YPOS(BUG_INDEX) = 2
        BUG_XDIR(BUG_INDEX) = RAND(2) * 2 - 1
        POKE 705 + BUG_INDEX,172
        I = 68 + Current_Level * 2
        IF I > 224 THEN I = 224
        BUG_MOVE_FRACTION(BUG_INDEX)= I
        BUG_MOVE_DELAY(BUG_INDEX) = I
        I = 2 + Current_Level / 4
        IF I>4 THEN I = 4
        BUG_YDIR(BUG_INDEX) = I    
    ELIF BUG_TYPE(BUG_INDEX) = 2 ' *** Flea
        BUG_XPOS(BUG_INDEX) = 48 + RAND(152)
        BUG_YPOS(BUG_INDEX) = 6
        BUG_XDIR(BUG_INDEX) = RAND(2) * 2 - 1
        POKE 705 + BUG_INDEX,80  
        IF Current_Level < 5
          I = 30 - Current_Level * 3
        ELSE
          I = 28 - Current_Level * 2
        ENDIF
        IF I < 5 THEN I = 5
        BUG_MOVE_FRACTION(BUG_INDEX) = I
        BUG_MOVE_DELAY(BUG_INDEX) = BUG_MOVE_FRACTION(BUG_INDEX) + RAND(24)
        BUG_YDIR(BUG_INDEX) = 0       
    ELIF BUG_TYPE(BUG_INDEX) = 3 ' *** Ant
        BUG_XPOS(BUG_INDEX) = 48 + RAND(152)
        BUG_YPOS(BUG_INDEX) = 2
        BUG_XDIR(BUG_INDEX) = RAND(2) * 2 - 1
        POKE 705 + BUG_INDEX,56
        I = 62 + Current_Level * 3
        IF I > 240 THEN I = 240
        BUG_MOVE_FRACTION(BUG_INDEX)= I
        BUG_MOVE_DELAY(BUG_INDEX) = I
        I = 3 + Current_Level 
        IF I > 6 THEN I = 6
        BUG_YDIR(BUG_INDEX) = I        
    ELIF BUG_TYPE(BUG_INDEX) = 4 '** Termite
        BUG_XPOS(BUG_INDEX) = 48 + RAND(152)
        BUG_YPOS(BUG_INDEX) = 2
        BUG_XDIR(BUG_INDEX) = RAND(2) * 2 - 1
        POKE 705 + BUG_INDEX,104
        I = 72 + Current_Level * 2
        IF I > 240 THEN I = 240
        BUG_MOVE_FRACTION(BUG_INDEX)= I
        BUG_MOVE_DELAY(BUG_INDEX) = I
        I = 2 + Current_Level / 4
        IF I>4 THEN I = 4
        BUG_YDIR(BUG_INDEX) = I            
    ELIF BUG_TYPE(BUG_INDEX) = 5 '** Bee'
        IF PEEK(53770) < 127
          BUG_XPOS(BUG_INDEX) = 32
          BUG_XDIR(BUG_INDEX) = 1
        ELSE
          BUG_XPOS(BUG_INDEX) = 224 
          BUG_XDIR(BUG_INDEX) = -1
        ENDIF
        BUG_YPOS(BUG_INDEX) = 24 + RAND(128)
        POKE 705 + BUG_INDEX,60
        I = 80 + Current_Level * 3
        IF I > 255 THEN I = 255
        BUG_MOVE_FRACTION(BUG_INDEX)= I
        BUG_MOVE_DELAY(BUG_INDEX) = I
    ELIF BUG_TYPE(BUG_INDEX) = 6 '** Roach
        BUG_XPOS(BUG_INDEX) = 60 + RAND(128)
        BUG_YPOS(BUG_INDEX) = 2
        POKE 705 + BUG_INDEX,26
        I = 60 + Current_Level * 4
        IF I > 240 THEN I = 240            
        BUG_MOVE_FRACTION(BUG_INDEX)= I
        BUG_MOVE_DELAY(BUG_INDEX) = I
    ELIF BUG_TYPE(BUG_INDEX) = 7 '** Fly
        BUG_XPOS(BUG_INDEX) = 54 + RAND(140)
        BUG_YPOS(BUG_INDEX) = 2
        BUG_XDIR(BUG_INDEX) = 3 + RAND(3)
        POKE 705 + BUG_INDEX,204
        I = 54 + Current_Level * 3
        IF I > 252 THEN I = 252            
        BUG_MOVE_FRACTION(BUG_INDEX)= I
        BUG_MOVE_DELAY(BUG_INDEX) = I
    ELIF BUG_TYPE(BUG_INDEX) = 8 '** Wasp
        BUG_XPOS(BUG_INDEX) = 56 + RAND(136)
        BUG_YPOS(BUG_INDEX) = 2
        BUG_XDIR(BUG_INDEX) = RAND(2)*2 - 1
        POKE 705 + BUG_INDEX,124
        I = 30 + Current_Level * 3
        IF I > 246 THEN I = 246            
        BUG_MOVE_FRACTION(BUG_INDEX)= I
        BUG_MOVE_DELAY(BUG_INDEX) = I             
    ENDIF
ENDPROC
'------------------------------
PROC DISPLAY_GAME_OVER
    FOR N=0 TO 9
        POSITION 12,12
        PRINT "                    "
        PAUSE 10
        POSITION 12,12
        PRINT "G A M E      O V E R"
        PAUSE 10
    NEXT N
    IF SC>H1 
        H3=H2
        H2=H1
        H1=SC
    ELIF SC>H2 
        H3=H2
        H2=SC
    ELIF SC>H3 
        H3=SC
    ENDIF          
ENDPROC 
'------------------------------
PROC DISPLAY_TITLE
    DPOKE 560,@display_list_title
    DPOKE 88,@SCREEN_ADDR
    POKE 82,0
    POKE 756,@CHARSET_GAME_HI    
    POKE 712,34:POKE 710,42:POKE 709,80:POKE 711,108
    POKE @NMIEN,64
    Z=USR(@PMCLEAR,@PMBANK+768,5)
    Z=USR(@SILENCE)
    DPOKE @VVBLKD,@TITLE_VBI
    TITLE_PHASE = 0
    TITLE_CHANGE = 0    
    REPEAT
      IF TITLE_CHANGE = 0
        CLS
        POSITION 12,2:PRINT "B U G   I N V A D E"
        POSITION 1,3:PRINT "Fast Basic Version"
        POSITION 22,3:PRINT "By Peter J. Meyer"  ':POKE DPEEK(560)+8,5 
        POSITION 3,7:PRINT "Your score:";SC
        IF TITLE_PHASE = 0
            POSITION 0,9:PRINT "High scores:"
            POSITION 12,9:PRINT H1
            POSITION 9,10:PRINT "2nd:";H2
            POSITION 10,11:PRINT "3rd:";H3
            POSITION 0,20:PRINT " Press START or FIRE to play begin game"
        ELSE          
            POSITION 0,9
            PRINT "Try some of these Atari cartrdige"
            PRINT "games available from Video 61."
            PRINT 
            PRINT "ANIMAL KEEPER      AMOK BOTS"
            PRINT "DELTA SPACE ARENA  HELICOMMANDER"
            PRINT "HEXXAGON           ICE CREAM IKE"
            PRINT "LASER BLAST X      MEGAOINDS"            
            PRINT "NOVAWOLF           SURROUND'EM"        
            PRINT "TILE SMASHERS      VENTURE"            
            PRINT 
            PRINT "PUTT EIGHTEEN MINATURE GOLF"
            PRINT "SECRETUM LABYRINTH THE LEGEND"

            
        ENDIF
        TITLE_PHASE = TITLE_PHASE + 1
        IF TITLE_PHASE >=2 THEN TITLE_PHASE = 0
        TITLE_CHANGE = 180
      ELSE
         TITLE_CHANGE =  TITLE_CHANGE - 1
      ENDIF
      PAUSE 1
    UNTIL PEEK(53279)=6 OR STRIG(0)=0    
ENDPROC
