'REM PROJECT: BLACK JACK ATARI FAST BASIC. PORTED FROM DARK BASIC VERSION.
'REM CREATED: FRIDAY, AUGUST 10, 2012
'REM ***** MAIN SOURCE FILE *****

'REM **** Conversion in Progress ****
DATA BLACKJACKVALUE()  BYTE =  11,2,3,4,5,6,7,8,9,10,10,10,10,11
DATA POKERVALUE()      BYTE =  15,2,3,4,5,6,7,8,9,10,12,13,14,11

DIM OLDORDER(512) WORD

DBL_DOWN_SET = 0
DBL_DOWN_3UP = 0
DECKSIZE = 52
NUMBER_OF_DECKS = 4
                                          
DIM BONUS_777_OPTION_SHOW$(1)   
BONUS_777_OPTION_SHOW$(0) =    "NO BONUS WITH 777       "
BONUS_777_OPTION_SHOW$(1) =    "PAYS 3:1, SUIT 4:1      "

DIM BONUS_678_OPTION_SHOW$(1)   
BONUS_678_OPTION_SHOW$(0) =    "NO BONUS WITH 678       "
BONUS_678_OPTION_SHOW$(1) =    "PAYS 3:1, SUIT 4:1      "
BONUS777 = 0
BONUS678 = 0
 
DEALERTIEWINS = 1

DEALERSOFT17HIT = 0
'DIM UNBUSTED_OPTION_SHOW$(1)
CARDCHARLIEWINSET = 6

SPLITSET = 0                      
'BLACKJACK_BONUS_OPTION_SHOW$(3) = "                                 "
SUIT21BONUS = 0
COLOR21BONUS  = 0 

DIM NUMBER_OF_PLAYERS_OPTION_SHOW$(3)
NUMBER_OF_PLAYERS_OPTION_SHOW$(0) = "ONE PLAYER   "            
NUMBER_OF_PLAYERS_OPTION_SHOW$(1) = "TWO PLAYERS  "
NUMBER_OF_PLAYERS_OPTION_SHOW$(2) = "THREE PLAYERS"
NUMBER_OF_PLAYERS_OPTION_SHOW$(3) = "FOUR PLAYERS "



MAXPLAYERS = 4
TRUE = 1
FALSE = 0
LASTPLAYER = 0
DIM CARDIMAGE$(56) 
CARDBACKIMAGE$ = "         "
CARDIMAGE$(0) = "         "
CARDIMAGE$(1) = "         "
CARDIMAGE$(2) = "         "
DIM BJCARDVALUE(56 * 10) BYTE
DIM PKCARDVALUE(56 * 10) BYTE
DIM CARDSUITVALUE(56 * 10) BYTE
DIM CARDORDER(56 * 10) BYTE
DIM CARDIMG(60) WORD
DIM MODE_RES_X(64)  BYTE
DIM MODE_RES_Y(64)  BYTE
DIM MODE_BITDEPTH(64)  BYTE
DISPPLAYERSPACING = 0
DISPPLAYERLEFT = 0
DISPPLAYERTOP = 0
DISPPLAYERVERTICAL = 0
SELECTED_BITDEPTH = 0
SELECTED_RES_X = 0
SELECTED_RES_Y = 0
DEALERHIDDEN = 0
CARDNO = 0
RESHUFFLENO = 0
NEWCARDVALUE = 0
ROUNDSPLAYED = 0
PLAYERNO = 0
GONEXTPLAYER = 0
ALLOWSPLIT = 0
EXITGAME = 0
NUMBEROFDECKS = 4
DIM PLAYERNNAME$(4) 
DIM PLAYERNDRAWN(4) BYTE
DIM PLAYERNTOTAL(16) BYTE
DIM PLAYERNCASH(4) WORD
DIM PLAYERNBETENTERY(4) 
DIM PLAYERNBET(4) 
DIM PLAYERNSPLIT(4) BYTE
DIM PLAYERNACES(4) BYTE
DIM PLAYERBJCARDVAL(80)  BYTE
DIM PLAYERPKCARDVAL(80)  BYTE
DIM PLAYERSUCARDVAL(80)  BYTE
DIM PLAYERNSPLITCARD(80) BYTE
DIM PLAYERNDOUBLEUSED(4) 
DIM PLAYERNMESSAGE$(4) 
DIM HANDREWARD(3) WORD
KEYPRESSED$=""
ERRORMESSAGE$ = ""
LASTKEYPRESSED = 0
DEALERTIEWINS = 0
DEALERSOFT17HIT = 0 ' IF DEALER HAS ONE ACE
DEALERNOHIDDEN = 0
CARDCHARLIEWINSET = 6
ALLOWSURRENDER = 0 ' PLAYER GETS BACK HALF BET
SUIT21BONUS = 0
COLOR21BONUS = 0
SUITEDAJBONUS = 0 ' ACE + JACK PAY IN SAME SUIT 2:1
JOKERBETRETURN = 0 
JOKERSPECIALACE = 0
JOKERSPRESANT = 0
DIM PLAYERNWINS(4) 
TOTALCARDS = 0
DECKSIZE = 0
SUITSIZE = 0
SS = 0   
END_PROGRAM = 0
A=0
B=0
C=0
D=0
E=0 
F$="" 
G=0 
H=0 
I=0 
J=0 
K=0 
L=0
M=0
N=0
O=0
P=0
Q=0
R=0
S=0
T=0
U=0
V=0
W=0
X=0
Y=0
Z=0
END_PROGRAM = FALSE

REPEAT
    EXEC INIT_DEFAULTS
    EXEC TITLE_SCREEN

    DO
    LOOP

    IF END_PROGRAM = 0
        EXEC INIT_GAME
        EXEC SHOWSTATS
       'EXEC SHOW_ALL_CARDS
        EXEC SHUFFLEDECKS    
        EXITGAME = 0
        REPEAT
            EXEC GETBETS
            IF EXITGAME = 0
                PLAYERNO = 0
                '**  EXEC SHOW_ALL_SPRITES
                EXEC PERFORM_DEAL
                IF PLAYERNO <= LASTPLAYER
                    REPEAT
                        EXEC PERFORM_PLAYERINPUT
                        IF PLAYERNSPLIT(PLAYERNO) = 2 
                            EXEC PERFORM_NEXTHAND                        
                        ELSE
                            PLAYERNO = PLAYERNO + 1
                        ENDIF
                    UNTIL PLAYERNO > LASTPLAYER    
                ENDIF
                PLAYERNO = 0 
                EXEC PERFORM_DEALER
                EXEC PERFORM_REWARDS
                REPEAT
                      EXEC SHOWSTATS
                      GET LASTKEYPRESSED
                      IF LASTKEYPRESSED>96 AND LASTKEYPRESSED<123 THEN LASTKEYPRESSED = LASTKEYPRESSED - 32 
                      KEYPRESSED$=CHR$(LASTKEYPRESSED)
                      IF KEYPRESSED$ = "S" 
                             '***  GET IMAGE 999,0,0,SELECTED_RES_X,SELECTED_RES_Y,3
                             F$="IMGBLACKJACK"   '***  + (STR$(INT(SS/10))) + (STR$(SS MOD 10)) + ".BMP"
                             '***  SAVE IMAGE F$,999
                             SS=SS+1
                             IF SS>9 THEN SS=0
                      ENDIF 
                      GET LASTKEYPRESSED:KEYPRESSED$=CHR$(LASTKEYPRESSED)        
                UNTIL KEYPRESSED$ = " " 
                '** EXEC SHOW_ALL_SPRITES
                FOR I = 0 TO CARDNO 
                    J = CARDORDER(I) 
                    '***  ' ****   SPRITEJ+1, -200, -200, CARDIMAGE(J)
                NEXT I
                IF CARDNO >= RESHUFFLENO THEN EXEC SHUFFLEDECKS
            ENDIF
        UNTIL EXITGAME >= 1
    ENDIF
UNTIL END_PROGRAM = TRUE

END

PROC INIT_GAME
        DECK = 0        
        CARDPLACE = 0
        IMAGEGET = 0
        LASTCARD = 0
        FILENUMBERSTRING$ = ""        
        ' SET WINDOW OFF
        '***  SET DISPLAY MODE SELECTED_RES_X, SELECTED_RES_Y, SELECTED_BITDEPTH
        '***  COLOR BACKDROP RGB(16,64,16)
        IF SELECTED_RES_Y > 300 AND SELECTED_RES_Y < 621
            DISPPLAYERTOP = 244
            DISPPLAYERVERTICAL = 27
        ENDIF
        IF SELECTED_RES_Y > 620 AND SELECTED_RES_Y < 780 
            DISPPLAYERTOP = 248
            DISPPLAYERVERTICAL = 28
        ENDIF
        IF SELECTED_RES_Y >= 779 AND SELECTED_RES_Y < 1000 
            DISPPLAYERTOP = 280
            DISPPLAYERVERTICAL = 30
        ENDIF
        IF SELECTED_RES_Y > 999
            DISPPLAYERTOP = 320
            DISPPLAYERVERTICAL = 32
        ENDIF
        IF LASTPLAYER = 1 
            DISPPLAYERSPACING = 250
            DISPPLAYERLEFT = 150
        ELSE            
            IF LASTPLAYER >= 4 
                DISPPLAYERSPACING = (SELECTED_RES_X - 120) / LASTPLAYER
                DISPPLAYERLEFT = 50
             ELSE
                DISPPLAYERSPACING = (SELECTED_RES_X - 160) / LASTPLAYER
                DISPPLAYERLEFT = 75
             ENDIF            
             IF DISPPLAYERSPACING <= 270
                DISPPLAYERSPACING = 270
                DISPPLAYERLEFT = 25
             ENDIF
        ENDIF         
        IF DECKSIZE = 56
            LASTCARD = 55
            SUITSIZE = 13
         ELIF DECKSIZE = 52
            LASTCARD = 51            
            SUITSIZE = 13
         ELIF DECKSIZE = 48
            LASTCARD = 47
            SUITSIZE = 12
         ENDIF
        TOTALCARDS = DECKSIZE * NUMBER_OF_DECKS - 1        
        RESHUFFLENO = TOTALCARDS - 22
        IF LASTPLAYER >= 4 AND NUMBEROFDECKS >=6 THEN RESHUFFLENO = TOTALCARDS - 28
        FOR CARDPLACE = 1 TO 64

        NEXT CARDPLACE        
        CARDNO = 0 
        IMAGEGET = 1
        FOR CARDPLACE = 0 TO LASTCARD
            IF (CARDPLACE MOD SUITSIZE) = 0 THEN CARDNO = 0            
            V =  BLACKJACKVALUE(CARDNO)
            BJCARDVALUE(CARDPLACE) = V
            V =  POKERVALUE(CARDNO)
            PKCARDVALUE(CARDPLACE) = V
            CARDSUITVALUE(CARDPLACE) = 1 + INT(CARDPLACE / SUITSIZE)
            CARDNO = CARDNO + 1 
        NEXT CARDPLACE
        CARDNO = DECKSIZE
        JOKERSPRESANT = 0
        IF DECKSIZE = 56 THEN SUITSIZE = 14
        FOR N = 1 TO MAXPLAYERS
            PLAYERNCASH(N) = 5000
            PLAYERNBET(N) = 0
        NEXT N        
ENDPROC

PROC CALCTOTAL
    FROM = 0
    TOTAL = 0
    ACES = 0
    PLAYERNACES(PLAYERNO) = 0
    IF PLAYERNSPLIT(PLAYERNO) <= 2
        FROM = 0
    ENDIF
    IF PLAYERNSPLIT(PLAYERNO) = 3
        FROM = 7
    ENDIF
    IF PLAYERNSPLIT(PLAYERNO) = 4
        FROM = 14
    ENDIF
    FOR I = FROM TO PLAYERNDRAWN(PLAYERNO) - 1
        TOTAL = TOTAL + PLAYERBJCARDVAL(PLAYERNO*20+I)
        IF PLAYERBJCARDVAL(PLAYERNO*20+I) = 11 
            ACES = ACES + 1
            PLAYERNACES(PLAYERNO) = PLAYERNACES(PLAYERNO) + 1
         ENDIF
    NEXT I
    ' CHECK IF THERE ARE ANY ACES WHEN A TOTAL IS MORE THAN 21, IF SO, SUBRACT 10 UNTIL NO MORE ACES OR LESS THAN 22.
    IF ACES > 0
        FOR I = 0 TO ACES - 1 
            IF TOTAL > 21 THEN TOTAL = TOTAL - 10
        NEXT I
    ENDIF
    IF PLAYERNSPLIT(PLAYERNO) <= 2 
        PLAYERNTOTAL(PLAYERNO+0) = TOTAL
    ENDIF
    IF PLAYERNSPLIT(PLAYERNO) = 3         
        PLAYERNTOTAL(PLAYERNO+4) = TOTAL
    ENDIF
ENDPROC

PROC PERFORM_DEAL
    PLAYERNUMBER = 0
    USEIMAGE = 0
    CARDSPACING = 0 
    CARDSPACING = 16
    ROUNDSPLAYED = ROUNDSPLAYED + 1
    CLS
    EXEC SHOWSTATS
    IF PLAYERNSPLIT(PLAYERNO) >= 2 THEN CARDSPACING = 14
    FOR PLAYERNUMBER = 0 TO LASTPLAYER
        FOR N = 0 TO 20
            PLAYERBJCARDVAL(PLAYERNUMBER*20+N) = 0
            PLAYERPKCARDVAL(PLAYERNUMBER*20+N) = 0
        NEXT N
        PLAYERNO = PLAYERNUMBER
        PLAYERNMESSAGE$(PLAYERNO) = ""
        PLAYERNSPLIT(PLAYERNO) = 0
        PLAYERNTOTAL(PLAYERNO+0) = 0
        PLAYERNTOTAL(PLAYERNO+4) = 0
        PLAYERNTOTAL(PLAYERNO+8) = 0
        PLAYERNTOTAL(PLAYERNO+12) = 0
        PLAYERNACES(PLAYERNO) = 0
        PLAYERNDOUBLEUSED(PLAYERNO) = 0
        PLAYERNSPLITCARD(PLAYERNO) = 0
         FOR N = 0 TO 1
             IF PLAYERNO = 0 
                 G = 16 * N                 
                 H =  10 * N
                 IF N = 1
                    '** USEIMAGE = CARDBACKIMAGE 
                    DEALERHIDDEN = CARDNO
                 ELSE
                    '** USEIMAGE = CARDIMAGE(CARDORDER(CARDNO))
                 ENDIF
             ELSE                 
                 G = 16 * N                 
                 H = 10 * N
                 IF SELECTED_RES_Y < 640 THEN H = 10 * N
                 '** USEIMAGE = CARDIMAGE(CARDORDER(CARDNO))
             ENDIF
             
             EXEC ANIMATEHIT
             NEWCARDVALUE = BJCARDVALUE(CARDORDER(CARDNO))
             PLAYERBJCARDVAL(PLAYERNO*20) = NEWCARDVALUE
             PLAYERPKCARDVAL(PLAYERNO*20) = PKCARDVALUE(CARDORDER(CARDNO))
             PLAYERSUCARDVAL(PLAYERNO*20) = CARDSUITVALUE(CARDORDER(CARDNO))
             IF PLAYERNO > 0              
                IF N = 0 
                    IF SPLITSET = 0
                            G = BJCARDVALUE(CARDORDER(CARDNO))
                    ELIF SPLITSET = 1
                            G = PKCARDVALUE(CARDORDER(CARDNO))
                    ELIF SPLITSET = 2
                            IF BJCARDVALUE(CARDNO) < 11 
                                G = PKCARDVALUE(CARDORDER(CARDNO))
                            ELSE
                                G = 255
                            ENDIF
                    ENDIF
                 ENDIF
                 IF N = 1 
                    IF SPLITSET = 0     
                        H = BJCARDVALUE(CARDORDER(CARDNO))
                    ELIF SPLITSET = 1
                            H = PKCARDVALUE(CARDORDER(CARDNO))
                    ELIF SPLITSET = 1
                            IF BJCARDVALUE(CARDNO) < 11 
                                H = PKCARDVALUE(CARDORDER(CARDNO))
                            ELSE
                                H = 254 ' 255 WILL NOT EQUAL 254
                            ENDIF
                     ENDIF 
                     IF G = H 
                        PLAYERNSPLIT(PLAYERNO) = 1
                        PLAYERNSPLITCARD(PLAYERNO) = CARDORDER(CARDNO)
                     ENDIF
                 ENDIF
             ENDIF
             CARDNO = CARDNO + 1
        NEXT N
        PLAYERNDRAWN(PLAYERNO) = 2
        IF PLAYERNO = 0 
             EXEC CALCTOTAL
        ELSE
             EXEC CALCTOTAL        
             IF PLAYERNTOTAL(PLAYERNO+0) = 21 
                PLAYERNMESSAGE$(PLAYERNO) = "TOTAL:21  BLACKJACK"               
             ELSE
                PLAYERNMESSAGE$(PLAYERNO) = "TOTAL:     "  ' STR$(PLAYERNTOTAL(PLAYERNO))
             ENDIF
             EXEC SHOWSTATS
        ENDIF
    NEXT PLAYERNUMBER
    PLAYERNO = 1
    IF PLAYERNTOTAL(1+0) = 21 
        PLAYERNO = 2
        IF PLAYERNTOTAL(2+0) = 21 
            PLAYERNO = 3
            IF PLAYERNTOTAL(3+0) = 21 
                PLAYERNO = 4
                IF PLAYERNTOTAL(4+0) = 21 THEN PLAYERNO = MAXPLAYERS + 1
            ENDIF
        ENDIF
    ENDIF
    IF PLAYERNTOTAL(0) = 21 
        IF PLAYERBJCARDVAL(0) = 11 THEN PLAYERNO = MAXPLAYERS + 1
    ENDIF
ENDPROC

PROC ANIMATEHIT
   CARDPOSX = 0
   CARDPOSY = -24
   N = 0
   REPEAT
       CARDPOSX = CARDPOSX + 1
       CARDPOSY = CARDPOSY + 1
       N = N - 1
   UNTIL N<=0
    EXEC SHOWSTATS
ENDPROC

PROC PERFORM_PLAYERINPUT
 
    KEYUP = 0
    GONEXTPLAYER = 0
    PLAYERNDOUBLEUSED(PLAYERNO) = 0
    CLS
    REPEAT         
        EXEC SHOWSTATS
        IF PLAYERNCASH(PLAYERNO) < PLAYERNBET(PLAYERNO) THEN PLAYERNDOUBLEUSED(PLAYERNO) = 1
        IF DBL_DOWN_SET = 1 AND (PLAYERNTOTAL(PLAYERNO+0) <=9 OR PLAYERNTOTAL(PLAYERNO+0)>=12) THEN PLAYERNDOUBLEUSED(PLAYERNO) = 1
        IF DBL_DOWN_SET = 2 AND (PLAYERNTOTAL(PLAYERNO+0) <=8 OR PLAYERNTOTAL(PLAYERNO+0)>=12) THEN PLAYERNDOUBLEUSED(PLAYERNO) = 1
        IF (PLAYERNTOTAL(PLAYERNO+0) < 21 OR (PLAYERNSPLIT(PLAYERNO) = 3) AND PLAYERNTOTAL(PLAYERNO+4) <21)
            EXEC SHOWCOMMANDS
            
            GET LASTKEYPRESSED
            IF LASTKEYPRESSED>96 AND LASTKEYPRESSED<123 THEN LASTKEYPRESSED = LASTKEYPRESSED - 32 
            KEYPRESSED$=CHR$(LASTKEYPRESSED)
            IF KEYPRESSED$ = "H" 
                EXEC PERFORM_HIT
                IF PLAYERNDRAWN(PLAYERNO) >=3 AND DBL_DOWN_3UP = FALSE THEN PLAYERNDOUBLEUSED(PLAYERNO) = 1
                IF PLAYERNDRAWN(PLAYERNO) >=4 AND DBL_DOWN_3UP = TRUE THEN PLAYERNDOUBLEUSED(PLAYERNO) = 1
            ENDIF
            IF KEYPRESSED$ = "S" 
                GONEXTPLAYER = TRUE
                IF PLAYERNSPLIT(PLAYERNO) <= 1 
                    PLAYERNMESSAGE$(PLAYERNO) = "TOTAL:"
                    PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+0))
                    PLAYERNMESSAGE$(PLAYERNO) =+"  STANDING"                
                ENDIF
                IF PLAYERNSPLIT(PLAYERNO) = 2                             
                    PLAYERNMESSAGE$(PLAYERNO) = "T:"
                    PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+0)) 
                    PLAYERNMESSAGE$(PLAYERNO) =+ " STAND|T:" 
                    PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+4))
                ENDIF
                IF PLAYERNSPLIT(PLAYERNO) = 3
                    PLAYERNMESSAGE$(PLAYERNO) = "T:"
                    PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+0)) 
                    PLAYERNMESSAGE$(PLAYERNO) =+ "|T:" 
                    PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+4)) 
                    PLAYERNMESSAGE$(PLAYERNO) =+ " STAND"
                ENDIF
            ENDIF
            IF KEYPRESSED$ = "D" AND PLAYERNDOUBLEUSED(PLAYERNO) = 0
                PLAYERNBET(PLAYERNO) = PLAYERNBET(PLAYERNO) * 2
                EXEC PERFORM_HIT
                GONEXTPLAYER = 1
                PLAYERNDOUBLEUSED(PLAYERNO) = 1
            ENDIF
            IF KEYPRESSED$ = "T" AND PLAYERNSPLIT(PLAYERNO) <= 1 
                EXEC PERFORM_SPLITDEAL
            ENDIF
            IF KEYPRESSED$ = "U" AND ALLOWSURRENDER = TRUE AND PLAYERNDRAWN(PLAYERNO) = 2 AND PLAYERNSPLIT(PLAYERNO) <= 1 
                PLAYERNDRAWN(PLAYERNO) = - PLAYERNDRAWN(PLAYERNO)                
                PLAYERNMESSAGE$(PLAYERNO) = "SURRENDERED"
                GONEXTPLAYER = 1
            ENDIF
            IF KEYPRESSED$ = "Q" 
                GONEXTPLAYER = 1
            ENDIF
            KEYPRESSED$ = ""
          ELSE
             GONEXTPLAYER = 1
          ENDIF
    UNTIL GONEXTPLAYER = 1
ENDPROC

PROC PERFORM_DEALER
    Z = 100
    G = 0 
    REPEAT
        Z = Z - 2
        ' ****STRETCH ' ****   SPRITECARDORDER(DEALERHIDDEN)+ 1,Z, 100        
    UNTIL Z<=2
    IF SELECTED_RES_Y < 640
        H = 58 + 10
    ELSE
        H = 64 + 12 
    ENDIF
    ' ****   SPRITECARDORDER(DEALERHIDDEN)+ 1, G, H, CARDIMAGE(CARDORDER(DEALERHIDDEN))
    REPEAT
        Z = Z + 2
        ' ****STRETCH ' ****   SPRITECARDORDER(DEALERHIDDEN)+ 1,Z, 100        
    UNTIL Z>=100
    EXEC CALCTOTAL
    PLAYERNO = 0
    IF PLAYERNTOTAL(0+0) < 21 
        REPEAT 
            IF DEALERSOFT17HIT = FALSE     
                IF PLAYERNTOTAL(0+0) <= 16 THEN EXEC PERFORM_HIT 
            ELSE
                IF PLAYERNTOTAL(0+0) <= 16 THEN EXEC PERFORM_HIT 
                EXEC CALCTOTAL
                IF PLAYERNTOTAL(0+0) = 17 AND PLAYERNACES(0) > 0 THEN EXEC PERFORM_HIT
                EXEC CALCTOTAL
            ENDIF                
        UNTIL PLAYERNTOTAL(0+0) >= 17
        IF PLAYERNTOTAL(0+0) <=21 
            PLAYERNMESSAGE$(0) = "TOTAL:" 
            PLAYERNMESSAGE$(0) =+ STR$(PLAYERNTOTAL(0)) 
            PLAYERNMESSAGE$(0) =+ "   STANDING"
        ELSE
            PLAYERNMESSAGE$(0) = "TOTAL:" 
            PLAYERNMESSAGE$(0) =+ STR$(PLAYERNTOTAL(0)) 
            PLAYERNMESSAGE$(0) =+ "   BUSTED"
        ENDIF        
    ELSE:IF PLAYERNDRAWN(0) = 2 AND PLAYERNTOTAL(0) = 21  
        PLAYERNMESSAGE$(0) = "TOTAL:" 
        PLAYERNMESSAGE$(0) =+ STR$(PLAYERNTOTAL(0)) 
        PLAYERNMESSAGE$(0) =+ "  BLACKJACK"
    ENDIF:ENDIF
    EXEC SHOWSTATS
ENDPROC

PROC PERFORM_HIT 
    CARDSPACING = 16    
    PLAYERNUMBER = PLAYERNO
    N = PLAYERNDRAWN(PLAYERNUMBER)
    IF PLAYERNUMBER = 0 
         N = PLAYERNDRAWN(0)
         G = 0                  
         IF SELECTED_RES_Y < 640
            H = 58 + 10 * N
         ELSE
            H = 64 + 12 * N
         ENDIF
    ELSE                 
         IF PLAYERNSPLIT(PLAYERNO) >= 2 THEN CARDSPACING = 14
         IF PLAYERNSPLIT(PLAYERNO) < 3 
            G = DISPPLAYERLEFT + (PLAYERNO - 1) * DISPPLAYERSPACING + 10 + CARDSPACING * N
            H = DISPPLAYERTOP + DISPPLAYERVERTICAL * 0 + 12 * N
            IF SELECTED_RES_Y < 640 THEN H = DISPPLAYERTOP + DISPPLAYERVERTICAL * 0 + 10 * N
         ELIF PLAYERNSPLIT(PLAYERNO) = 3 
            N = PLAYERNDRAWN(PLAYERNUMBER) - 7
            G = DISPPLAYERLEFT + (PLAYERNO - 1) * DISPPLAYERSPACING + 78 + INT(DISPPLAYERSPACING * .11) + CARDSPACING * N
            H = DISPPLAYERTOP + DISPPLAYERVERTICAL * 0 + 12 * N
            IF SELECTED_RES_Y < 640 THEN H = DISPPLAYERTOP + DISPPLAYERVERTICAL * 0 + 10 * N
         ENDIF         
    ENDIF
    EXEC ANIMATEHIT
    NEWCARDVALUE = BJCARDVALUE(CARDORDER(CARDNO))
    PLAYERBJCARDVAL(PLAYERNO*20+PLAYERNDRAWN(PLAYERNO)) = NEWCARDVALUE
    PLAYERPKCARDVAL(PLAYERNO*20+PLAYERNDRAWN(PLAYERNO)) = PKCARDVALUE(CARDORDER(CARDNO))
    PLAYERSUCARDVAL(PLAYERNO*20+PLAYERNDRAWN(PLAYERNO)) = CARDSUITVALUE(CARDORDER(CARDNO))
    CARDNO = CARDNO + 1
    PLAYERNDRAWN(PLAYERNO) = PLAYERNDRAWN(PLAYERNO) + 1
    EXEC CALCTOTAL
    IF PLAYERNSPLIT(PLAYERNO) <> 3 
        ' BLACKJACKFORM.PLAYERTOTALSHOW(PLAYERNO).TEXT = CSTR(PLAYERNTOTAL(PLAYERNO+0))
    ELSE
        'BLACKJACKFORM.PLAYERTOTALSHOW(PLAYERNO).TEXT = PLAYERNTOTAL(PLAYERNO+4) & "|T:" & PLAYERNTOTAL(PLAYERNO*200)
    ENDIF
    IF PLAYERNTOTAL(PLAYERNO*200) >= 22 
        IF PLAYERNSPLIT(PLAYERNO) < 2 
            PLAYERNMESSAGE$(PLAYERNO) = "TOTAL:" 
            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+0)) 
            PLAYERNMESSAGE$(PLAYERNO) =+ "  BUSTED"
            GONEXTPLAYER = TRUE
        ENDIF
        IF PLAYERNSPLIT(PLAYERNO) >= 2 AND PLAYERNTOTAL(PLAYERNO+4) < 22
            PLAYERNMESSAGE$(PLAYERNO) = "T:" 
            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+0)) 
            PLAYERNMESSAGE$(PLAYERNO) =+ " BUST|T:"  
            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+4))       
            IF PLAYERNSPLIT(PLAYERNO) = 2 THEN GONEXTPLAYER = TRUE
        ENDIF
        IF PLAYERNSPLIT(PLAYERNO) = 3 AND PLAYERNTOTAL(PLAYERNO+4) >= 22
            PLAYERNMESSAGE$(PLAYERNO) = "T:" 
            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+0)) 
            PLAYERNMESSAGE$(PLAYERNO) =+ "|T:"  
            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+4)) 
            PLAYERNMESSAGE$(PLAYERNO) =+ " BUST"
        ENDIF
    ELSE
        IF PLAYERNSPLIT(PLAYERNO) < 2 
            PLAYERNMESSAGE$(PLAYERNO) = "TOTAL:" 
            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+0))
        ELSE
            PLAYERNMESSAGE$(PLAYERNO) = "T:" 
            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+0)) 
            PLAYERNMESSAGE$(PLAYERNO) =+ "|T:"  
            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+4))
        ENDIF
        IF (PLAYERNDRAWN(PLAYERNO) = 6 OR PLAYERNDRAWN(PLAYERNO) = 13) AND CARDCHARLIEWINSET = 6
            IF PLAYERNSPLIT(PLAYERNO) < 2 
                PLAYERNMESSAGE$(PLAYERNO) = "TOTAL:" 
                PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+0)) 
                PLAYERNMESSAGE$(PLAYERNO) =+ " SIX CARD CHARLIE"
            ELSE
                PLAYERNMESSAGE$(PLAYERNO) = "T:" 
                PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+0)) 
                PLAYERNMESSAGE$(PLAYERNO) =+ " C6|T:"  
                PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+4))
            ENDIF
            GONEXTPLAYER = TRUE
        ENDIF
        IF (PLAYERNDRAWN(PLAYERNO) = 5 OR PLAYERNDRAWN(PLAYERNO) = 12) AND CARDCHARLIEWINSET = 5
            IF PLAYERNSPLIT(PLAYERNO) < 2 
                PLAYERNMESSAGE$(PLAYERNO) = "TOTAL:" 
                PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+0)) 
                PLAYERNMESSAGE$(PLAYERNO) =+ " FIVE CARD CHARLIE"
            ELSE
                PLAYERNMESSAGE$(PLAYERNO) = "T:" 
                PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+0)) 
                PLAYERNMESSAGE$(PLAYERNO) =+ " C5|T:"  
                PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+4))
            ENDIF
            GONEXTPLAYER = TRUE
        ENDIF
    ENDIF
ENDPROC

PROC PERFORM_REWARDS
    SPLITREWARD = 0
    PLAYERNUMBER = 0
    BONUSREWARD = 0
    SPLITBASE = 0   
    HANDNUMBER = 0
    HANDTOTAL = 0
    HANDREWARD(0) = 0
    HANDREWARD(1) = 0
    HANDREWARD(2) = 0
    HANDREWARD(3) = 0
    FOR PLAYERNUMBER = 1 TO LASTPLAYER
        PLAYERNO = PLAYERNUMBER
        HANDNUMBER = 0
        SPLITBASE = 0
        REPEAT
            IF PLAYERNDRAWN(PLAYERNO) >= 2
                PLAYERNDRAWN(PLAYERNO) = 0
                HANDREWARD(HANDNUMBER) = 0
                FOR C = 0 TO 6
                    IF PLAYERBJCARDVAL(PLAYERNO*20+SPLITBASE + C) > 0 THEN PLAYERNDRAWN(PLAYERNO) = PLAYERNDRAWN(PLAYERNO) + 1
                NEXT C
                IF HANDNUMBER = 0 THEN HANDTOTAL = PLAYERNTOTAL(PLAYERNO+0)
                IF HANDNUMBER = 1 THEN HANDTOTAL = PLAYERNTOTAL(PLAYERNO+4)
                IF HANDNUMBER = 2 THEN HANDTOTAL = PLAYERNTOTAL(PLAYERNO+8)
            ENDIF
            IF HANDTOTAL = 21 
                IF PLAYERNDRAWN(PLAYERNO) = 2 
                    C = 0
                    IF COLOR21BONUS = 1 
                        IF PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  0) = 2 OR PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  0) = 4 THEN C = C + 1
                        IF PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  1) = 2 OR PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  1) = 4 THEN C = C + 1
                        IF C = 2 
                            HANDREWARD(HANDNUMBER) = PLAYERNBET(PLAYERNO) * 2
                            IF HANDNUMBER = 0 
                               PLAYERNMESSAGE$(PLAYERNO) = "BLACKJACK BLACK  WON:" 
                               PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO) * 2)
                            ENDIF
                            PLAYERNWINS(PLAYERNO) = PLAYERNWINS(PLAYERNO) + 1
                        ELSE
                            C = 0
                            IF PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  0) = 1 OR PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  0) = 3 THEN C = C + 1
                            IF PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  1) = 1 OR PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  1) = 3 THEN C = C + 1
                            IF C = 2 
                                HANDREWARD(HANDNUMBER) =PLAYERNBET(PLAYERNO) * 2
                                IF HANDNUMBER = 0
                                    PLAYERNMESSAGE$(PLAYERNO) = "BLACKJACK BLACK  WON:" 
                                    PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO) * 2)
                                ENDIF
                                PLAYERNWINS(PLAYERNO) = PLAYERNWINS(PLAYERNO) + 1
                            ELSE
                                C = 0
                            ENDIF
                        ENDIF
                    ENDIF
                    IF C = 0 
                        IF SUIT21BONUS = 1 AND PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  0) = PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  1) 
                            HANDREWARD(HANDNUMBER) = PLAYERNBET(PLAYERNO) * 2
                            IF HANDNUMBER = 0
                                PLAYERNMESSAGE$(PLAYERNO) = "SUITED BLACKJACK   WON:" 
                                PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO) * 2)
                            ENDIF
                            PLAYERNWINS(PLAYERNO) = PLAYERNWINS(PLAYERNO) + 1
                        ELSE
                            HANDREWARD(HANDNUMBER) = INT(PLAYERNBET(PLAYERNO) * 1.5)
                            IF HANDNUMBER = 0 
                              PLAYERNMESSAGE$(PLAYERNO) = "BLACKJACK   WON:" 
                              PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO) * 1.5)
                            ENDIF
                            PLAYERNWINS(PLAYERNO) = PLAYERNWINS(PLAYERNO) + 1
                        ENDIF
                    ENDIF
                ENDIF             
                IF PLAYERNDRAWN(PLAYERNO) = 3            
                    BONUSREWARD = 0           
                    IF BONUS777 = 1                               
                        C  = 0
                        IF PLAYERBJCARDVAL(PLAYERNO*20+SPLITBASE +  0) = 7 THEN C = C + 1
                        IF PLAYERBJCARDVAL(PLAYERNO*20+SPLITBASE +  1) = 7 THEN C = C + 1
                        IF PLAYERBJCARDVAL(PLAYERNO*20+SPLITBASE +  2) = 7 THEN C = C + 1                
                        IF C = 3
                            IF PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  0) = PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  1) AND PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  0) = PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  2) 
                                IF HANDNUMBER = 0 
                                    PLAYERNMESSAGE$(PLAYERNO) = "21  SUITED 777 BONUS    WON:" 
                                    PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO) * 4)
                                ENDIF
                                HANDREWARD(HANDNUMBER) =PLAYERNBET(PLAYERNO) * 4
                                BONUSREWARD = 1
                                PLAYERNWINS(PLAYERNO) = PLAYERNWINS(PLAYERNO) + 1
                            ELSE
                                IF HANDNUMBER = 0 
                                    PLAYERNMESSAGE$(PLAYERNO) = "21  777 BONUS   WON:" 
                                    PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO) * 3)
                                ENDIF    
                                HANDREWARD(HANDNUMBER) = PLAYERNBET(PLAYERNO) * 3
                                BONUSREWARD = 1
                                PLAYERNWINS(PLAYERNO) = PLAYERNWINS(PLAYERNO) + 1
                            ENDIF
                        ELSE
                            IF PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  0) = PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  1) AND PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  0) = PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  2) AND SUIT21BONUS = TRUE AND BONUSREWARD = FALSE
                                IF HANDNUMBER = 0 
                                    PLAYERNMESSAGE$(PLAYERNO) = "SUITED 21 BONUS    WON:"
                                    PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO) * 2)
                                ENDIF
                                HANDREWARD(HANDNUMBER) =PLAYERNBET(PLAYERNO) * 2
                                BONUSREWARD = 1                    
                                PLAYERNWINS(PLAYERNO) = PLAYERNWINS(PLAYERNO) + 1
                            ENDIF
                        ENDIF                
                    ENDIF 
                    IF BONUS678 = 1 AND BONUSREWARD = 0
                        C  = 0
                        IF PLAYERBJCARDVAL(PLAYERNO*20+SPLITBASE +  0) = 6 OR PLAYERBJCARDVAL(PLAYERNO*20+SPLITBASE +  1)  = 6 OR PLAYERBJCARDVAL(PLAYERNO*20+SPLITBASE +  2)  = 6 THEN C = C + 1
                        IF PLAYERBJCARDVAL(PLAYERNO*20+SPLITBASE +  0) = 7 OR PLAYERBJCARDVAL(PLAYERNO*20+SPLITBASE +  1)  = 7 OR PLAYERBJCARDVAL(PLAYERNO*20+SPLITBASE +  2)  = 7 THEN C = C + 1
                        IF PLAYERBJCARDVAL(PLAYERNO*20+SPLITBASE +  0) = 8 OR PLAYERBJCARDVAL(PLAYERNO*20+SPLITBASE +  1)  = 8 OR PLAYERBJCARDVAL(PLAYERNO*20+SPLITBASE +  2)  = 8 THEN C = C + 1
                        IF C = 3 
                            IF PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  0) = PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  1) AND PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  0) = PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  2) 
                                IF HANDNUMBER = 0 
                                    PLAYERNMESSAGE$(PLAYERNO) = "21  SUITED 678 BONUS   WON:" 
                                    PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO) * 4)
                                ENDIF
                                HANDREWARD(HANDNUMBER) = PLAYERNBET(PLAYERNO) * 4
                                BONUSREWARD = 1
                                PLAYERNWINS(PLAYERNO) = PLAYERNWINS(PLAYERNO) + 1
                            ELSE  
                                IF HANDNUMBER = 0
                                    PLAYERNMESSAGE$(PLAYERNO) = "21  678 BONUS  WON:" 
                                    PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO) * 3)
                                ENDIF
                                HANDREWARD(HANDNUMBER) = PLAYERNBET(PLAYERNO) * 3
                                BONUSREWARD = 1
                                PLAYERNWINS(PLAYERNO) = PLAYERNWINS(PLAYERNO) + 1
                            ENDIF
                        ELSE
                            IF PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  0) = PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  1) AND PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  0) = PLAYERSUCARDVAL(PLAYERNO*20+SPLITBASE +  2) AND SUIT21BONUS = TRUE AND BONUSREWARD = FALSE
                                IF HANDNUMBER = 0 
                                    PLAYERNMESSAGE$(PLAYERNO) = "SUITED 21 BONUS  WON:" 
                                    PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO) * 2)
                                ENDIF
                                HANDREWARD(HANDNUMBER) = PLAYERNBET(PLAYERNO) * 2
                                BONUSREWARD = 1                 
                                PLAYERNWINS(PLAYERNO) = PLAYERNWINS(PLAYERNO) + 1
                            ENDIF
                        ENDIF
                    ENDIF 
                    IF BONUSREWARD = 0 OR PLAYERNDRAWN(PLAYERNO) >=4 
                        IF (HANDTOTAL > PLAYERNTOTAL(0) OR PLAYERNTOTAL(0) >= 22)  
                            IF HANDNUMBER = 0 
                                PLAYERNMESSAGE$(PLAYERNO) = "TOTAL :" 
                                PLAYERNMESSAGE$(PLAYERNO) =+ STR$(HANDTOTAL) 
                                PLAYERNMESSAGE$(PLAYERNO) =+ "  WON:" 
                                PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO))
                            ENDIF
                            HANDREWARD(HANDNUMBER) = PLAYERNBET(PLAYERNO) 
                            PLAYERNWINS(PLAYERNO) = PLAYERNWINS(PLAYERNO) + 1
                        ENDIF
                        IF HANDTOTAL = PLAYERNTOTAL(0) 
                            IF HANDNUMBER = 0 
                                PLAYERNMESSAGE$(PLAYERNO) = "TOTAL :" 
                                PLAYERNMESSAGE$(PLAYERNO) =+ STR$(HANDTOTAL) 
                                PLAYERNMESSAGE$(PLAYERNO) =+ "    PUSH"
                            ENDIF
                            HANDREWARD(HANDNUMBER) = 0
                        ENDIF
                    ENDIF            
                 ENDIF
            ELIF HANDTOTAL >= 22 
                  IF HANDNUMBER = 0
                        PLAYERNMESSAGE$(PLAYERNO) = STR$(HANDTOTAL) 
                        PLAYERNMESSAGE$(PLAYERNO) =+" BUSTED  LOST:" 
                        PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO))
                  ENDIF 
                  HANDREWARD(HANDNUMBER) =- PLAYERNBET(PLAYERNO) 
            ELIF PLAYERNDRAWN(PLAYERNO) <= -1 
                  IF HANDNUMBER = 0 
                        PLAYERNMESSAGE$(PLAYERNO) = "SURRENDERED  LOST:" 
                        PLAYERNMESSAGE$(PLAYERNO) =+ STR$(INT(PLAYERNBET(PLAYERNO)/2))
                  ENDIF
                  HANDREWARD(HANDNUMBER) = - INT(PLAYERNBET(PLAYERNO) / 2)
            ELIF HANDTOTAL < 22 
                IF PLAYERNDRAWN(PLAYERNO) >= CARDCHARLIEWINSET AND PLAYERNDRAWN(PLAYERNO) <= 7
                    IF HANDNUMBER = 0
                        IF CARDCHARLIEWINSET = 5 
                            PLAYERNMESSAGE$(PLAYERNO) = "FIVE CARD CHARLIE  WON:" 
                            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO))
                        ENDIF
                        IF CARDCHARLIEWINSET = 6
                            PLAYERNMESSAGE$(PLAYERNO) = "SIX CARD CHARLIE  WON:" 
                            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO))
                        ENDIF
                    ENDIF
                    HANDREWARD(HANDNUMBER) = PLAYERNBET(PLAYERNO) 
                    PLAYERNWINS(PLAYERNO) = PLAYERNWINS(PLAYERNO) + 1
                ELSE
                    IF (HANDTOTAL > PLAYERNTOTAL(0) OR PLAYERNTOTAL(0) >= 22 OR PLAYERNDRAWN(PLAYERNO) >= CARDCHARLIEWINSET)
                        HANDREWARD(HANDNUMBER) = PLAYERNBET(PLAYERNO) 
                        IF HANDNUMBER = 0
                            PLAYERNMESSAGE$(PLAYERNO) = "TOTAL:" 
                            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(HANDTOTAL) 
                            PLAYERNMESSAGE$(PLAYERNO) =+ "   WON:" 
                            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO))
                        ENDIF
                        PLAYERNWINS(PLAYERNO) = PLAYERNWINS(PLAYERNO) + 1
                    ENDIF
                    IF HANDTOTAL = PLAYERNTOTAL(0) AND PLAYERNTOTAL(0) < 22                      
                        IF DEALERTIEWINS = 1 AND PLAYERNTOTAL(0) = 17                     
                            IF HANDNUMBER = 0  
                                PLAYERNMESSAGE$(PLAYERNO) = "TIE:" 
                                PLAYERNMESSAGE$(PLAYERNO) =+ STR$(HANDTOTAL) 
                                PLAYERNMESSAGE$(PLAYERNO) =+ "  LOST:" 
                                PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO))
                            ENDIF
                            HANDREWARD(HANDNUMBER) =- PLAYERNBET(PLAYERNO) 
                        ELSE
                            PLAYERNMESSAGE$(PLAYERNO) = "TOTAL:" 
                            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(HANDTOTAL) 
                            PLAYERNMESSAGE$(PLAYERNO) =+ "    PUSH"
                            HANDREWARD(HANDNUMBER) = 0
                        ENDIF
                    ENDIF                        
                    IF HANDTOTAL < PLAYERNTOTAL(0) AND PLAYERNTOTAL(0) < 22
                        IF HANDNUMBER = 0  
                            PLAYERNMESSAGE$(PLAYERNO) = "TOTAL:" 
                            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(HANDTOTAL) 
                            PLAYERNMESSAGE$(PLAYERNO) =+ "  LOST:" 
                            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNBET(PLAYERNO))
                        ENDIF
                        HANDREWARD(HANDNUMBER) =- PLAYERNBET(PLAYERNO) 
                    ENDIF
                ENDIF
            ENDIF
            IF HANDNUMBER = 1
                PLAYERNMESSAGE$(PLAYERNO) = "T:" 
                PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+0))
                IF HANDREWARD(0) > 0 
                    PLAYERNMESSAGE$(PLAYERNO) =+ " W:" 
                    PLAYERNMESSAGE$(PLAYERNO) =+ STR$(HANDREWARD(0))
                ENDIF
                IF HANDREWARD(0) < 0 
                  PLAYERNMESSAGE$(PLAYERNO) =+ " L:" 
                  PLAYERNMESSAGE$(PLAYERNO) =+ STR$(ABS(HANDREWARD(0)))
                ENDIF
                IF HANDREWARD(0) = 0
                    PLAYERNMESSAGE$(PLAYERNO) =+ " P"
                ENDIF
                PLAYERNMESSAGE$(PLAYERNO) =+ "|T:" 
                PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+4))
                IF HANDREWARD(1) > 0  
                    PLAYERNMESSAGE$(PLAYERNO) =+" W:" 
                    PLAYERNMESSAGE$(PLAYERNO) =+STR$(HANDREWARD(1))
                ENDIF
                IF HANDREWARD(1) < 0
                    PLAYERNMESSAGE$(PLAYERNO) =+" L:" 
                    PLAYERNMESSAGE$(PLAYERNO) =+STR$(ABS(HANDREWARD(1)))
                ENDIF
                IF HANDREWARD(1) = 0
                    PLAYERNMESSAGE$(PLAYERNO) =+ " P"
                ENDIF
            ENDIF
            SPLITBASE = SPLITBASE + 7
            IF PLAYERNSPLIT(PLAYERNO) <=2 OR PLAYERNSPLIT(PLAYERNO) =4 
                    HANDNUMBER = 2
            ELIF PLAYERNSPLIT(PLAYERNO) =3
                    IF HANDNUMBER = 0 
                        HANDNUMBER = 1
                    ELSE
                        HANDNUMBER = 2
                    ENDIF
            ELIF PLAYERNSPLIT(PLAYERNO) =5
                    HANDNUMBER = HANDNUMBER + 1
            ENDIF
        UNTIL HANDNUMBER >=2
        PLAYERNCASH(PLAYERNUMBER) = PLAYERNCASH(PLAYERNUMBER) + HANDREWARD(0) + HANDREWARD(1)
    NEXT PLAYERNUMBER
    ' BLACKJACKFORM.COMMANDDEAL.VISIBLE = TRUE
    ROUNDSPLAYED = ROUNDSPLAYED + 1
ENDPROC

PROC PERFORM_SPLITDEAL 
    CARDSPACING = 14
    IF PLAYERNSPLIT(PLAYERNO) = 1
        FOR N = 7 TO 13
            PLAYERBJCARDVAL(PLAYERNO*20+N) = 0
            PLAYERPKCARDVAL(PLAYERNO*20+N) = 0
        NEXT N
        IF PLAYERNO >= 1 AND PLAYERNO <= 5
            G = DISPPLAYERLEFT + (PLAYERNO - 1) * DISPPLAYERSPACING + 78 + INT(DISPPLAYERSPACING * .11) 
            H = DISPPLAYERTOP + DISPPLAYERVERTICAL * 0 
            PLAYERBJCARDVAL(PLAYERNO*20+7) = PLAYERBJCARDVAL(PLAYERNO*20+1)        
            PLAYERPKCARDVAL(PLAYERNO*20+7) = PKCARDVALUE(CARDORDER(CARDNO))
            FOR N = 1 TO 6 
                PLAYERBJCARDVAL(PLAYERNO*20+N) = 0
                PLAYERPKCARDVAL(PLAYERNO*20+N) = 0
            NEXT N
            PLAYERNDRAWN(PLAYERNO) = 1
            N = PLAYERNSPLITCARD(PLAYERNO)
            ' ****   SPRITEN+1, G, H, CARDIMAGE(N)        
            G = DISPPLAYERLEFT + (PLAYERNO - 1) * DISPPLAYERSPACING + 10 + CARDSPACING * 1
            H = DISPPLAYERTOP + DISPPLAYERVERTICAL * 0 + 12 * 1
            EXEC ANIMATEHIT
            NEWCARDVALUE = BJCARDVALUE(CARDORDER(CARDNO))
            PLAYERBJCARDVAL(PLAYERNO*20+1) = BJCARDVALUE(CARDORDER(CARDNO))
            PLAYERSUCARDVAL(PLAYERNO*20+1) = CARDSUITVALUE(CARDORDER(CARDNO))                
            CARDNO = CARDNO + 1
            PLAYERNDRAWN(PLAYERNO) = 2
            PLAYERNSPLIT(PLAYERNO) = 2        
            EXEC CALCTOTAL
            G = DISPPLAYERLEFT + (PLAYERNO - 1) * DISPPLAYERSPACING + 78 + INT(DISPPLAYERSPACING * .11) + CARDSPACING * 1
            H = DISPPLAYERTOP + DISPPLAYERVERTICAL * 0 + 12 * 1
            EXEC ANIMATEHIT
            PLAYERBJCARDVAL(PLAYERNO*20+8) = BJCARDVALUE(CARDORDER(CARDNO))
            PLAYERSUCARDVAL(PLAYERNO*20+8) = CARDSUITVALUE(CARDORDER(CARDNO))                
            CARDNO = CARDNO + 1        
            PLAYERNSPLIT(PLAYERNO) = 3
            PLAYERNDRAWN(PLAYERNO) = 9
            EXEC CALCTOTAL
            PLAYERNMESSAGE$(PLAYERNO) = STR$(PLAYERNTOTAL(PLAYERNO+0)) 
            PLAYERNMESSAGE$(PLAYERNO) =+ "|T:" 
            PLAYERNMESSAGE$(PLAYERNO) =+ STR$(PLAYERNTOTAL(PLAYERNO+4))
        ENDIF
        PLAYERNSPLIT(PLAYERNO) = 2
        PLAYERNDRAWN(PLAYERNO) = 2
        EXEC SHOWSTATS
     ELSE
      IF PLAYERNSPLIT(PLAYERNO) = 2 OR PLAYERNSPLIT(PLAYERNO) = 3
            FOR N = 14 TO 20
                PLAYERBJCARDVAL(PLAYERNO*20+N) = 0
            NEXT N
            G = DISPPLAYERLEFT + (PLAYERNO - 1) * DISPPLAYERSPACING + 148 + INT(DISPPLAYERSPACING * .17) 
            H = DISPPLAYERTOP + DISPPLAYERVERTICAL * 0 
            IF PLAYERNSPLIT(PLAYERNO) = 2
                PLAYERBJCARDVAL(PLAYERNO*20+7) = PLAYERBJCARDVAL(PLAYERNO*20+1)        
                FOR N = 1 TO 6 
                    PLAYERBJCARDVAL(PLAYERNO*20+N) = 0
                NEXT N
                PLAYERNDRAWN(PLAYERNO) = 1
            ENDIF     
            IF PLAYERNSPLIT(PLAYERNO) = 3
                PLAYERBJCARDVAL(PLAYERNO*20+14) = PLAYERBJCARDVAL(PLAYERNO*20+8)        
                FOR N = 8 TO 13
                    PLAYERBJCARDVAL(PLAYERNO*20+N) = 0
                NEXT N
                PLAYERNDRAWN(PLAYERNO) = 8
            ENDIF   
      ENDIF
     ENDIF
ENDPROC

PROC PERFORM_NEXTHAND    
    IF PLAYERNSPLIT(PLAYERNO) = 2 
        PLAYERNSPLIT(PLAYERNO) = 3
        PLAYERNDRAWN(PLAYERNO) = 9
    ENDIF
    IF PLAYERNSPLIT(PLAYERNO) = 3 AND PLAYERBJCARDVAL(PLAYERNO*20+14) > 0
        PLAYERNSPLIT(PLAYERNO) = 4
        PLAYERNDRAWN(PLAYERNO) = 15
    ENDIF
    EXEC CALCTOTAL
ENDPROC

PROC GETBETS    
    GetBetPlayerNumber = 0
    DisplayBetPlayerNumber = 0
    N = 0
    BETIN$ = ""
    BETOK = 0
    BETVAL = 0
    ERRORMESSAGE$ = "" 
    QUITBOXLEFT = 0
    CLS
    BETIN$ = ""   
    FOR GetBetPlayerNumber = 1 TO LASTPLAYER        
        ERRORMESSAGE$ = ""
        REPEAT
            BETOK = FALSE            
            FOR DisplayBetPlayerNumber = 1 TO LASTPLAYER
                ' ****   CENTER TEXT DISPPLAYERLEFT+84+DISPPLAYERSPACING*(DisplayBetPlayerNumber-1), DISPPLAYERTOP + DISPPLAYERVERTICAL * 6,  "PLAYER : " + STR$(DisplayBetPlayerNumber)
                ' ****   CENTER TEXT DISPPLAYERLEFT+84+DISPPLAYERSPACING*(DisplayBetPlayerNumber-1), DISPPLAYERTOP + DISPPLAYERVERTICAL * 7,  "CASH : " + STR$(PLAYERNCASH(DisplayBetPlayerNumber))                
                IF GetBetPlayerNumber <> DisplayBetPlayerNumber                 
                    ' ****   TEXT DISPPLAYERLEFT+12+DISPPLAYERSPACING*(DisplayBetPlayerNumber-1),DISPPLAYERTOP + DISPPLAYERVERTICAL * 8, "BET : " + STR$(PLAYERNBET(DisplayBetPlayerNumber))
                ENDIF
            NEXT DisplayBetPlayerNumber            
            ' ****   CENTER TEXT QUITBOXLEFT + 40 ,8 , "UIT"
            ' ****   CENTER TEXT QUITBOXLEFT + 22 ,4, "Q" 
            BETIN$ = ""
            INPUT BETIN$       
            IF BETIN$="" AND EXITGAME = 0
              ERRORMESSAGE$ = "You Must Enter a Number."
            ELIF BETIN$="" AND EXITGAME = 1
              EXITGAME = 2
            ELSE    
              IF BETIN$="Q" 
                  ERRORMESSAGE$ = "Quit? Press Enter to Confirm."
                  EXITGAME = 1
              ELSE
                EXITGAME = 0
                BETVAL = VAL(BETIN$)
                IF BETVAL > 9 AND BETVAL<1001 
                  BETOK = 1
                  ERRORMESSAGE$ = ""
                  PLAYERNBET(GetBetPlayerNumber) = BETVAL                
                ELIF BETVAL>1000 
                   ERRORMESSAGE$ = "The Maximum Bet is 1000."
                ELIF BETVAL<10 
                   ERRORMESSAGE$ = "The Minimal Bet is 10."
                ENDIF
                IF ERRORMESSAGE$ <> "" 
                    BETOK = 0
                ENDIF
              ENDIF
            ENDIF            
            ' ****  TEXT DISPPLAYERLEFT+62+DISPPLAYERSPACING*(GetBetPlayerNumber-1),DISPPLAYERTOP + DISPPLAYERVERTICAL * 8, ENTRY$(1)
            ' ****  TEXT DISPPLAYERLEFT+12+DISPPLAYERSPACING*(GetBetPlayerNumber-1),DISPPLAYERTOP + DISPPLAYERVERTICAL * 8, "BET"            
        UNTIL BETOK = 1 OR EXITGAME = 2
    NEXT GetBetPlayerNumber
ENDPROC

PROC SHOWSTATS
    DisplayBetPlayerNumber = 0
    DISPDEALERLEFT = 0
    ' TEXT 2, 2,  "CARDNO : " + STR$(CARDNO) + " / " + STR$(TOTALCARDS)
    FOR DisplayBetPlayerNumber = 0 TO LASTPLAYER        
        IF DisplayBetPlayerNumber = 0 
            ' ****   CENTER TEXT DISPDEALERLEFT, 16,  "DEALER"
            ' ****   CENTER TEXT DISPDEALERLEFT, 17 + DISPPLAYERVERTICAL, PLAYERNMESSAGE$(DisplayBetPlayerNumber)            
        ELSE
            ' ****   CENTER TEXT DISPPLAYERLEFT+84+DISPPLAYERSPACING*(DisplayBetPlayerNumber-1), DISPPLAYERTOP + DISPPLAYERVERTICAL * 6,  "PLAYER : " + STR$(DisplayBetPlayerNumber)
            ' ****   CENTER TEXT DISPPLAYERLEFT+84+DISPPLAYERSPACING*(DisplayBetPlayerNumber-1), DISPPLAYERTOP + DISPPLAYERVERTICAL * 7,  "CASH : " + STR$(PLAYERNCASH(DisplayBetPlayerNumber))
            ' ****   CENTER TEXT DISPPLAYERLEFT+78+DISPPLAYERSPACING*(DisplayBetPlayerNumber-1),DISPPLAYERTOP + DISPPLAYERVERTICAL * 8, "BET : " + STR$(PLAYERNBET(DisplayBetPlayerNumber))
            IF LEN(PLAYERNMESSAGE$(DisplayBetPlayerNumber)) < 22
                ' ****   CENTER TEXT DISPPLAYERLEFT+78+DISPPLAYERSPACING*(DisplayBetPlayerNumber-1),DISPPLAYERTOP + DISPPLAYERVERTICAL * 9, PLAYERNMESSAGE$(DisplayBetPlayerNumber)
            ELSE
                ' ****   TEXT DISPPLAYERLEFT+DISPPLAYERSPACING*(DisplayBetPlayerNumber-1) - DISPPLAYERLEFT + 2,DISPPLAYERTOP + DISPPLAYERVERTICAL * 9, PLAYERNMESSAGE$(DisplayBetPlayerNumber)
            ENDIF
        ENDIF
    NEXT DisplayBetPlayerNumber
ENDPROC

PROC SHOWCOMMANDS    
    SPLITEXTRALEFT = 0
    DisplayBetPlayerNumber = 0
    IF PLAYERNSPLIT(PLAYERNO) = 3 OR PLAYERNSPLIT(PLAYERNO) = 4 
        SPLITEXTRALEFT = 0
    ELSE
        SPLITEXTRALEFT = 0
    ENDIF
    DisplayBetPlayerNumber = PLAYERNO - 1    
    ' ****   CENTER TEXT DISPPLAYERLEFT + SPLITEXTRALEFT  + 68 + DISPPLAYERSPACING * DisplayBetPlayerNumber, DISPPLAYERTOP + DISPPLAYERVERTICAL * 10, "IT"    
    ' ****   CENTER TEXT DISPPLAYERLEFT + SPLITEXTRALEFT  + 48 + DISPPLAYERSPACING * DisplayBetPlayerNumber, DISPPLAYERTOP + DISPPLAYERVERTICAL * 10 - 3, "H"
    ' ****   CENTER TEXT DISPPLAYERLEFT + SPLITEXTRALEFT  + 180 + DISPPLAYERSPACING * DisplayBetPlayerNumber,DISPPLAYERTOP + DISPPLAYERVERTICAL * 10, "TAND"
    ' ****   CENTER TEXT DISPPLAYERLEFT + SPLITEXTRALEFT  + 149 + DISPPLAYERSPACING * DisplayBetPlayerNumber,DISPPLAYERTOP + DISPPLAYERVERTICAL * 10 - 4, "S"
    IF PLAYERNDOUBLEUSED(PLAYERNO) = 0 
        ' ****   CENTER TEXT DISPPLAYERLEFT + SPLITEXTRALEFT  + 64 + DISPPLAYERSPACING * DisplayBetPlayerNumber,DISPPLAYERTOP + DISPPLAYERVERTICAL * 11, "OUBLE"
        ' ****   CENTER TEXT DISPPLAYERLEFT + SPLITEXTRALEFT  + 24 + DISPPLAYERSPACING * DisplayBetPlayerNumber,DISPPLAYERTOP + DISPPLAYERVERTICAL * 11 - 4, "D"
    ENDIF    
    IF PLAYERNSPLIT(PLAYERNO) = 1 AND PLAYERNDRAWN(PLAYERNO) <= 2
        ' ****   CENTER TEXT DISPPLAYERLEFT + SPLITEXTRALEFT  + 162 + DISPPLAYERSPACING * DisplayBetPlayerNumber,DISPPLAYERTOP + DISPPLAYERVERTICAL * 11, "SPLI"
        ' ****   CENTER TEXT DISPPLAYERLEFT + SPLITEXTRALEFT  + 190 + DISPPLAYERSPACING * DisplayBetPlayerNumber,DISPPLAYERTOP + DISPPLAYERVERTICAL * 11 - 4, "T"
    ENDIF    
    IF ALLOWSURRENDER = TRUE AND PLAYERNDRAWN(PLAYERNO) = 2 AND PLAYERNSPLIT(PLAYERNO) <= 1
        ' ****   CENTER TEXT DISPPLAYERLEFT + SPLITEXTRALEFT  + 72 + DISPPLAYERSPACING * DisplayBetPlayerNumber,DISPPLAYERTOP + DISPPLAYERVERTICAL * 12, "S   RRENDER"
        ' ****   CENTER TEXT DISPPLAYERLEFT + SPLITEXTRALEFT  + 35 + DISPPLAYERSPACING * DisplayBetPlayerNumber,DISPPLAYERTOP + DISPPLAYERVERTICAL * 12 - 4, "U"        
    ENDIF
    IF PLAYERNSPLIT(PLAYERNO) = 2 
        ' ****   CENTER TEXT DISPPLAYERLEFT + SPLITEXTRALEFT  + 180 + DISPPLAYERSPACING * DisplayBetPlayerNumber,DISPPLAYERTOP + DISPPLAYERVERTICAL * 12, "HAND 1"
    ENDIF        
    IF PLAYERNSPLIT(PLAYERNO) = 3
        ' ****   CENTER TEXT DISPPLAYERLEFT + SPLITEXTRALEFT  + 180 + DISPPLAYERSPACING * DisplayBetPlayerNumber,DISPPLAYERTOP + DISPPLAYERVERTICAL * 12, "HAND 2"    
    ENDIF        
    IF PLAYERNSPLIT(PLAYERNO) = 4
        ' ****   CENTER TEXT DISPPLAYERLEFT + SPLITEXTRALEFT  + 180 + DISPPLAYERSPACING * DisplayBetPlayerNumber,DISPPLAYERTOP + DISPPLAYERVERTICAL * 12, "HAND 3"    
    ENDIF        
ENDPROC

PROC SHUFFLEDECKS
    B = 0
    I = 0
    J = 0
    K = 0
    L = 0
    N = 0
    M = 0
    O = 0
    ' ****   F AS FLOAT
    H = 0
    G0 = 0
    G1 = 0
    G2 = 0
    ' ****   DIS$ = ""
    
    ' ****   DIS$ = MID$(GET DATE$,5) + MID$(GET DATE$,4) + MID$(GET DATE$, 2) + MID$(GET DATE$,8) 
    ' ****   J = VAL(DIS$)
    
    ' ****   G1 = SELECTED_RES_Y * .2 + 24
    ' ****   F = (SELECTED_RES_X * 0.6) / TOTALCARDS
    FOR I = 0 TO TOTALCARDS + JOKERSPRESANT
        CARDORDER(I) = -1
        ' ****   SPRITEI+1, SELECTED_RES_X * .18 + F * I  , G1 , CARDBACKIMAGE
    NEXT I
    REPEAT
        J = 5 + RAND(3) * 2
    UNTIL (TOTALCARDS MOD J) > 0 AND ((TOTALCARDS + 1) MOD J) <> 2 AND ((TOTALCARDS + 1) MOD J) <> 3 AND ((TOTALCARDS + 1) MOD J) <> 4
    CARDNO = 0
    FOR I = 0 TO TOTALCARDS + JOKERSPRESANT
        IF CARDORDER(I) = -1 
            CARDORDER(I) = CARDNO 
            CARDNO = (CARDNO + J) MOD (TOTALCARDS + 1 + JOKERSPRESANT)
        ENDIF
    NEXT I
    CARDNO = 0
    M = 0
    FOR M = 0 TO NUMBER_OF_DECKS
        FOR I = 0 TO TOTALCARDS            
            J = RAND(TOTALCARDS + JOKERSPRESANT)
            IF I = J THEN J = RAND(TOTALCARDS + JOKERSPRESANT)
            IF I = J THEN J = RAND(TOTALCARDS + JOKERSPRESANT)
            K = CARDORDER(J)
            CARDORDER(J) = CARDORDER(I)
            CARDORDER(I) = K            
        NEXT I
        IF M = 1 
            FOR I = 0 TO TOTALCARDS + JOKERSPRESANT
                OLDORDER(I) = CARDORDER(I)
            NEXT I    
        ENDIF
    NEXT M
    IF NUMBER_OF_DECKS <= 4
        L = 4
    ELIF NUMBER_OF_DECKS <= 6
        L = 7
    ELSE
        L = 10
    ENDIF  
    ' ****   G2 = SELECTED_RES_Y * .8 
    ' ****   G1 = SELECTED_RES_Y * .6
    ' ****   G0 = SELECTED_RES_Y * .4
    I = 0 
    REPEAT
        K = OLDORDER(I)
        J = CARDORDER(I)
        IF (I MOD L) = 1
            ' ****   SPRITEK+1, SELECTED_RES_X * .36 + F * K * .5 , G0 , CARDBACKIMAGE
        ENDIF
        IF (I MOD L) = 2
            ' ****   SPRITEK+1, SELECTED_RES_X * .36 + F * J * .5  , G1 , CARDBACKIMAGE
        ENDIF
        ' ****   SPRITEK+1, SELECTED_RES_X * .18 + F * J  , G2 , CARDBACKIMAGE
        ' **** SET    SPRITEPRIORITY K+1,J+1      
        I = I + 1
    UNTIL I > TOTALCARDS + JOKERSPRESANT
    CARDNO = 0
    FOR I = 0 TO  TOTALCARDS + JOKERSPRESANT
       ' ****   SPRITEI+1, -200, -200, CARDIMAGE(I)
    NEXT I
ENDPROC

PROC INIT_DEFAULTS
    LASTPLAYER = 1
    DECKSIZE = 52    
    NUMBER_OF_DECKS = 4
    DBL_DOWN_SET = 0
    SPLITSET = 0
    BONUS777 = 1
    BONUS678 = 0
    DEALERTIEWINS  = 0
    DEALERSOFT17HIT = 0
    CARDCHARLIEWINSET = 6
    SUIT21BONUS  = 0    
    COLOR21BONUS  = 0    
    DBL_DOWN_3UP = 0           
    ALLOWSURRENDER = 0   
ENDPROC


PROC TITLE_SCREEN             
   A = 0
   B = 0
   C = 0
   I = 0
   J = 0
   K = 0   
   L = 0   
   N = 0
   CONSOL_DOWN = 0
   SELECTED = 0
   POKE @DLIV1,SELECTED   
   MOUSECLICK = 0
   BUTTONUP = 0
   SELECTED_MODE = 0
   MX = 0
   MY = 0
   ' SET WINDOW OFF
   ' SET DISPLAY MODE 1024,768,32
   ' SELECTED_BITDEPTH=32
   ' **** EMPTY CHECKLIST
   ' **** PERFORM CHECKLIST FOR DISPLAY MODES
    A = 1
    N = 1
    B = 0 
    
'   SET WINDOW ON
'   REPEAT
      RESET_TITLE = 0
      POKE 82,0
      POKE 710,210:POKE 709,10:POKE 712,210
      POKE 756,@CHARSET_GAME_HI
      DPOKE 560,@display_list_title
      DPOKE $0224,@TITLE_VBI
      DPOKE $0200,@TITLE00_DLI
      DPOKE 88,@SCREEN_ADDR
      POKE $D40E,192
      POKE @DLIV0,0
      POKE @DLIV1,0
      POKE 752,1
      POSITION 0,0
      CLS
      PRINT "   ATARI CASINO         BLACK JACK     "
      POSITION 0,1      
      PRINT "               A Game By Peter J. Meyer"


      POSITION 0,4
      PRINT "   Deck Size:"
      'POSITION 0,5
      PRINT "Number Decks:"
      'POSITION 0,6
      PRINT " Dealer Hits:"     
      'POSITION 0,7     
      PRINT "  Dealer Tie:"
      'POSITION 0,8
      PRINT "  Black Jack:"
      'POSITION 0,9 
      PRINT " Double Down:"
      'POSITION 0,10
      PRINT "Unbust Limit:"
      'POSITION 0,11
      PRINT "  Split Rule:"
      'POSITION 0,12
      PRINT "   Surrender:"
      'POSITION 0,13
      PRINT "   777 Bonus:"
      'POSITION 0,14
      PRINT "   678 Bonus:"
      'POSITION 0,15
      PRINT "     Players:"
                                                                                            
      POSITION 10,17
      PRINT "Press Start to Begin"

     EXEC SHOW_OPTIONS
     
     REPEAT
        IF PEEK($D01F) <> 7 
          IF CONSOL_DOWN = 0
            IF PEEK($D01F) = 5 
              SELECTED=PEEK(@DLIV1)
              SELECTED = SELECTED + 1
              IF SELECTED>11 THEN SELECTED=0
              POKE @DLIV1,SELECTED              
            ELIF PEEK($D01F) = 3
               A=PEEK(@DLIV1)
               EXEC Change_Game_Option
               EXEC SHOW_OPTIONS
            ENDIF          
            CONSOL_DOWN = 150
          ELSE
            CONSOL_DOWN = CONSOL_DOWN - 1 
          ENDIF
        ELSE
          CONSOL_DOWN = 0
        ENDIF   
     UNTIL PEEK($D01F) = 6
     

''    UNTIL SELECTING=2 OR SELECTING=3
    
   'REM GET IMAGE 255,0,0,640,480
   'REM SAVE IMAGE "TITLE SCREEN SHOT.BMP",255
ENDPROC

PROC SHOW_OPTIONS
      POSITION 13,4
      'Deck size can be 48 or 52 cards, 56 Deck Size disabled
      IF DECKSIZE = 52
        PRINT "48 CARD DECKS(Spanish 21)"
      ELIF DECKSIZE = 48
        PRINT "52 CARD DECKS(Standard)  "
      ELIF DECKSIZE = 56
        PRINT "56 CARD DECKS            "
      ENDIF
      

      POSITION 13,5
      'Number of Decks can 4,6,or 8 Decks
      IF NUMBER_OF_DECKS = 4
        PRINT "FOUR DECKS  "
      ELIF NUMBER_OF_DECKS = 6
        PRINT "SIX DECKS   "
      ELIF NUMBER_OF_DECKS = 8
        PRINT "EIGHT DECKS "
      ENDIF

      

      POSITION 13,6
      IF DEALERSOFT17HIT = 0
        PRINT "HOLDS ON 17 OR MORE      "
      ELSE
        PRINT "DEALER HITS SOFT 17      "
      ENDIF


      POSITION 13,7
      IF DEALERTIEWINS = 0
        PRINT "BET RETURNED. PUSH.      "
      ELSE
        PRINT "LOOSE BET, GOES TO DEALER"
      ENDIF
     
      POSITION 13,8 
      IF SUIT21BONUS = 0
          PRINT "NO BLACKJACK BONUS ALL 3:2"
      ELIF SUIT21BONUS = 1
          PRINT "SUIT BLACKJACK PAYS 2:1   "
      ELIF SUIT21BONUS = 2                    
          PRINT "SAME COLOR  PAYS 2:1      "
      ENDIF


      POSITION 13,9 
      IF DBL_DOWN_SET = 0
         PRINT "ANY TOTAL ALLOWED  "
      ELIF DBL_DOWN_SET = 1
         PRINT "10 OR 11 ONLY      "
      ELIF DBL_DOWN_SET = 2
         PRINT "9, 10, OR 11 ONLY  "
      ELSE  
         PRINT "UPTO 3 CARDS       "
      ENDIF

     
    
     POSITION 13,10
     IF CARDCHARLIEWINSET = 6 
          PRINT "SIX CARD CHARLIE WINS  "
     ELSE  
          PRINT "FIVE CARD CHARLIE WINS "
     ENDIF


      POSITION 13,11
      IF SPLITSET = 0
          PRINT "SPLIT ANY PAIR 10=(J,Q,K)"
      ELIF SPLITSET = 1
          PRINT "SPLIT EXACT PAIR         "
      ELSE  
          PRINT "NO SPLIT ACES            "
      ENDIF



     POSITION 13,12
     IF ALLOWSURRENDER = 0
        PRINT "NO PLAYER SURRENDER      "
     ELSE
        PRINT "ALLOWED HALF BET RETURNED"
     ENDIF  
  



     POSITION 13,13
     IF BONUS777 < 2 
        PRINT BONUS_777_OPTION_SHOW$(BONUS777)
     ENDIF

  

     POSITION 13,14  
     IF BONUS678<2
        PRINT BONUS_678_OPTION_SHOW$(BONUS678)
     ENDIF

     POSITION 13,15
     I = LASTPLAYER - 1 
     IF I < 4
           PRINT NUMBER_OF_PLAYERS_OPTION_SHOW$(I)
     ENDIF
ENDPROC

PROC Change_Game_Option
    SELECTED=PEEK(@DLIV1)
    IF SELECTED = 0 
        'Deck size can be 48 or 52 cards
        DECKSIZE = DECKSIZE + 4
        IF DECKSIZE >=56 THEN DECKSIZE = 48      
    ELIF SELECTED = 1
        'Number of Decks can 4,6,or 8 Decks
        NUMBER_OF_DECKS = NUMBER_OF_DECKS + 2
        IF NUMBER_OF_DECKS >= 10 THEN NUMBER_OF_DECKS = 4 
    ELIF SELECTED = 2
        'A soft hit is when dealer has 17 with an Ace, be able to hit again, and if over 21, Ace becomes 1.
        IF DEALERSOFT17HIT = 0 
          DEALERSOFT17HIT = 1
        ELSE
          DEALERSOFT17HIT = 0
        ENDIF    
    ELIF SELECTED = 3
        'Some Casino's have a push rule, if tie, bet is returned.
        IF DEALERTIEWINS = 0
            DEALERTIEWINS = 1
        ELSE
            DEALERTIEWINS = 0
        ENDIF
    ELIF SELECTED = 4
        'Blackjack on opening deal. If two hards are same suit or color, 2x bet is paid out'
        SUIT21BONUS = SUIT21BONUS + 1
        IF SUIT21BONUS >=3 THEN SUIT21BONUS = 0
    ELIF SELECTED = 5
        DBL_DOWN_SET = DBL_DOWN_SET + 1
        IF DBL_DOWN_SET >=3 THEN DBL_DOWN_SET = 0    
    ELIF SELECTED = 6
        CARDCHARLIEWINSET = CARDCHARLIEWINSET + 1
        IF CARDCHARLIEWINSET >=7 THEN CARDCHARLIEWINSET = 5
    ELIF SELECTED = 7
         SPLITSET = SPLITSET + 1
         IF SPLITSET>=3 THEN SPLITSET = 0
    ELIF SELECTED = 8
       IF ALLOWSURRENDER = 0
          ALLOWSURRENDER = 1
       ELSE
          ALLOWSURRENDER = 0
       ENDIF      
    ELIF SELECTED = 9
        IF BONUS777 = 0
            BONUS777 = 1
        ELSE
            BONUS777 = 0
        ENDIF
    ELIF SELECTED = 10
       IF BONUS678 = 0
          BONUS678 = 1
       ELSE
          BONUS678 = 0
       ENDIF    
    ELIF SELECTED = 11
      LASTPLAYER = LASTPLAYER + 1
      IF LASTPLAYER >=4 THEN LASTPLAYER = 1    
    ENDIF    
      


ENDPROC



     
    

  



