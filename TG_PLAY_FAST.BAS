'can we port to fast basic (Atari)?'
'can we port to visual basic.net (Windows PC)?'
DECLARE SUB LOADPICTURE (F$)
DECLARE SUB SETSCREEN ()
DEFINT A-Z
COMMON SHARED ADAPTER, PCOL, HIGHT, MAXHIGHT, VX, VY

GOSUB Initialize
REPEAT
  GOSUB Get_game
  GOSUB Main_line
  GOSUB Game_over
UNTIL CHOICE = 81
RUN "TG_MAIN.BAS"

PROC Initialize()
  'CHDIR "TEXTGAME"
  RANDOMIZE TIMER
  OPEN "TG.PIF" FOR INPUT AS #1
    INPUT #1, ADAPTER
    INPUT #1, PATH$
  CLOSE #1
 
  PW_DIM = 32
  DIM PW$(PW_DIM)
  OB_DIM = 128
  DIM Ot(OB_DIM), OP(OB_DIM), OV(OB_DIM), OV2(OB_DIM), OV3(OB_DIM)
  DIM OL(OB_DIM), ON$(OB_DIM), Od$(OB_DIM)
  HS_DIM = 64
  DIM Ht(HS_DIM), Hl(HS_DIM), HS(HS_DIM), HH(HS_DIM), Hm(HS_DIM), Hv(HS_DIM)
  DIM Hn$(HS_DIM), HO(HS_DIM)
  RM_DIM = 192
  DIM RT(RM_DIM), Rn(RM_DIM), Rnl(RM_DIM), Rnk(RM_DIM), Rw(RM_DIM), Rwl(RM_DIM)
  DIM Rwk(RM_DIM), Re(RM_DIM), Rel(RM_DIM), Rek(RM_DIM), Rs(RM_DIM), Rsl(RM_DIM)
  DIM Rsk(RM_DIM), Ru(RM_DIM), Rul(RM_DIM), Rd(RM_DIM), Rdl(RM_DIM), Ro(RM_DIM)
  DIM Rol(RM_DIM), Rov(RM_DIM), REN$(RM_DIM), Rd$(RM_DIM)
   S0 = 0
   S1 = 0
   S2 = 0
   S3 = 0
   S4 = 0
   S5 = 0
   S6 = 0
   S7 = 0
   S8 = 0
   S9 = 0
   S10 = 0
   S11 = 0
   S12 = 0
   S13 = 0
   S14 = 0
   S15 = 0
   S16 = 0
   S17 = 0
   S18 = 0
   S19 = 0
   S20 = 0
   S21 = 0
   S22 = 0
   S23 = 0
   S24 = 0
   S25 = 0
   S26 = 0
   S27 = 0
   S28 = 0
   S29 = 0
   S30 = 0
   S31 = 0
   S32 = 0
   S33 = 0
   S34 = 0
   S35 = 0
   S36 = 0
   S37 = 0
   S38 = 0
   S39 = 0
   S40 = 0
   CU$ = " "
   a$ = SPACE$(640)
   CHOICE = 0
   EXIT_NAME$ = "EXIT"
   EXIT_ADJECTIVE$ = "GOING"
ENDPROC

PROC Main_line
 DO
   REPEAT
     PR = R
     CLS
     IF RT(R) = 8 
       GOSUB Answer_riddle
     ELSE
       EXEC New_place
       IF SP! < 0 OR FW <= 0 THEN DEAD = 1
       Ft = 1
       IF DEAD = 0 
         WHILE R = PR AND SP! >= 0 AND FW >= 0 AND TN <= S12 AND DEAD = 0 AND NOT WIN
           TN = TN + 1
           FW = FW - 1
           IF Ft = 0 
             PRINT "--------------------------------------------------------------------------------";
           ELSE
             PRINT
             Ft = 0
           ENDIF
           H = 0
           IF Fm 
             EXEC Check_for_hostilities
           ELSE
             EXEC Move_hostilities
           ENDIF
           EXEC Enter_command
           IF LEN(a$) >= 2 
             EXEC Perform_action
           ELSE
             EXEC Go_direction
           ENDIF
           IF Imm > 0 
             Imm = Imm - 1
           ELSE
             IF Inf > 0 
               SP! = SP! - Inf
             ENDIF
             IF Psn > 0 
               SP! = SP! - Psn
             ENDIF
             IF H > 0 
               EXEC Hostility_attacks
             ENDIF
           ENDIF
         ENDWHILE
       ENDIF
       N = 0
       IF R = S1 OR S1 = 0 THEN N = N + 1
       IF OL(S2) = 0 OR S2 = 0 THEN N = N + 1
       IF OL(S3) = 0 OR S3 = 0 THEN N = N + 1
       IF OL(S4) = 0 OR S4 = 0 THEN N = N + 1
       IF OL(S7) = 0 OR S7 = 0 THEN N = N + 1
       IF OL(S30) = 0 OR S30 = 0 THEN N = N + 1
       IF OL(S31) = 0 OR S31 = 0 THEN N = N + 1
       IF OL(S32) = 0 OR S32 = 0 THEN N = N + 1
       IF OL(S33) = 0 OR S33 = 0 THEN N = N + 1
       IF OL(S34) = 0 OR S34 = 0 THEN N = N + 1
       IF OL(S35) = 0 OR S35 = 0 THEN N = N + 1
       IF HH(S5) = 0 OR S5 = 0 THEN N = N + 1
       IF HH(S36) = 0 OR S36 = 0 THEN N = N + 1
       IF HH(S37) = 0 OR S37 = 0 THEN N = N + 1
       IF HH(S38) = 0 OR S38 = 0 THEN N = N + 1
       IF HH(S39) = 0 OR S39 = 0 THEN N = N + 1
       IF C >= S6 OR S6 = 0 THEN N = N + 1
       IF N = 17 THEN WIN = 1
     ENDIF
   UNTIL DEAD = 1 OR WIN = 1 OR TN > S12
   IF TN > S12  LIVES = -1
   IF SP! < 0 OR FW < 0 OR DEAD 
     DEAD = 0
     LIVES = LIVES - 1
     IF LIVES > 0 
       PRINT "You have "; LIVES; " lives left."
       PRINT "Press any key to continue."
       X$ = INKEY$
       REPEAT
       UNTIL INKEY$ <> ""
     ENDIF
     IF SP! < 10 THEN SP! = S8
     IF FW < 10 THEN FW = S13
     IF S16 = 2 OR S16 = 3 OR RT(R) = 2 OR RT(R) = 5 OR RT(R) = 9 
       PR = S14
       R = S14
     ENDIF
     IF S16 = 1 
       PR = 0
       H = 0
     ENDIF
   ENDIF
 UNTIL LIVES <= 0 OR WIN = 1
ENDPROC

PROC Check_for_hostilities
    FOR N = 1 TO HOS
      IF Hl(N) = R 
        EXEC Hostility_present
      ENDIF
    NEXT N
    Fm = 0
ENDPROC

PROC Hostility_present
    M = Ht(N) AND 127
    IF HH(N) >= 0 
      IF M = 1 OR M = 2 OR M = 12 OR M = 13 
        PRINT "The "; Hn$(N); " is attacking you."
        H = N
      ENDIF
      IF (M > 2 AND RT(N) < 12) OR M > 13 
        PRINT "The "; Hn$(N); " is in here."
        H = N
      ENDIF
      IF HH(N) > 0 AND M < 12 
        PRINT "It has "; HH(N); " hit points."
      ENDIF
    ELSE
      PRINT "There is a dead "; Hn$(N); " in here."
    ENDIF
    ENDPROC

PROC Place_Info
    a$ = Rd$(R)
    R$ = REN$(R)
    CLS
    IF LEFT$(a$, 1) = "(" 
      a$ = MID$(a$, 2, INSTR(1, a$, ")") - 3)
      EXEC Read_text_file
    ENDIF
    I = INSTR(1, a$, "|")
    IF I > 0 
      F$ = MID$(a$, I + 1)
      a$ = LEFT$(a$, I - 1)
      LOADPICTURE F$
    ENDIF
    I = INSTR(1, a$, "{")
    IF I > 0 AND (S0 = 1 OR S0 = 2) 
      IF INSTR(1, a$, "}") > 0 
         J = INSTR(1, a$, "}") - I - 1
      ENDIF
      EXIT_NAME$ = MID$(a$, I + 1, J)
      a$ = LEFT$(a$, I - 1)
      IF INSTR(1, EXIT_NAME$, ",") > 0 
        J = INSTR(1, EXIT_NAME$, ",")
        EXIT_ADJECTIVE$ = MID$(EXIT_NAME$, J + 1)
        EXIT_NAME$ = LEFT$(EXIT_NAME$, J - 1)
      ENDIF
    ENDIF
    IF S0 = 0 OR RT(R) = 8 OR RT(R) = 2 
      ADJ = 1
      EXEC Display
    ENDIF
ENDPROC

PROC Read_text_file
    ON ERROR GOTO 314
    P$ = a$
    OPEN P$ FOR INPUT AS #1
    INPUT #1, a
    a$ = INPUT$(a, 1)
314 CLOSE #1
ENDPROC

PROC New_place
    IF RT(R) <> 4 OR L > 0 
      IF RT(R) = 14 AND a$ <> "L" THEN EXEC Lock_exits
      EXEC Place_Info
      IF S0 = 1 OR S0 = 2 THEN a$ = a$ + " "
      ON ERROR GOTO 0
      IF RT(R) <> 2 
        t1 = 0
        B$ = ""
        IF Rn(R) > 0 AND Rn(R) <> 4 
          B$ = B$ + "North"
          t1 = 1
          IF Rn(R) = 3 OR Rn(R) = 5 THEN B$ = B$ + "(locked)"
          IF Rn(R) = 2 THEN B$ = B$ + "(unlocked)"
        ENDIF
        IF (Re(R) > 0 AND Re(R) <> 4) AND t1 > 0 
          B$ = B$ + ", "
          T2 = LEN(B$)
        ENDIF
        IF Re(R) > 0 AND Re(R) <> 4 
          B$ = B$ + "East"
          t1 = t1 + 1
          IF Re(R) = 3 OR Re(R) = 5 THEN B$ = B$ + "(locked)"
          IF Re(R) = 2 THEN B$ = B$ + "(unlocked)"
        ENDIF
        IF (Rw(R) > 0 AND Rw(R) <> 4) AND t1 
          B$ = B$ + ", "
          T2 = LEN(B$)
        ENDIF
        IF Rw(R) > 0 AND Rw(R) <> 4 
          B$ = B$ + "West"
          t1 = t1 + 1
          IF Rw(R) = 3 OR Rw(R) = 5 THEN B$ = B$ + "(locked)"
          IF Rw(R) = 2 THEN B$ = B$ + "(unlocked)"
        ENDIF
        IF (Rs(R) > 0 AND Rs(R) <> 4) AND t1 
          B$ = B$ + ", "
          T2 = LEN(B$)
        ENDIF
        IF Rs(R) > 0 AND Rs(R) <> 4 
          B$ = B$ + "South"
          t1 = t1 + 1
          IF Rs(R) = 3 OR Rs(R) = 5 THEN B$ = B$ + "(locked)"
          IF Rs(R) = 2 THEN B$ = B$ + "(unlocked)"
        ENDIF
        IF (Ru(R) > 0 AND Ru(R) <> 4) AND t1 
          B$ = B$ + ", "
          T2 = LEN(B$)
        ENDIF
        IF Ru(R) > 0 AND Ru(R) <> 4 
          B$ = B$ + "Up"
          t1 = t1 + 1
        ENDIF
        IF (Rd(R) > 0 AND Rd(R) <> 3 AND Rd(R) <> 4) AND t1 
          B$ = B$ + ", "
          T2 = LEN(B$)
        ENDIF
        IF Rd(R) > 0 AND Rd(R) <> 4 AND Rd(R) <> 3 
          B$ = B$ + "Down"
          t1 = t1 + 1
        ENDIF
        IF Ro(R) > 0 AND Ro(R) < 6 
          R$ = REN$(R)
          B$ = B$ + ", "
          T2 = LEN(B$)
          IF S0 = 1 OR S0 = 2 
            B$ = B$ + "There is a"
            t1 = t1 + 1
            IF INSTR(1, "AEIOU", UCASE$(RIGHT$(R$, 1))) > 1 THEN B$ = B$ + "n"
          ENDIF
          T2 = LEN(B$)
          B$ = B$ + " " + R$
          IF Ro(R) = 5 THEN B$ = B$ + "(locked)"
        ELSE
          IF Ro(R) = 4 THEN B$ = B$ + "(blocked)"
        ENDIF
        B$ = B$ + "_ "
        IF t1 > 1 
          IF S0 = 0 
            a$ = "EXITS:"
          ELSE
            a$ = a$ + "There are " + LCASE$(EXIT_NAME$) + "s " + LCASE$(EXIT_ADJECTIVE$) + " "
            B$ = LCASE$(B$)
          ENDIF
          IF t1 > 2 THEN P$ = LEFT$(B$, T2 - 1) + " and" + MID$(B$, T2)
          IF t1 = 2 THEN P$ = LEFT$(B$, T2 - 2) + " and" + MID$(B$, T2)
          a$ = a$ + P$
        ELSEIF t1 = 1 
          IF S0 = 0 
             a$ = "EXIT:"
          ELSE
             a$ = a$ + "There is a " + LCASE$(EXIT_NAME$) + " " + LCASE$(EXIT_ADJECTIVE$) + " "
             B$ = LCASE$(B$)
          ENDIF
          a$ = a$ + B$
        ENDIF
        IF S0 = 0 OR S0 = 1 
          ADJ = 1
          EXEC Display
        ENDIF
        IF RT(R) <> 7 
          B$ = ""
          t1 = 0
          T2 = 0
          FOR N = 1 TO OBJ
            IF OL(N) = R AND t1 
              B$ = B$ + ", "
              T2 = LEN(B$)
            ENDIF
            IF OL(N) = R 
              t1 = t1 + 1
              B$ = B$ + ON$(N)
            ENDIF
          NEXT N
          IF Ro(R) = 7 
            IF t1 THEN B$ = B$ + ", "
            T2 = LEN(B$)
            B$ = B$ + R$
            t1 = t1 + 1
          ENDIF
          IF t1 > 0 
            'B$ = B$ + "_ "
            P$ = B$
            IF t1 = 2 THEN P$ = LEFT$(B$, T2 - 2) + " and" + MID$(B$, T2)
            IF t1 > 2 THEN P$ = LEFT$(B$, T2 - 1) + " and" + MID$(B$, T2)
            IF S0 = 0 OR S0 = 1 
              a$ = "OBJECT"
              IF t1 > 1 THEN a$ = a$ + "S"
              a$ = a$ + ":" + P$
            ELSEIF S0 = 2 
              IF UCASE$(RIGHT$(B$, 1)) = "S" AND t1 = 1 THEN t1 = 99
              IF t1 < 3 
                a$ = a$ + "There is "
                IF INSTR(1, "AEIOU", UCASE$(LEFT$(B$, 1))) 
                  a$ = a$ + "an "
                ELSE
                  a$ = a$ + "a "
                ENDIF
                a$ = a$ + LCASE$(P$) + " in here_ "
              ELSE
                a$ = a$ + "There are " + LCASE$(P$) + " in here_ "
              ENDIF
            ENDIF
          ENDIF
          IF B$ <> "" OR S0 = 2 
            ADJ = 1
            EXEC Display
         ENDIF
        ELSE
          PRINT "OBJECT"; TAB(30); UCASE$(CU$)
          FOR N = 1 TO OBJ
            IF OL(N) = R 
              PRINT UCASE$(ON$(N)); TAB(30); OP(N)
            ENDIF
          NEXT N
        ENDIF
      ELSEIF RT(R) = 2 
        DEAD = 1
      ENDIF
    ELSE
      CLS
      PRINT "Too dark, can not see_"
    ENDIF
    IF RT(R) = 3 
      SP! = SP! - S17
      PRINT "You lost "; S17; C$
      IF SP! < 0 
        SP! = 0
        DEAD = 1
        PRINT "Died because did not enough"; C$
      ENDIF
    IF RT(R) = 5 
      EXEC Find_survival_equipment
    ENDIF
    IF RT(R) = 9 
      C = C - S24
      IF C < 0 
        C = 0
      ENDIF
    ENDIF
    IF RT(R) = 15 OR (RT(R) = 10 AND S16 = 3) THEN S14 = R
ENDPROC

PROC Go_direction
    B$ = "Can not go that way, it is locked. "
    P$ = "Can not go that way, need password. "
    IF a$ = "N" 
      N = Rn(R)
      M = Rnl(R)
      EXEC Ok_to_move
    ENDIF
    IF a$ = "E" 
      N = Re(R)
      M = Rel(R)
      EXEC Ok_to_move
    ENDIF
    IF a$ = "W" 
      N = Rw(R)
      M = Rwl(R)
      EXEC Ok_to_move
    ENDIF
    IF a$ = "S" 
      N = Rs(R)
      M = Rsl(R)
      EXEC Ok_to_move
    ENDIF
    IF a$ = "D" 
      IF Rd(R) = 1 OR Rd(R) = 4 
        R = Rdl(R)
      ENDIF
      IF Rd(R) = 2 
        M = Rdl(R)
        EXEC Ok_to_climb
      ENDIF
    ENDIF
    IF a$ = "U" 
      IF Ru(R) = 1 OR Ru(R) = 4 
        R = Rul(R)
      ENDIF
      IF Ru(R) = 2 
        M = Rul(R)
        EXEC Ok_to_climb
      ENDIF
    ENDIF
    IF a$ = "I" 
      EXEC Examine_inventory
    ENDIF
    IF Ht(H) = 7 OR Ht(H) = 14 OR (RT(R) = 6 AND H > 0) 
      IF PR > 0 THEN R = PR
      PRINT "The "; Hn$(H); " prevents you from exiting."
    ENDIF
    IF a$ = "L" 
      PR = 0
    ENDIF
ENDPROC

PROC Ok_to_move
    IF N = 3 
      PRINT B$
    ENDIF
    IF N = 5 
      PRINT P$
    ENDIF
    IF N = 1 OR N = 2 OR N = 4 
      R = M
    ENDIF
    ENDPROC

PROC Ok_to_climb
    FOR N = 1 TO OBJ
      IF Ot(N) = 11 AND OL(N) = 0 THEN R = M
    NEXT N
    IF PR = R 
      PRINT "Need something to climb with."
    ENDIF
ENDPROC

PROC Enter_command
    EXEC Show_status
    REPEAT
      PRINT
      PRINT "Enter command."
      INPUT ">", a$
    UNTIL a$ <> ""
    IF a$ = "*" 
      a$ = PC$
      LOCATE CSRLIN - 1, 2
      PRINT PC$
    ELSE
      a$ = UCASE$(a$)
    ENDIF
    EXEC Break_down
ENDPROC

PROC Break_down
    PC$ = a$
    IF RIGHT$(a$, 1) = "." OR RIGHT$(a$, 1) = "!" 
      a$ = MID$(a$, 1, LEN(a$) - 1)
    ENDIF
    LOC_THE = INSTR(1, a$, "THE ")
    IF LOC_THE > 0 
      NOUN$ = MID$(a$, LOC_THE + 4)
      a$ = LEFT$(a$, LOC_THE - 1) + NOUN$
    ENDIF
    LOC_TO = INSTR(1, a$, "TO ")
    IF LOC_TO > 0 
      NOUN$ = MID$(a$, LOC_TO + 3)
      a$ = LEFT$(a$, LOC_TO - 1) + NOUN$
    ENDIF
    EXEC SEPERATE_VERB_NOUN
    IF Verb$ = "JUMP" OR Verb$ = "LEAP" OR Verb$ = "CLIMB" OR Verb$ = "JOG" 
      a$ = MID$(NOUN$, 2)
      EXEC SEPERATE_VERB_NOUN
    ENDIF
    IF Verb$ = "GO" OR Verb$ = "RUN" OR Verb$ = "WALK" OR Verb$ = "TURN" 
      a$ = MID$(NOUN$, 2)
      EXEC SEPERATE_VERB_NOUN
    ENDIF
    PC$ = a$
    ENDIF
    IF Verb$ = "SOUTH" THEN a$ = "S"
    IF Verb$ = "NORTH" THEN a$ = "N"
    IF Verb$ = "EAST" THEN a$ = "E"
    IF Verb$ = "WEST" THEN a$ = "W"
    IF Verb$ = "UP" THEN a$ = "U"
    IF Verb$ = "DOWN" THEN a$ = "D"
    IF Verb$ = "LOOK" OR a$ = "GAZE" OR LEFT$(a$, 3) = "SEE" OR a$ = "GLANCE" OR LEFT$(a$, 4) = "VIEW" THEN a$ = "L"
    IF Verb$ = "INVENTORY" OR a$ = "INV" THEN a$ = "I"
    IF Verb$ = "KICK" THEN a$ = "K2"
    IF Verb$ = "PUNCH" OR Verb$ = "HIT" OR Verb$ = "SLAP" THEN a$ = "K3"
    IF Verb$ = "EXAMINE" OR Verb$ = "READ" OR Verb$ = "LOOK" OR Verb$ = "CHECK" THEN
      IF LEFT$(NOUN$, 3) = " AT" 
        NOUN$ = MID$(NOUN$, 4)
      ENDIF
      IF NOUN$ <> "" 
        a$ = "X" + NOUN$
      ELSE
        a$ = "L"
      ENDIF
    ENDIF
    IF Verb$ = "DIG" OR Verb$ = "SHOVEL" OR Verb$ = "UNCOVER" THEN a$ = "DIG"
    IF Verb$ = "SHOUT" OR Verb$ = "SAY" OR Verb$ = "CAST" THEN a$ = "S" + NOUN$
    IF Verb$ = "MAKE" OR Verb$ = "ASSEMBLE" OR Verb$ = "BUILD" THEN a$ = "M" + NOUN$
    IF Verb$ = "GET" OR Verb$ = "BUY" OR Verb$ = "TAKE" OR Verb$ = "STEEL" OR Verb$ = "GRAB" OR Verb$ = "OBTAIN" OR Verb$ = "POSSESS" 
      a$ = "G" + NOUN$
    ENDIF
    IF Verb$ = "SELL" OR Verb$ = "DROP" OR Verb$ = "LEAVE" THEN a$ = "D" + NOUN$
    IF Verb$ = "ENTER" OR Verb$ = "CROSS" OR Verb$ = "IN" THEN a$ = "R" + NOUN$
    IF Verb$ = "USE" OR Verb$ = "ACTIVATE" OR Verb$ = "DETONATE" THEN a$ = "U" + NOUN$
    IF Verb$ = "EAT" OR Verb$ = "SWALLOW" OR Verb$ = "DRINK" OR Verb$ = "BITE" OR Verb$ = "CONSUME" THEN a$ = "E" + NOUN$
    IF Verb$ = "ZAP" OR Verb$ = "TELEPORT" OR B$ = "TRANSPORT" OR B$ = "BEAM" THEN a$ = "T" + NOUN$
    IF Verb$ = "SHOOT" OR Verb$ = "SWING" OR Verb$ = "POKE" OR Verb$ = "JAB" OR Verb$ = "STAB" 
      a$ = "K" + NOUN$
    ENDIF
    IF Verb$ = "THROW" OR Verb$ = "HURL" OR Verb$ = "FLING" OR Verb$ = "PITCH" 
      a$ = "K1" + MID$(NOUN$, 2)
    ENDIF
    IF Verb$ = "ON" OR Verb$ = "LIGHT" THEN a$ = "ON"
    IF Verb$ = "OFF" OR Verb$ = "UNLIGHT" THEN a$ = "OFF"
    IF Verb$ = "OPEN" OR Verb$ = "UNLOCK" THEN a$ = "O" + NOUN$
    IF Verb$ = "LOCK" OR Verb$ = "CLOSE" THEN a$ = "C" + NOUN$
    IF Verb$ = "PUSH" OR Verb$ = "PULL" OR Verb$ = "YANK" OR Verb$ = "MOVE" THEN a$ = "P" + NOUN$
    IF Verb$ = "FRY" OR Verb$ = "BURN" THEN a$ = "B" + NOUN$
    IF Verb$ = "BREAK" OR Verb$ = "BUST" OR Verb$ = "HIT" OR Verb$ = "SLAM" THEN a$ = "H" + NOUN$
650
ENDPROC

PROC Show_status
   a$ = "    You "
   IF S12 > 0 
     a$ = a$ + "are on turn " + STR$(TN) + " of " + STR$(S12) + ". "
   ENDIF
   B$ = ""
   T2 = 1
   LC = 1
   IF S15 > 0 
     B$ = B$ + STR$(LIVES)
     IF LIVES = 1 
       B$ = B$ + " life"
     ELSE
       B$ = B$ + " lives"
     ENDIF
     B$ = B$ + " left, "
     LC = T2
     T2 = LEN(B$)
   ENDIF
   IF S8 > 0 
     B$ = B$ + STR$(SP!) + LCASE$(C$) + ", "
     LC = T2
     T2 = LEN(B$)
   ENDIF
   IF S22 > 0 
     B$ = B$ + STR$(C) + " " + LCASE$(CU$)
     IF C <> 1 AND UCASE$(RIGHT$(CU$, 1)) <> "S" THEN B$ = B$ + "s"
     B$ = B$ + ", "
     LC = T2
     T2 = LEN(B$)
   ENDIF
   IF S21 = 1 
     B$ = B$ + STR$(ARMR) + " armor point"
     IF NOT ARMR = 1 THEN B$ = B$ + "s"
     LC = T2
     B$ = B$ + ", "
     T2 = LEN(B$)
   ENDIF
   IF B$ <> "" 
     MID$(B$, T2 - 1) = ". "
     IF LC > 1 
       P$ = LEFT$(B$, LC) + "and" + MID$(B$, LC)
       B$ = P$
     ENDIF
     a$ = a$ + "You have " + B$
   ENDIF
   IF S13 > 0 AND FW < 13 
     a$ = a$ + "You need food or water, have " + STR$(FW) + " turns left. "
   ENDIF
   IF LEN(a$) > 9 
     ADJ = 1
     EXEC Display
   ENDIF
ENDPROC

PROC SEPERATE_VERB_NOUN
  SPACE = INSTR(1, a$, " ")
  IF SPACE 
    Verb$ = MID$(a$, 1, SPACE - 1)
    NOUN$ = MID$(a$, SPACE)
  ELSE
    Verb$ = a$
    NOUN$ = ""
  ENDIF
ENDPROC

PROC Perform_action
    IF RT(R) = 12 THEN EXEC Correct_action
    P$ = MID$(a$, 1, 2)
    IF P$ = "G " THEN EXEC Get_objects
    IF P$ = "D " THEN EXEC Drop_objects
    IF P$ = "X " THEN EXEC Examine_object
    IF P$ = "E " THEN EXEC Consume_object
    IF a$ = "ON" THEN EXEC Light_object
    IF P$ = "P " THEN EXEC Push_object
    IF P$ = "O " THEN EXEC Open_it
    IF P$ = "C " THEN EXEC Close_exit
    IF a$ = "DIG" THEN EXEC Dig_up_exit
    IF P$ = "S " THEN EXEC Check_password
    IF P$ = "B " THEN EXEC Burn_object
    IF P$ = "R " THEN EXEC Enter_other_exit
    IF P$ = "H " THEN EXEC Break_object
    IF P$ = "U " THEN EXEC Blow_open_exit
    IF P$ = "K " OR P$ = "K1" OR P$ = "K2" OR P$ = "K3" THEN EXEC Fight_hostility
    IF P$ = "T " THEN EXEC Transport
    IF P$ = "M " THEN EXEC Search_For_Components
    IF a$ = "OFF" AND L = 1 THEN L = 0: PR = 0
ENDPROC

PROC Display
    J = 1
    I = 80
    IF I > LEN(a$) THEN I = LEN(a$)
    WHILE I <= LEN(a$) AND J <= LEN(a$)
    IF I < LEN(a$) 
      IF MID$(a$, I + 1, 1) = " " THEN I = I - 1
        B$ = MID$(a$, I, 1)
        WHILE B$ <> " " AND B$ <> "." AND B$ <> "-" AND B$ <> "," AND B$ <> "!" AND B$ <> "?"
          B$ = MID$(a$, I, 1)
          I = I - 1
        WEND
      ENDIF
      IF MID$(a$, J, 1) = " " THEN J = J + 1
      IF MID$(a$, I, 1) = " " THEN I = I - 1
      IF I - J > 1 THEN P$ = MID$(a$, J, I - J + 1)
      IF ADJ = 2 
        T = INT(39.5 - LEN(P$) / 2)
      ENDIF
      IF ADJ = 1 OR T < 0 THEN T = 0
      IF T THEN PRINT TAB(T);
      PRINT P$;
      IF LEN(P$) < 80 THEN PRINT
      J = I + 1
      I = I + 80
      IF I > LEN(a$) AND J < LEN(a$) 
        I = LEN(a$)
        ADJ = 1
      ENDIF
    WEND
    ADJ = 1
    ON ERROR GOTO 0
ENDPROC

PROC Get_objects
    P$ = MID$(a$, 3, LEN(a$))
    a$ = ""
    IF Oc < S10 AND Ht(H) <> 8 
      FOR N = 1 TO OBJ
        IF OL(N) = R AND (INSTR(1, UCASE$(ON$(N)), P$) OR INSTR(1, P$, UCASE$(ON$(N))) > 0 OR P$ = UCASE$(ON$(N))) 
          IF RT(R) <> 7 OR (RT(R) = 7 AND C > OP(N)) 
          a$ = "Ok,"
            IF RT(R) = 7 
              a$ = a$ + " brought it for " + STR$(OP(N)) + " " + CU$
              C = C - OP(N)
              OL(N) = 0
            ELSE
              a$ = a$ + " got it."
            ENDIF
            IF NOT (Ot(N) = 18 OR Ot(N) = 20 OR Ot(N) = 23 OR Ot(N) = 24 OR Ot(N) = 26 OR Ot(N) = 28) 
              OL(N) = 0
              IF S21 = 0 THEN Oc = Oc + 1
              IF Ot(N) = 6 THEN EXEC Measure_armor
            ELSE
              IF Ot(N) = 18 
                a$ = a$ + "Now have more turns."
                OL(N) = -1
                S12 = S12 + OV(N)
              ENDIF
              IF Ot(N) = 20 
                a$ = a$ + "Gained " + STR$(OV(N)) + " lives."
                OL(N) = -1
                LIVES = LIVES + OV(N)
              ENDIF
              IF Ot(N) = 22 
                a$ = a$ + "Can carry " + STR$(OV(N)) + " more objects."
                S10 = S10 + OV(N)
                OL(N) = -1
              ENDIF
              IF Ot(N) = 23 OR Ot(N) = 24 OR Ot(N) = 26 
                a$ = "Can't get it, too big and heavy."
                OL(N) = R
              ENDIF
              IF Ot(N) = 28 
                EXEC Collect_with_container
              ENDIF
            ENDIF
          ELSE
            a$ = "Do not have enougth " + CU$ + ". "
          ENDIF
        ENDIF
      NEXT N
      ADJ = 1
    ENDIF
    IF Ht(H) = 8 
      a$ = "The " + Hn$(H) + " stops you from getting anything."
    ENDIF
    IF Oc >= S10 
      a$ = a$ + "Can't carry anymore objects."
    ENDIF
    ADJ = 1
    EXEC Display
ENDPROC

PROC Drop_objects
    P$ = MID$(a$, 3, LEN(a$))
    FOR N = 1 TO OBJ
      IF OL(N) = 0 AND INSTR(1, ON$(N), P$) 
        OL(N) = R
        IF RT(R) <> 7 
          PRINT "Ok, dropping "; ON$(N); ". "
          Oc = Oc - 1
        ELSE
          IF S27 = 0 THEN P = OP(N)
          IF S27 = 1 THEN P = INT((OP(N) * .9) / 5) * 5
          PRINT "Ok, sold "; ON$(N); " for "; OP(N); " "; CU$
          Oc = Oc - 1
          C = C + P
        ENDIF
        IF Ot(N) = 14 OR Ot(N) = 27 
          L = 0
        ENDIF
      ENDIF
    NEXT N
ENDPROC

PROC Examine_inventory
    a$ = "Inventory:"
    t1 = 0
    FOR N = 1 TO OBJ
      IF OL(N) = 0 AND t1 
        a$ = a$ + ", "
      ENDIF
      IF OL(N) = 0 
        a$ = a$ + LCASE$(ON$(N))
        t1 = 1
      ENDIF
    NEXT N
    a$ = a$ + ". "
    EXEC Display
ENDPROC

PROC Examine_object
    P$ = MID$(a$, 3, LEN(a$))
    t1 = 0
    FOR N = 1 TO OBJ
      IF OL(N) = 0 AND (INSTR(1, ON$(N), P$) OR INSTR(1, P$, ON$(N))) 
        t1 = 1
        a$ = Od$(N)
        IF LEFT$(a$, 1) = "(" 
          a$ = MID$(a$, 2, INSTR(1, a$, ")") - 3)
          EXEC Read_text_file
        ENDIF
        IF Ot(N) = 34 THEN EXEC Read_text_file
        IF Ot(N) = 41 THEN EXEC Show_screen_file
        ADJ = 2
        EXEC Display
      ENDIF
    NEXT N
    IF t1 = 0 
      PRINT "Object not in inventory."
    ENDIF
ENDPROC

PROC Show_screen_file
  P$ = a$
  OPEN P$ FOR INPUT AS #1
  a$ = INPUT$(1920, 1)
  CLOSE #1
  CLS
  LOCATE 1, 1
  PRINT a$;
  REPEAT
  UNTIL INKEY$ <> ""
  a$ = ""
ENDPROC

PROC Consume_object
    P$ = MID$(a$, 3)
    FOR N = 1 TO OBJ
      IF OL(N) = 0 AND INSTR(1, ON$(N), P$) 
        IF Ot(N) = 5 
          a$ = "That's better, now have " + STR$(OV(N)) + " more turns before getting more food or water."
          FW = FW + OV(N)
        ENDIF
        IF Ot(N) = 17 
          IF OV(N) = 2 
            a$ = "Lost " + STR$(OV2(N)) + " " + C$ + " because it was poisoned."
            SP! = SP! - OV2(N)
            OL(N) = -1
          ENDIF
          IF OV(N) = 1 
            a$ = "The " + ON$(N) + " was a lethal poison, you are dead."
            SP! = -1
            OL(N) = -1
            DEAD = 1
          ENDIF
          IF OV(N) = 3 
            a$ = "Member was poisoned, must find an antidote or will die."
            OL(N) = -1
            Psn = OV2(N)
          ENDIF
        ENDIF
        IF Ot(N) = 16 
          SP! = SP! + OV(N)
          a$ = "That's better, now have " + STR$(SP!) + " " + C$ + ". "
          OL(N) = -1
        ENDIF
        IF Ot(N) = 28 
          Imm = Imm + (OV(N))
          a$ = "Have immune power for " + STR$(Imm) + " turns."
          OL(N) = -1
        ENDIF
        IF Ot(N) = 31 AND Inf > 0 
          OL(N) = -1
          a$ = "It cured the infection."
          Inf = 0
        ENDIF
        IF Ot(N) = 32 AND Psn > 0 
          OL(N) = -1
          a$ = "It stopped the poison."
          Psn = 0
        ENDIF
        IF (Ot(N) = 16 OR Ot(N) = 17 OR Ot(N) = 28 OR Ot(N) = 31 OR Ot(N) = 32) AND OL(N) = -1 
          ADJ = 1
          EXEC Display
          I = INSTR(3, UCASE$(ON$(N)), " OF ")
          IF I > 0 
            a$ = MID$(ON$(N), I)
            ON$(N) = "EMPTY " + a$
            OL(N) = 0
            Ot(N) = 28
          ENDIF
        ENDIF
        IF OL(N) = -1 
          Oc = Oc - 1
        ENDIF
      ENDIF
    NEXT N
ENDPROC

PROC Light_object
     FOR N = 1 TO OBJ
       IF OL(N) = 0 
         IF Ot(N) = 13 
           Lt = Lt + OV(N)
           PR = 0
           L = 1
         ENDIF
         IF Ot(N) = 14 
           Lt = Lt + OV(N)
           OL(N) = -1
           Oc = Oc - 1
         ENDIF
       ENDIF
     NEXT N
     PR = 0
ENDPROC

PROC Collect_with_container
     M = 1
     WHILE M < OBJ AND (Ot(M) <> 29 OR OL(M) > 0)
       M = M + 1
     WEND
     IF Ot(M) = 29 AND OL(M) = 0 
       a$ = "Ok, collecting it."
       OV2(N) = OV2(N) - 1
       IF OV2(N) <= 0 
         OL(N) = -1
       ENDIF
       IF OV(N) = 1 
         Ot(M) = 5
       ENDIF
       IF OV(N) = 2 
         Ot(M) = 16
       ENDIF
       IF OV(N) = 3 
         Ot(M) = 17
       ENDIF
       IF OV(N) = 4 
         Ot(M) = 28
       ENDIF
       IF MID$(ON$(M), 1, 6) = "EMPTY " 
         LET ON$(M) = MID$(ON$(M), 7)
       ENDIF
       B$ = ON$(N) + " OF " + ON$(M)
       LET ON$(M) = B$
     ELSE
       a$ = "Need something to contain it."
     ENDIF
ENDPROC

PROC Find_survival_equipment
     N = 1
     WHILE N < OBJ + 1 AND Ot(N) <> 15
       N = N + 1
     WEND
     IF N >= OBJ + 1 
       DEAD = 1
     ENDIF
ENDPROC

PROC Push_object
     P$ = MID$(a$, 3, LEN(a$))
     WHILE N < OBJ + 1 AND NOT (OL(N) = R AND Ot(N) = 23 AND INSTR(1, P$, ON$(N))) > 0
       N = N + 1
     WEND
     IF OL(N) = R AND Ot(N) = 23 AND INSTR(1, ON$(N), P$) AND N < OBJ 
       IF OV(N) 
         EXEC Reveal
       ENDIF
     ELSE
       PRINT "Can't push it."
     ENDIF
ENDPROC

PROC Open_it
     IF Rn(R) = 3 AND OL(Rnk(R)) = 0 
       Rn(R) = 2
       PR = 0
     ENDIF
     IF Re(R) = 3 AND OL(Rek(R)) = 0 
       Re(R) = 2
       PR = 0
     ENDIF
     IF Rw(R) = 3 AND OL(Rwk(R)) = 0 
       Rw(R) = 2
       PR = 0
     ENDIF
     IF Rs(R) = 3 AND OL(Rsk(R)) = 0 
       Rs(R) = 2
       PR = 0
     ENDIF
     IF Rn(R) = 3 AND OL(Rnk(R)) AND PR = R 
       PRINT "Can't open it , need "; ON$(Rnk(R))
     ENDIF
     IF Re(R) = 3 AND OL(Rek(R)) AND PR = R 
       PRINT "Can't open it , need "; ON$(Rek(R))
     ENDIF
     IF Rw(R) = 3 AND OL(Rwk(R)) AND PR = R 
       PRINT "Can't open it , need "; ON$(Rwk(R))
     ENDIF
     IF Rs(R) = 3 AND OL(Rsk(R)) AND PR = R 
       PRINT "Can't open it , need "; ON$(Rsk(R))
     ENDIF
     IF PR = R 
       P$ = MID$(a$, 3, LEN(a$))
       FOR N = 1 TO OBJ
         IF OL(N) = 0 AND INSTR(1, ON$(N), P$) 
           IF Ot(N) = 19 
             IF OV2(N) = 0 OR OL(OV2(N)) = 0 
               PRINT "Ok, found "; ON$(OV(N)); ". "
               OL(OV(N)) = 0
             ENDIF
             IF OV2(N) > 0 AND OL(OV2(N)) > 0 
               PRINT "Cannot open it, need "; ON$(OV2(N)); ". "
             ENDIF
           ENDIF
           IF Ot(N) = 22 
             B = 0
             IF OV(N) > 99 
               B = INT(OV(N) / 100)
               OV(N) = OV(N) - B * 100
             ENDIF
             IF B = 0 OR OL(B) = 0 
               EXEC Reveal
             ENDIF
           ENDIF
           IF Ot(N) <> 19 AND Ot(N) <> 22 
             PRINT "It will not open."
           ENDIF
         ENDIF
       NEXT N
     ENDIF
ENDPROC

PROC Close_exit
     IF Rn(R) = 2 AND OL(Rnk(R)) = 0 
       Rn(R) = 3
       PR = 0
     ENDIF
     IF Re(R) = 2 AND OL(Rek(R)) = 0 
       Re(R) = 3
       PR = 0
     ENDIF
     IF Rw(R) = 2 AND OL(Rwk(R)) = 0 
       Rw(R) = 3
       PR = 0
     ENDIF
     IF Rs(R) = 2 AND OL(Rsk(R)) = 0 
       Rs(R) = 3
       PR = 0
     ENDIF
ENDPROC

PROC Dig_up_exit
     FOR N = 1 TO OBJ
       IF Ot(N) = 10 AND OL(N) = 0 
         IF Rd(R) = 3 
           Rd(R) = 1
           PR = 0
         ENDIF
         IF Ro(R) = 2 
           Ro(R) = 1
           PR = 0
         ENDIF
       ENDIF
     NEXT N
ENDPROC

PROC Check_password
     P$ = MID$(a$, 3, LEN(a$))
     PW = 0
     PRINT CHR$(34); P$; CHR$(34)
     FOR N = 0 TO Pwd
       a$ = UCASE$(PW$(N))
       IF INSTR(1, UCASE$(P$), a$) > 0 AND a$ <> "" 
         PW = N + 1
       ENDIF
     NEXT N
     IF Rn(R) = 5 AND PW = Rnk(R) 
       Rn(R) = 1
       PR = 0
     ENDIF
     IF Rw(R) = 5 AND PW = Rwk(R) 
       Rw(R) = 1
       PR = 0
     ENDIF
     IF Re(R) = 5 AND PW = Rek(R) 
       Re(R) = 1
       PR = 0
     ENDIF
     IF Rs(R) = 5 AND PW = Rsk(R) 
       Rs(R) = 1
       PR = 0
     ENDIF
     IF Ht(H) = 13 AND PW = HH(H) 
       PRINT "You defeated the "; Hn$(H); ". "
       HH(H) = -1
     ENDIF
     IF Ht(H) = 14 AND PW = HH(H) 
       PRINT "You got rid of the "; Hn$(H); ". "
       N = H
       WHILE Hl(N) = R
       EXEC Random_place
       WEND
     ENDIF
     IF Ro(R) = 5 AND OV3(R) = PW 
       Ro(R) = 1
       PR = 0
     ENDIF
     IF Ro(R) = 6 AND OV3(R) = PW 
       Ro(R) = 1
       PR = 0
     ENDIF
     IF RT(R) = 11 AND Ro(R) = PW 
       R = Rol(R)
     ENDIF
     FOR N = 1 TO OBJ
       IF Ot(N) > 34 AND Ot(N) < 47 AND OV2(N) = PW AND OL(N) = 0 
         IF Ot(N) = 35 
           OL(N) = -1
           SP! = SP! + OV(N)
         ENDIF
         IF Ot(N) = 36 
           R = OV(N)
         ENDIF
         IF Ot(N) = 38 
           Lt = Lt + OV(N)
           OV(N) = 0
           EXEC Light_object
           L = 1
         ENDIF
         IF Ot(N) = 39 
           EXEC Kill_Hostilities
         ENDIF
         IF Ot(N) = 40  
            EXEC Entrap_hostilities
         ENDIF
         IF Ot(N) = 42 
           EXEC Search_For_Components
           OL(N) = -1
         ENDIF
       ENDIF
       IF Ot(N) = 37 
         B = 0
         IF OV(N) > 99 
           B = INT(OV(N) / 100)
           OV(N) = OV(N) - B * 100
         ENDIF
         IF PW = B 
           EXEC Reveal
         ENDIF
       ENDIF
     NEXT N
ENDPROC

PROC Correct_action
     IF INSTR(1, R$, PC$) OR INSTR(1, PC$, R$) 
       R = Ro(R)
     ELSE
       R = Rol(R)
     ENDIF
ENDPROC

PROC Burn_object
     IF L > 0 AND Lt > 0 
       P$ = MID$(a$, 3, LEN(a$))
       FOR N = 1 TO OBJ
         IF Ot(N) = 24 AND OL(N) = R AND INSTR(1, ON$(N), P$) 
           IF OV(N) > 0 
             EXEC Reveal
           ENDIF
           OL(N) = -1
           IF SP! < 0 
             DEAD = 1
           ENDIF
         ENDIF
       NEXT N
     ELSE
       PRINT "Need something to burn it with."
     ENDIF
ENDPROC

PROC Reveal
     IF OV(N) = 1 
       OL(OV2(N)) = R
       PR = 0
     ENDIF
     IF OV(N) = 2 
       Rd(N) = 1
       Rdl(N) = OV2(N)
     ENDIF
     IF OV(N) = 3 
       Ro(R) = 1
       Rol(R) = OV2(N)
       REN$(R) = Od$(N)
     ENDIF
     IF OV(N) = 4 
       Rn(N) = 1
       Rnl(N) = OV2(N)
     ENDIF
     IF OV(N) = 5 
       Re(N) = 1
       Rel(N) = OV2(N)
     ENDIF
     IF OV(N) = 6 
       Rw(N) = 1
       Rwl(N) = OV2(N)
     ENDIF
     IF OV(N) = 7 
       Rs(N) = 1
       Rsl(N) = OV2(N)
     ENDIF
     IF OV(N) = 8 
       Ru(N) = 1
       Rul(N) = OV2(N)
     ENDIF
     IF OV(N) = 10 
       B = INT(OV(N) / 100)
       a = OV(N) - B * 100
       FOR M = a TO B
         OL(M) = 0
         PRINT "Ok, Found "; ON$(N)
       NEXT M
     ENDIF
     IF OV(N) = 11 THEN EXEC Examine_object
     IF OV(N) = 9 AND (Ot(N) = 22 OR Ot(N) = 23 OR Ot(N) = 24 OR Ot(N) = 26 OR Ot(N) = 36) 
       Hl(OV2(N)) = R
       PR = 0
     ENDIF
     IF OV(N) = 9 AND Ot(N) = 24 
       a$ = "The " + ON$(N) + " exploded. Damaged by " + STR$(OV2(N)) + C$
       SP! = SP! - OV2(N)
       EXEC Display
     ENDIF
     PR = 0
     OV(N) = 0
ENDPROC

PROC Enter_other_exit
     IF INSTR(1, R$, MID$(a$, 3)) 
       IF Ro(R) = 1 OR Ro(R) = 7 
         R = Rol(R)
       ENDIF
       IF Ro(R) = 4 
         PRINT "Can't go that way, it is blocked."
       ENDIF
       IF Ro(R) = 8 
         PRINT "Can't go that way yet."
       ENDIF
       IF Ro(R) = 3 
         M = Rol(R)
         EXEC Ok_to_climb
       ENDIF
     ENDIF
ENDPROC

PROC Kill_Hostilities
     FOR M = 1 TO HOS
       IF Hl(M) = R THEN HH(N) = -1
       IF Hl(M) = Rnl(R) THEN HH(M) = -1
       IF Hl(M) = Rel(R) THEN HH(M) = -1
       IF Hl(M) = Rwl(R) THEN HH(M) = -1
       IF Hl(M) = Rsl(R) THEN HH(M) = -1
       IF Hl(M) = Rul(R) THEN HH(M) = -1
       IF Hl(M) = Rdl(R) THEN HH(M) = -1
       IF Hl(M) = Rol(R) THEN HH(M) = -1
     NEXT M
ENDPROC

PROC Entrap_hostilities
     FOR M = 1 TO HOS
       IF Hl(M) = R THEN Hm(M) = 1
       IF Hl(M) = Rnl(R) THEN Hm(M) = 1
       IF Hl(M) = Rel(R) THEN Hm(M) = 1
       IF Hl(M) = Rwl(R) THEN Hm(M) = 1
       IF Hl(M) = Rsl(R) THEN Hm(M) = 1
       IF Hl(M) = Rul(R) THEN Hm(M) = 1
       IF Hl(M) = Rdl(R) THEN Hm(M) = 1
       IF Hl(M) = Rol(R) THEN Hm(M) = 1
     NEXT M
ENDPROC

PROC Lock_exits
     IF Rn(R) = 2 THEN Rn(R) = 3
     IF Re(R) = 2 THEN Re(R) = 3
     IF Rw(R) = 2 THEN Rw(R) = 3
     IF Rs(R) = 2 THEN Rs(R) = 3
ENDPROC

PROC Search_For_Components
     P$ = MID$(a$, 3)
     B$ = ""
     a = 1
     B = -1
     t1 = 1
     FOR N = 1 TO OBJ
       IF Ot(N) = 33 THEN EXEC Make_object
     NEXT N
ENDPROC

PROC Make_object
     B = N
     B$ = ""
     REPEAT
       P = OV(B)
       IF OL(B) > 0 THEN t1 = 0
       B$ = B$ + CHR$(B)
       IF P > 0 
         a = a + 1
         B = P
       ENDIF
     UNTIL P = 0
     P = OV2(B)
     IF INSTR(1, P$, ON$(M)) OR INSTR(1, ON$(M), P$) 
       IF t1 = 1 
         PRINT "Ok, made "; LCASE$(ON$(P)); ". "
         PR = 0
         IF P < 1000 
           OL(P) = R
         ELSE
           Ro(R) = (Ro(R) = 8)
         ENDIF
         FOR M = 1 TO a
           OL(ASC(MID$(B$, M, 1))) = -1
           Oc = Oc - 1
         NEXT M
       ELSE
        PRINT "Don't have everything to make "; LCASE$(ON$(P)); ". "
       ENDIF
     ENDIF
ENDPROC

PROC Blow_open_exit
     P$ = MID$(a$, 3)
     FOR N = 1 TO OBJ
       IF OL(N) = 0 AND (Ot(N) = 7 OR Ot(N) = 8) AND INSTR(1, ON$(N), P$) > 0 
         EXEC Open_other_exit
       ENDIF
     NEXT N
ENDPROC

PROC Open_other_exit
     IF (OL(OV(N)) = 0 OR OV(N) = 0) AND Ro(R) = 4 
       Ro(R) = 1
       PR = 0
     ENDIF
ENDPROC

PROC Break_object
     M = 1
     WHILE M < OBJ AND Ot(M) <> 27
       M = M + 1
     WEND
     IF Ot(M) = 27 
       FOR N = 1 TO OBJ
         IF OL(N) = R AND Ot(N) = 26 
           IF OV(N) > 1 
             EXEC Reveal
           ENDIF
         ELSE
           IF Ot(N) = 25 AND OL(N) = 0 
             OL(OV(N)) = 0
             OL(N) = -1
           ENDIF
         ENDIF
       NEXT N
     ELSE
       PRINT "Need something to break it."
     ENDIF
ENDPROC

PROC Fight_hostility
     IF H > 0 AND (Ht(H) < 13 OR Ht(H) = 15) 
       IF LEN(a$) > 2 
         P$ = MID$(a$, 3)
         Hd = 0
         FOR N = 1 TO OBJ
           t1 = OV2(N)
           IF OL(N) = 0 AND Ot(N) = 3 
             IF t1 > OBJ OR t1 = 0 
               t1 = N
             ENDIF
             IF INSTR(1, ON$(N), P$) > 0 OR INSTR(1, ON$(t1), P$) > 0 
               IF OV2(N) = 0 
                 Hd = OV(N)
               ENDIF
               IF OV2(N) > 0 AND OV2(N) < 500 
                 IF OV(OV2(N)) > 0 AND OL(OV2(N)) = 0 
                   OV(OV2(N)) = OV(OV2(N)) - 1
                   Hd = OV(N)
                 ENDIF
                 IF OV(OV2(N)) <= 0 AND OL(OV2(N)) = 0 
                   Hd = 0
                   PRINT "You are out of "; ON$(OV2(N)); ". "
                 ENDIF
               ENDIF
               IF OV2(N) = 500 
                 IF MID$(a$, 1, 2) = "K1" 
                 OL(N) = R
                 Hd = OV(N)
                 ELSE
                 PRINT "It must be thrown."
                 ENDIF
               ENDIF
               IF OV2(N) = 510 
                 Hd = OV(N)
                 OL(N) = -1
               ENDIF
               IF Ht(H) = 12 AND Ot(N) = HH(H) 
                 HH(H) = 0
                 Hd = 1
               ELSE
                 IF Ht(H) = 0 
                   Hd = 0
                 ENDIF
               ENDIF
             ENDIF
           ELSE
             IF P$ = ON$(N) 
               IF (Ot(N) < 3 OR Ot(N) > 4) AND OL(N) = 0 
                 PRINT "It is not a weapon."
               ENDIF
               IF OL(N) > 0 
                 PRINT "It is not in your inventory."
               ENDIF
             ENDIF
           ENDIF
         NEXT N
       ENDIF
       IF LEFT$(a$, 2) = "K2" 
         Hd = S26
       ENDIF
       IF LEFT$(a$, 2) = "K3" 
         Hd = S25
       ENDIF
       IF Hd > 0 
         HH(H) = HH(H) - Hd
         IF HH(H) >= 0 
           a$ = "You just damaged the " + Hn$(H) + " by " + STR$(Hd) + " hitpoints."
           ADJ = 1
           EXEC Display
         ELSE
           PRINT "You killed the "; Hn$(H); ". "
           IF Ht(H) = 6 AND HS(H) > 0 
             PRINT "Found "; ON$(HS(H)); ". "
             OL(HS(H)) = R
             HS(H) = 0
           ENDIF
           EXEC Search_dead_inventory
           Ht(H) = Ht(H) OR 128
           Hm(H) = Hm(H) OR 128
           H = 0
           IF S19 
             PRINT "You have been awarded "; S19; " "; CU$; ". "
             C = C + S19
           ENDIF
         ENDIF
       ENDIF
     ELSE
       IF H = 0 
         PRINT "There is nothing here."
       ENDIF
       IF Ht(H) > 12 AND Ht(H) < 15 
         PRINT "You caused no damage."
       ENDIF
     ENDIF
ENDPROC

PROC Search_dead_inventory
     FOR N = 1 TO OBJ
       IF OL(N) > 1999 
         IF OL(N) - 2000 = H 
           OL(N) = 0
           PRINT "Now found the "; ON$(N); ". "
         ENDIF
       ENDIF
     NEXT N
ENDPROC

PROC Hostility_attacks
     a$ = "NO ATTACK "
     IF Ht(H) = 1 OR Ht(H) = 12 OR Ht(H) = 13 
       IF S21 = 0 THEN P! = ((100 - ARMR) / 100) * HS(H)
       IF S21 > 0 THEN P! = (1 - ARMR / (50 + ARMR)) * HS(H)
       P! = INT(((.5 + P!) * 100) / 100)
       SP! = SP! - INT(P!)
       IF SP! > 0 
         a$ = "The " + Hn$(H) + " attacked, and lost " + STR$(P!) + C$ + ". "
       ENDIF
     ENDIF
     IF Ht(H) = 2 OR SP! < 0 
       DEAD = 1
       a$ = "The " + Hn$(H) + " killed you."
       IF PR > 0 THEN R = PR
     ENDIF
     IF Ht(H) = 3 OR Ht(H) = 4 OR Ht(H) = 5 
       FOR N = 1 TO OBJ
         IF OL(N) = 0 
           IF Ht(H) = 3 
             OL(N) = R
           ENDIF
           IF Ht(H) = 4 AND Ot(N) = 1 
             OL(N) = R
             a$ = "The " + Hn$(H) + " attacked and you lost all treasures."
           ENDIF
           IF Ht(H) = 5 AND Ot(N) = 2 
             OL(N) = R
             a$ = "The " + Hn$(H) + " attacked and you lost all keys."
           ENDIF
           IF Ht(H) = 15 AND Ot(N) = 3 OR Ot(N) = 4 
             OL(N) = R
             a$ = "The " + Hn$(H) + " attacked and you lost all weapons."
           ENDIF
         ENDIF
       NEXT N
     ENDIF
     IF Ht(H) = 9 
       R = HS(H)
     ENDIF
     IF Ht(H) = 10 AND C > HS(H) 
       C = C - HS(H)
       a$ = "The " + Hn$(H) + " just took " + STR$(HS(H)) + " " + CU$ + ". "
     ENDIF
     IF Ht(H) = 16 
       a$ = "The " + Hn$(H) + " just bit you. The wounds are infected."
       Inf = HS(H)
     ENDIF
     IF Ht(H) = 17 
       a$ = "The " + Hn$(H) + " just bit you and injected poison."
       Psn = HS(H)
     ENDIF
     IF a$ <> "NO ATTACK " 
       ADJ = 1
       EXEC Display
     ENDIF
ENDPROC

PROC Move_hostilities
     IF S11 = 1 
       FOR N = 1 TO HOS
         IF HH(N) >= 0 
           EXEC Random_place
         ENDIF
         IF Ht(N) = 6 
           EXEC Move_objects_around
         ENDIF
         IF Ht(N) = 11 
           EXEC Heal_hostility
         ENDIF
         IF Hl(N) = R 
           EXEC Hostility_present
         ENDIF
       NEXT N
     ENDIF
     IF S11 = 2 
       FOR N = 1 TO HOS
         IF HH(N) >= 0 
           EXEC Random_place
         ENDIF
         IF Ht(N) = 6 
           EXEC Move_objects_around
         ENDIF
         IF Ht(N) = 11 
           EXEC Heal_hostility
         ENDIF
         IF Hl(N) = R 
           EXEC Hostility_present
         ENDIF
       NEXT N
     ENDIF
     IF S11 = 3 
       FOR N = 1 TO HOS         
           IF Hm(N) = 2
             EXEC Random_place
           ELIF Hm(N) = 3
             B = INT(RND * 7)
             EXEC Move_place
           ELIF Hm(N) = 4
             EXEC Inc_place_number
           ELIF Hm(N) = 5
             B = INT(RND * 4)
             EXEC Move_place
           ELIF Hm(N) = 6
             EXEC Random_place
           ELIF Hm(N) = 7
             EXEC Dec_place_number
           ELIF Hm(N) = 8
             EXEC Persue_player
           ENDIF
         IF Ht(N) = 6 
           EXEC Move_objects_around
         ELSEIF Ht(N) = 11 
           EXEC Heal_hostility
         ENDIF
         IF Hl(N) = R THEN EXEC Hostility_present
       NEXT N
     ENDIF
ENDPROC

PROC Move_place
     a = Hl(N)
     IF B = 0 AND Rn(a) = 1 OR Rn(a) = 2 OR Rn(a) = 4 THEN Hl(N) = Rnl(a)
     IF B = 1 AND Re(a) = 1 OR Re(a) = 2 OR Re(a) = 4 THEN Hl(N) = Rel(a)
     IF B = 2 AND Rw(a) = 1 OR Rw(a) = 2 OR Rw(a) = 4 THEN Hl(N) = Rwl(a)
     IF B = 3 AND Rs(a) = 1 OR Rs(a) = 2 OR Rs(a) = 4 THEN Hl(N) = Rsl(a)
     IF B = 4 AND Ru(a) = 1 OR Ru(a) = 2 OR Ru(a) = 4 THEN Hl(N) = Rul(a)
     IF B = 5 AND Rd(a) = 1 OR Rd(a) = 2 OR Rd(a) = 4 THEN Hl(N) = Rdl(a)
     IF B = 6 AND Ro(a) = 1 OR Ro(a) = 3 THEN Hl(N) = Rol(a)
     M = RT(Hl(N))
     IF M > 1 AND M < 4 OR M > 6 AND M < 11 OR M > 14 THEN Hl(N) = a
ENDPROC

PROC Random_place
     a = INT(RND * Plc) + 1
     M = RT(a)
     IF (M > 1 AND M < 4 OR M > 6 AND M < 11 OR M > 14) AND Hm(N) = 2 
       Hl(N) = a
     ENDIF
     IF M <> 2 AND M <> 8 AND Hm(N) = 6 
       Hl(N) = a
     ENDIF
ENDPROC

PROC Inc_place_number
     REPEAT
       Hl(N) = Hl(N) + 1
       IF Hl(N) > Plc THEN Hl(N) = 1
       M = RT(Hl(N))
     UNTIL M = 1 OR M = 4 OR M = 5 OR M = 6 OR M > 10
ENDPROC

PROC Dec_place_number
     REPEAT
       Hl(N) = Hl(N) - 1
       IF Hl(N) < 1 THEN Hl(N) = Plc
       M = RT(Hl(N))
     UNTIL M = 1 OR M = 4 OR M = 5 OR M = 6 OR M > 10
ENDPROC

PROC Persue_player
     a = Hl(N)     
       IF R = Rnl(a)
         Hl(N) = Rnl(N)
       ELIF R = Rel(a)
         Hl(N) = Rel(N)
       ELIF R = Rwl(a)
         Hl(N) = Rwl(N)
       ELIF R = Rsl(a)
         Hl(N) = Rsl(N)
       ELIF R = Rul(a)
         Hl(N) = Rul(N)
       ELIF R = Rdl(a)
         Hl(N) = Rdl(N)
       ELIF R = Rol(a)
         Hl(N) = Rol(N)
       ELSE
         B = INT(RND * 7)
         EXEC Move_place
        ENDIF
     IF M > 1 AND M < 4 OR M > 6 AND M < 11 OR M > 14 THEN Hl(N) = a
ENDPROC

PROC Move_objects_around
     M = 1 + INT(RND * 2)
     IF HS(N) > 0 AND M = 1 
       OL(HS(N)) = Hl(N)
       HS(N) = 0
     ELSE
       IF HS(N) = 0 
         WHILE M < OBJ AND NOT OL(M) = Hl(N)
           M = M + 1 + INT(RND(3))
         WEND
         IF OL(M) = Hl(N) AND Ot(M) <> 23 AND Ot(M) <> 24 AND Ot(M) <> 26 
           OL(M) = 1000
           HS(N) = M
         ENDIF
       ENDIF
     ENDIF
ENDPROC

PROC Heal_hostility
     a = Hl(N)
     M = 1 + INT(RND * 2)
     WHILE M < HOS
       IF Hl(M) = a AND HH(M) < 0 AND (Ht(H) < 12 OR Ht(M) > 14) 
         IF S23 > 0 THEN HH(M) = S23
         IF S23 < 0 THEN HH(M) = HO(M)
         Ht(M) = Ht(M) AND 127
         Hm(M) = Hm(M) AND 127
       ENDIF
       M = M + INT(RND * 3)
     WEND
ENDPROC

PROC Answer_riddle
     EXEC Place_Info
     PRINT "Enter Answer."
     INPUT a$
     IF INSTR(1, R$, a$) 
       IF Rn(R) = 1 
         R = Rnl(R)
       ELSE
         IF Rn(R) = 2 
           SP! = SP! + Rnl(R)
         ENDIF
         IF Rn(R) = 3 
           C = C + Rnl(R)
         ENDIF
         IF Rn(R) = 4 
           TN = TN + Rnl(R)
         ENDIF
         IF Rn(R) = 5 
           FW = FW + Rnl(R)
         ENDIF
         IF Rn(R) = 6 
           LIVES = LIVES + Rnl(R)
         ENDIF
         R = Rol(R)
       ENDIF
     ELSE
       IF Rw(R) = 1 
         R = Rwl(R)
       ELSE
         IF Rw(R) = 2 
           SP! = SP! - Rwl(R)
           IF SP! < 0 
             SP! = 0
           ENDIF
         ENDIF
         IF Rw(R) = 3 
           C = C - Rwl(R)
           IF C < 0 
             C = 0
           ENDIF
         ENDIF
         IF Rw(R) = 4 
           TN = TN - Rwl(R)
         ENDIF
         IF Rw(R) = 5 
           FW = FW - Rwl(R)
           IF FW < 3 
             FW = 3
           ENDIF
         ENDIF
         IF Rw(R) = 6 
           DEAD = 1
         ENDIF
         R = Rol(R)
       ENDIF
     ENDIF
ENDPROC

PROC Measure_armor
  IF S21 < 3 
    ARMR = 0
    FOR M = 1 TO OBJ
      IF Ot(M) = 6 AND OL(M) = 0 
        IF ARMR < OV(M) AND (S21 = 0 OR S21 = 2) THEN ARMR = OV(M)
        IF S21 = 1 THEN ARMR = ARMR + OV(M)
        IF ARMR > 99 THEN ARMR = 99
      ENDIF
    NEXT M
  ELSEIF S21 = 3 
    ARMR = 0
    FOR M = 46 TO 53
      P = 0
      FOR O = 1 TO OBJ
        IF Ot(O) = M AND P < OV(M) THEN P = OV(M)
      NEXT O
      ARMR = ARMR + P
      IF ARMR > 99 THEN ARMR = 99
    NEXT M
  ENDIF
ENDPROC

PROC Transport
     P$ = MID$(a$, 3, LEN(a$))
     MID$(a$, 16) = " "
     FOR N = 1 TO Plc
       IF INSTR(0, P$, Rd$) > 0 AND RT(N) = 13 THEN
         R = N
       ENDIF
     NEXT N
ENDPROC

PROC Get_game
     CLS
     PRINT "                       TEXT ADVENTURE GAME PLAYER          IBM PC Version 1.00"
     PRINT "                            BY PETER J. MEYER"
     EXEC Load_file
     ON ERROR GOTO 0
     a$ = OP$
     Oc = 0
     CLS
     IF LEFT$(T$, 1) <> "|" THEN
       PRINT TAB(40 - LEN(T$) / 2); T$
     ELSE
       a$ = MID$(T$, 2)
       LOADPICTURE a$
     ENDIF
     IF I$ <> "" 
       PRINT TAB(39); "By"
       PRINT TAB(40 - LEN(I$) / 2); I$
     ENDIF
     PRINT
     a$ = OP$
     ADJ = 2
     EXEC Display
     PRINT
     PRINT "Press any key to begin program.";
     WHILE INKEY$ = ""
     WEND
     SCREEN 0, 0, 0
     IF S18 = 0 OR S8 > 0 
       C$ = " shield points"
     ENDIF
     IF S18 = 1 
       C$ = " health points"
     ENDIF
     IF S18 = 2 
       C$ = " hit points"
     ENDIF
     IF S18 = 3 
       C$ = " life points"
     ENDIF
     FOR N = 1 TO OBJ
       IF OL(N) = 0 
         Oc = Oc + 1
       ENDIF
     NEXT N
     IF S15 > 0 
       LIVES = S15
     ELSE
       LIVES = 1
     ENDIF
     IF S23 < 0 
       REDIM HO(HOS)
       FOR N = 1 TO HOS
         HO(N) = HH(N)
       NEXT N
     ENDIF
     C = S22
     SP! = S8
     Imm = 0
     Psn = 0
     Inf = 0
     FW = S13
     R = 1
     DEAD = 0
     WIN = 0
     IF S28 = 0 
        RANDOMIZE TIMER
     ELSE
        RANDOMIZE S28
     ENDIF
ENDPROC

Game_over
     IF WIN = 1 
       MID$(a$, 640) = " "
       a$ = CP$
       Oc = 0
       PRINT " ";
       ADJ = 2
       EXEC Display
       PRINT
       PRINT "GAME SOLVED."
     ELSE
       PRINT "GAME OVER."
     ENDIF
     PRINT "(P)lay other, (M)ain menu, or (Q)uit."
     CHOICE = 0
     WHILE CHOICE <> 77 AND CHOICE <> 80 AND CHOICE <> 81
       a$ = INKEY$
       IF LEN(a$) = 1 THEN CHOICE = ASC(a$)
     WEND
ENDPROC

PROC Load_file
  EXEC Get_directory
  EXEC Get_file_name
  OPEN F$ FOR INPUT AS #1
  INPUT #1, a
  OP$ = INPUT$(a, 1)
  INPUT #1, a
  CP$ = INPUT$(a, 1)
  LINE INPUT #1, T$
  LINE INPUT #1, I$
  LINE INPUT #1, Nf$
  LINE INPUT #1, CU$
  LINE INPUT #1, a$
  INPUT #1, S0
  INPUT #1, S1
  INPUT #1, S2
  INPUT #1, S3
  INPUT #1, S4
  INPUT #1, S5
  INPUT #1, S6
  INPUT #1, S7
  INPUT #1, S8
  INPUT #1, S9
  INPUT #1, S10
  INPUT #1, S11
  INPUT #1, S12
  INPUT #1, S13
  INPUT #1, S14
  INPUT #1, S15
  INPUT #1, S16
  INPUT #1, S17
  INPUT #1, S18
  INPUT #1, S19
  INPUT #1, S20
  INPUT #1, S21
  INPUT #1, S22
  INPUT #1, S23
  INPUT #1, S24
  INPUT #1, S25
  INPUT #1, S26
  INPUT #1, S27
  INPUT #1, S28
  INPUT #1, S29
  INPUT #1, S30
  INPUT #1, S31
  INPUT #1, S32
  INPUT #1, S33
  INPUT #1, S34
  INPUT #1, S35
  INPUT #1, S36
  INPUT #1, S37
  INPUT #1, S38
  INPUT #1, S39
  INPUT #1, S40
  INPUT #1, S41
  INPUT #1, S42
  INPUT #1, S43
  INPUT #1, S44
  INPUT #1, S45
  INPUT #1, S46
  INPUT #1, S47
  INPUT #1, Pwd
  FOR N = 0 TO Pwd
   INPUT #1, PW$(N)
  NEXT N
  INPUT #1, OBJ
  FOR N = 1 TO OBJ
   INPUT #1, Ot(N)
   INPUT #1, OP(N)
   INPUT #1, OL(N)
   INPUT #1, OV(N)
   INPUT #1, OV2(N)
   INPUT #1, OV3(N)
   INPUT #1, ON$(N)
   LINE INPUT #1, Od$(N)
  NEXT N
  INPUT #1, HOS
  FOR N = 1 TO HOS
   INPUT #1, Hl(N)
   INPUT #1, Ht(N)
   INPUT #1, HS(N)
   INPUT #1, HH(N)
   INPUT #1, Hv(N)
   INPUT #1, Hm(N)
   LINE INPUT #1, Hn$(N)
  NEXT N
   INPUT #1, Plc
   FOR N = 1 TO Plc
   INPUT #1, RT(N)
   INPUT #1, Rn(N)
   INPUT #1, Rnl(N)
   INPUT #1, Rnk(N)
   INPUT #1, Rw(N)
   INPUT #1, Rwl(N)
   INPUT #1, Rwk(N)
   INPUT #1, Re(N)
   INPUT #1, Rel(N)
   INPUT #1, Rek(N)
   INPUT #1, Rs(N)
   INPUT #1, Rsl(N)
   INPUT #1, Rsk(N)
   INPUT #1, Ru(N)
   INPUT #1, Rul(N)
   INPUT #1, Rd(N)
   INPUT #1, Rdl(N)
   INPUT #1, Ro(N)
   INPUT #1, Rol(N)
   LINE INPUT #1, REN$(N)
   INPUT #1, Rov(N)
   LINE INPUT #1, Rd$(N)
  NEXT N
  CLOSE #1
  Modified = 0
ENDPROC

PROC Get_directory
  ON ERROR GOTO CHECK_ERROR
  CHDIR PATH$
  FILES "*.TGF"
  ON ERROR GOTO 0
ENDPROC

PROC CHECK_ERROR
  ON ERROR GOTO CHECK_ERROR
RESUME NEXT

PROC Get_file_name
  INPUT "Name of File:", a$
  a$ = UCASE$(a$)
  IF INSTR(1, a$, ".TGF") = 0 THEN a$ = a$ + ".TGF"
  F$ = a$
ENDPROC

SUB LOADPICTURE (F$)
  IF INSTR(1, F$, ".") = 0 THEN F$ = F$ + ".GRF"
  OPEN F$ FOR INPUT AS #1
  INPUT #1, a
  INPUT #1, PCOL
  INPUT #1, HIGHT
  INPUT #1, a
  SETSCREEN
  FOR N = 1 TO HIGHT
    LINE INPUT #1, a$
    O = VX + 1
    FOR M = 1 TO LEN(a$)
      P = ASC(MID$(a$, M, 1))
      TR = INT(P / 16)
      LINE (O, N)-(O + TR - 1, N), P AND 15
      O = O + TR
    NEXT M
  NEXT N
  CLOSE #1
  LOCATE VY, 1
END SUB

SUB SETSCREEN
  VX = 0
  VY = 0
  CHRWIDTH = 8
  IF ADAPTER = 1 
    SCREEN 3
    NCOL = 1
    VX = 40
    MAXHIGHT = 348
    CHRHIGHT = 14
    CHRWIDTH = 9
  ENDIF
  IF ADAPTER > 1 AND PCOL = 0 
    SCREEN 2
    NCOL = 1
    MAXHIGHT = 200
    CHRHIGHT = 8
  ENDIF
  IF ADAPTER = 3 AND PCOL = 1 
    SCREEN 9
    NCOL = 15
    MAXHIGHT = 350
    CHRHIGHT = 14
  ENDIF
  IF ADPATER = 4 AND PCOL = 1 
    SCREEN 12
    NCOL = 15
    MAXHIGHT = 480
    CHRHIGHT = 16
  ENDIF
  VY = INT(HIGHT / CHRHIGHT) + 1
END SUB

