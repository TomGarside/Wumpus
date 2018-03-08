       IDENTIFICATION DIVISION.
       PROGRAM-ID. WUMPUS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FLAGS.
          05 FIRST-RAND                         PIC 9(1) VALUE 0.
          05 GAMEOVER                           PIC 9(1) VALUE 0.
       01 CURRENT-TIME                          PIC 9(18) VALUE 0.
       01 NUM                                   PIC 9(2) VALUE 5.
       01 SEED                                  PIC 9(2) VALUE 0.
       01 USER-INPUT                            PIC X(20) VALUE SPACES.

       01 ADVENTURER.
          05 ARROWS                             PIC 9(1) VALUE 5.
          05 CURRENT-ROOM                       PIC 9(2) VALUE 3.
       01 ROOMS.
          05 WUMPUS-ROOM                        PIC 9(2)  VALUE ZERO.
          05 ROOM  OCCURS 20 TIMES.
              10   PASSAGE1                     PIC 9(02) VALUE 0.
              10   PASSAGE2                     PIC 9(02) VALUE 0.
              10   PASSAGE3                     PIC 9(02) VALUE 0.
              10   WUMPUS                       PIC 9(01) VALUE 0.
              10   PIT                          PIC 9(01) VALUE 0.
              10   BAT                          PIC 9(01) VALUE 0.
       01 ARROW-PATH.
          05 COM                                PIC X(20) VALUE SPACES.
          05 ARROW-ROOM OCCURS 5 TIMES.
              10 AROOM                          PIC 9(02) VALUE 0.

       PROCEDURE DIVISION.

       P-100-MAIN.
           PERFORM P-500-PRINT-INTRO THRU P-599-EXIT.
           PERFORM P-200-GEN-CAVE THRU P-200-EXIT.
           PERFORM P-400-GAME-LOOP UNTIL GAMEOVER=1
           GOBACK.
       P-199-EXIT.
           EXIT.

       P-200-GEN-CAVE.
       MOVE 020506000 TO ROOM(1)
       MOVE 010307000 TO ROOM(2)
       MOVE 020408000 TO ROOM(3)
       MOVE 030509000 TO ROOM(4)
       MOVE 010410000 TO ROOM(5)
       MOVE 011112000 TO ROOM(6)
       MOVE 031213000 TO ROOM(7)
       MOVE 031314000 TO ROOM(8)
       MOVE 041415000 TO ROOM(9)
       MOVE 051115000 TO ROOM(10)
       MOVE 061016000 TO ROOM(11)
       MOVE 060717000 TO ROOM(12)
       MOVE 070818000 TO ROOM(13)
       MOVE 080919000 TO ROOM(14)
       MOVE 091020000 TO ROOM(15)
       MOVE 111720000 TO ROOM(16)
       MOVE 121618000 TO ROOM(17)
       MOVE 131719000 TO ROOM(18)
       MOVE 141820000 TO ROOM(19)
       MOVE 151619000 TO ROOM(20)

       PERFORM P-300-GEN-RANDNO
       MOVE 1 TO WUMPUS (NUM)

       PERFORM P-300-GEN-RANDNO
       MOVE 1 TO BAT (NUM)

       PERFORM P-300-GEN-RANDNO
       MOVE 1 TO BAT (NUM)

       PERFORM P-300-GEN-RANDNO
       MOVE 1 TO PIT (NUM)

       PERFORM P-300-GEN-RANDNO
       MOVE 1 TO PIT (NUM).

       P-200-EXIT.
          EXIT.

       P-300-GEN-RANDNO.
          IF FIRST-RAND EQUAL ZERO THEN
            ACCEPT CURRENT-TIME FROM TIME
            MOVE CURRENT-TIME(16:2) TO SEED
            COMPUTE NUM = FUNCTION RANDOM(SEED) * 20 + 1
            MOVE 1 TO FIRST-RAND
          ELSE
            COMPUTE NUM = FUNCTION RANDOM * 20 + 1
          END-IF.
       P-300-EXIT.
          EXIT.

       P-400-GAME-LOOP.

        IF BAT(CURRENT-ROOM) EQUAL 1 THEN
            DISPLAY 'OH NO GRABBED BY A SUPER BAT'
            PERFORM P-300-GEN-RANDNO
            MOVE NUM TO CURRENT-ROOM
        END-IF

        IF WUMPUS(CURRENT-ROOM) EQUAL 1 THEN
            DISPLAY 'YOU ARE DEVOURED BY THE WUMPUS'
            MOVE 1 TO GAMEOVER
            NEXT SENTENCE
        END-IF

        IF PIT(CURRENT-ROOM) EQUAL 1 THEN
            DISPLAY 'YOU STUMBLE DOWN A BOTTOMLESS PIT'
            MOVE 1 TO GAMEOVER
            NEXT SENTENCE
        END-IF

        IF WUMPUS(PASSAGE1(CURRENT-ROOM)) EQUAL 1 OR
           WUMPUS(PASSAGE2(CURRENT-ROOM)) EQUAL 1 OR
           WUMPUS(PASSAGE3(CURRENT-ROOM)) EQUAL 1
           THEN
           DISPLAY "YOU SMELL THE DANK ODOUR OF THE WUMPUS"
        END-IF

        IF BAT(PASSAGE1(CURRENT-ROOM)) EQUAL 1 OR
           BAT(PASSAGE2(CURRENT-ROOM)) EQUAL 1 OR
           BAT(PASSAGE3(CURRENT-ROOM)) EQUAL 1
           THEN
           DISPLAY "YOU HEAR A DISTANT FLAPPING"
        END-IF

        IF PIT(PASSAGE1(CURRENT-ROOM)) EQUAL 1 OR
           PIT(PASSAGE2(CURRENT-ROOM)) EQUAL 1 OR
           PIT(PASSAGE3(CURRENT-ROOM)) EQUAL 1
           THEN
           DISPLAY "YOU FEEL A COLD BREEZE"
        END-IF.

        IF GAMEOVER EQUAL ZERO THEN
            PERFORM P-600-ACCEPT-COMMAND THRU P-699-EXIT
        END-IF.

       P-499-EXIT.
          EXIT.

       P-500-PRINT-INTRO.
        DISPLAY "INTRO PLACE HOLDER".

       P-599-EXIT.
          EXIT.

       P-600-ACCEPT-COMMAND.
         DISPLAY 'YOU ARE IN ROOM ' CURRENT-ROOM
         DISPLAY "WHAT WOULD YOU LIKE TO DO ?"
         ACCEPT USER-INPUT FROM CONSOLE
         EVALUATE USER-INPUT(1:4)
                  WHEN "GO 1"
                      MOVE PASSAGE1(CURRENT-ROOM) TO CURRENT-ROOM
                  WHEN "GO 2"
                      MOVE PASSAGE2(CURRENT-ROOM) TO CURRENT-ROOM
                  WHEN "GO 3"
                      MOVE PASSAGE3(CURRENT-ROOM) TO CURRENT-ROOM
                  WHEN "SHOO"
                      DISPLAY "TWAAANG"
                      UNSTRING USER-INPUT DELIMITED BY SPACES INTO COM,
                      AROOM(1), AROOM(2), AROOM(3), AROOM(4), AROOM(5)
                      PERFORM P-700-SHOOT-ARROW THRU P-799-EXIT
                  WHEN "QUIT"
                      GOBACK
                  WHEN "PRIN"
                       DISPLAY " <-1---2---3---4---5->
                                __/__/___/___/___/_
                                _6___7___8___9___10
                                / \ / \ / \ / \ / \
                               11__12__13__14__15__
                              __\___\___\___\___\__
                              <-16--17--18--19--20-> "
                  WHEN OTHER
                      DISPLAY "IM SORRY I DONT UNDERSTAND"
                      DISPLAY "TRY GO 1,2 OR 3"

         END-EVALUATE.

       P-699-EXIT.
        EXIT.

       P-700-SHOOT-ARROW.
          COMPUTE ARROWS = ARROWS - 1

          IF AROOM(1) EQUALS PASSAGE1(CURRENT-ROOM) OR
                            PASSAGE2(CURRENT-ROOM) OR
                            PASSAGE3(CURRENT-ROOM) THEN
            IF WUMPUS(AROOM(1)) EQUALS 1 THEN
                DISPLAY "YOU KILLED THE WUMPUS"
                MOVE 1 TO GAMEOVER
            END-IF
          ELSE
             DISPLAY "CRUNCH THE ARROW HITS A CAVE WALL AND SNAPS!1"
             NEXT SENTENCE
          END-IF

          IF AROOM(2) EQUALS PASSAGE1(AROOM(1)) OR
                             PASSAGE2(AROOM(1)) OR
                             PASSAGE3(AROOM(1)) THEN
            IF WUMPUS(AROOM(2)) EQUALS 1 THEN
                 DISPLAY "YOU KILLED THE WUMPUS"
                 MOVE 1 TO GAMEOVER
            END-IF
          ELSE
             DISPLAY "CRUNCH THE ARROW HITS A CAVE WALL AND SNAPS2"
             NEXT SENTENCE
          END-IF

          IF AROOM(3) EQUALS PASSAGE1(AROOM(2)) OR
                              PASSAGE2(AROOM(2)) OR
                              PASSAGE3(AROOM(2)) THEN
             IF WUMPUS(AROOM(3)) EQUALS 1 THEN
                DISPLAY "YOU KILLED THE WUMPUS"
                MOVE 1 TO GAMEOVER
            END-IF
          ELSE
                DISPLAY "CRUNCH THE ARROW HITS A CAVE WALL AND SNAPS3"
                NEXT SENTENCE
          END-IF

          IF AROOM(4) EQUALS PASSAGE1(AROOM(3)) OR
                              PASSAGE2(AROOM(3)) OR
                              PASSAGE3(AROOM(3)) THEN
            IF WUMPUS(AROOM(4)) EQUALS 1 THEN
                DISPLAY "YOU KILLED THE WUMPUS"
                MOVE 1 TO GAMEOVER
            END-IF
          ELSE
            DISPLAY "CRUNCH THE ARROW HITS A CAVE WALL AND SNAPS4"
            NEXT SENTENCE
          END-IF

          IF AROOM(5) EQUALS PASSAGE1(AROOM(4)) OR
                                PASSAGE2(AROOM(4)) OR
                                PASSAGE3(AROOM(4)) THEN
            IF WUMPUS(AROOM(5)) EQUALS 1 THEN
                  DISPLAY "YOU KILLED THE WUMPUS"
                  MOVE 1 TO GAMEOVER
            END-IF
          ELSE
            DISPLAY "CRUNCH THE ARROW HITS A CAVE WALL AND SNAPS5"
            NEXT SENTENCE
          END-IF.

          PERFORM P-300-GEN-RANDNO
          IF NUM < 15 THEN
            DISPLAY "YOU WOKE THE WUMPUS"
            MOVE ZERO TO WUMPUS(WUMPUS-ROOM)
            PERFORM P-300-GEN-RANDNO
            MOVE 1 TO WUMPUS(NUM)
            MOVE NUM TO WUMPUS-ROOM
          END-IF

          IF ARROWS EQUAL ZERO THEN
            DISPLAY "UH OH OUT OF ARROWS"
            MOVE 1 TO GAMEOVER
          END-IF.

       P-799-EXIT.
        EXIT.
