       IDENTIFICATION DIVISION.
       PROGRAM-ID. WUMPUS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FLAGS.
          05 FIRST-RAND                         PIC 9(1) VALUE 0.
          05 GAMEOVER                           PIC 9(1) VALUE 0.
          05 ARROW-HIT                          PIC 9(1) VALUE 0.
       01 ARROW-RM                              PIC 9(1) VALUE 1.
       01 ARROW-ROOM-COUNT                      PIC 9(1) VALUE 1.
       01 CURRENT-TIME                          PIC 9(18) VALUE 0.
       01 NUM                                   PIC 9(2) VALUE 5.
       01 SEED                                  PIC 9(2) VALUE 0.
       01 USER-INPUT                            PIC X(20) VALUE SPACES.

       01 ADVENTURER.
          05 ARROWS                             PIC 9(1) VALUE 5.
          05 CURRENT-ROOM                       PIC 9(2) VALUE 1.
       01 ROOMS.
          05 WUMPUS-ROOM                        PIC 9(2)  VALUE 0.
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
       01 MESSAGES.
          05 ARROW-MESAGE                       PIC X(47) VALUE
                       "CRUNCH THE ARROW HITS A CAVE WALL AND SNAPS!".
          05 KILLED-WUMPUS-MESSAGE              PIC X(21) VALUE
                       "YOU KILLED THE WUMPUS".


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
       MOVE NUM TO WUMPUS-ROOM

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
            COMPUTE NUM = FUNCTION RANDOM * 20 + 1
            MOVE 1 TO FIRST-RAND
          ELSE
            COMPUTE NUM = FUNCTION RANDOM * 20 + 1
          END-IF.
          DISPLAY NUM.
       P-300-EXIT.
          EXIT.

       P-400-GAME-LOOP.

        IF BAT(CURRENT-ROOM) EQUAL 1 THEN
            PERFORM P-900-DISPLAY-BATS
            PERFORM P-300-GEN-RANDNO
            MOVE NUM TO CURRENT-ROOM
        END-IF

        IF WUMPUS(CURRENT-ROOM) EQUAL 1 THEN
            PERFORM P-800-DISPLAY-WUMPUS
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
                      COMPUTE ARROWS = ARROWS - 1
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
                  WHEN "CHEA"
                        DISPLAY WUMPUS-ROOM
                  WHEN OTHER
                      DISPLAY "IM SORRY I DONT UNDERSTAND"
                      DISPLAY "TRY GO 1,2 OR 3"

         END-EVALUATE.

       P-699-EXIT.
        EXIT.

       P-700-SHOOT-ARROW.
           MOVE CURRENT-ROOM TO ARROW-RM

          PERFORM UNTIL ARROW-HIT EQUAL 1
             DISPLAY ARROW-RM
             IF AROOM(ARROW-ROOM-COUNT) EQUALS PASSAGE1(ARROW-RM) OR
                                PASSAGE2(ARROW-RM) OR
                                PASSAGE3(ARROW-RM) THEN
                IF WUMPUS(AROOM(ARROW-ROOM-COUNT)) EQUALS 1 THEN
                    DISPLAY KILLED-WUMPUS-MESSAGE
                    MOVE 1 TO GAMEOVER
                    MOVE 1 TO ARROW-HIT
                END-IF
                MOVE AROOM(ARROW-ROOM-COUNT) TO ARROW-RM
                COMPUTE ARROW-ROOM-COUNT = ARROW-ROOM-COUNT + 1
             ELSE
                 DISPLAY ARROW-MESAGE
                 MOVE 1 TO ARROW-HIT
                 PERFORM P-300-GEN-RANDNO
                 IF NUM < 15 THEN
                   DISPLAY "YOU WOKE THE WUMPUS"
                   MOVE 0 TO WUMPUS (WUMPUS-ROOM)
                   PERFORM P-300-GEN-RANDNO
                   MOVE 1 TO WUMPUS (NUM)
                   MOVE NUM TO WUMPUS-ROOM
                 END-IF
              END-IF
          END-PERFORM
          INITIALIZE ARROW-HIT

          IF ARROWS EQUAL ZERO THEN
            DISPLAY "UH OH OUT OF ARROWS"
            MOVE 1 TO GAMEOVER
          END-IF.

       P-799-EXIT.
        EXIT.

       P-800-DISPLAY-WUMPUS.



            DISPLAY"                          ####                     "
            DISPLAY"                         #    #                    "
            DISPLAY"                        #  ..  #                   "
            DISPLAY"                       #  .  .  #                  "
            DISPLAY"                      #  . 00 .  #                 "
            DISPLAY"                      #  . 00 .  #                 "
            DISPLAY"                  ####    .  .    ####             "
            DISPLAY"                 #         ..         #            "
            DISPLAY"                #                      #           "
            DISPLAY"               #         vvvvvv         #          "
            DISPLAY"              #   #     v      v     #   #         "
            DISPLAY"              #  # #   >        <   # #  #         "
            DISPLAY"              #  #  #   ^      ^   #  #  #         "
            DISPLAY"              #  #   #   ^^^^^^   #   #  #         "
            DISPLAY"              #  #    #          #    #  #         "
            DISPLAY"              #  #     #        #     #  #         "
            DISPLAY"              #  #      #      #      #  #         "
            DISPLAY"              #  #       ##  ##       #  #         "
            DISPLAY"              #  #         ##         #  #         "
            DISPLAY"             (----)                  (----)        "
            DISPLAY"            (------)                (------)       "
            DISPLAY"             YOU ARE DEVOURED BY THE WUMPUS       ".


       P-899-EXIT.
         EXIT.

       P-900-DISPLAY-BATS.

            DISPLAY"                                                   "
            DISPLAY"             ^ w ^                                 "
            DISPLAY"            / \#/ \                  ^ w ^         "
            DISPLAY"              /v\                   / \#/ \        "
            DISPLAY"                       ^ w ^          /v\          "
            DISPLAY"                      / \#/ \                      "
            DISPLAY"                        /v\                        "
            DISPLAY"                                                   "
            DISPLAY"                            ^ w ^                  "
            DISPLAY"                           / \#/ \                 "
            DISPLAY"            ^ w ^            /v\                   "
            DISPLAY"           / \#/ \                                 "
            DISPLAY"             /v\                                   "
            DISPLAY"             OH NO GRABBED BY SUPER BATS!!        ".


       P-999-EXIT.
           EXIT.
