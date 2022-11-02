       IDENTIFICATION DIVISION.
       PROGRAM-ID. 3DRENDER.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.     
       SPECIAL-NAMES.             
       DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  CT-CONSTANTS.
           05  CT-TURNS         PIC 9(08)    VALUE 1.
           05  CT-FILLER-LINE   PIC X(100)   VALUE ALL '-'.
           05  CT-WAIT          PIC 9V999    VALUE 1,000.
           05  CT-CANVAS-WIDTH  PIC 9(03)    VALUE 100.
           05  CT-CANVAS-HEIGHT PIC 9(03)    VALUE 40.
           05  CT-SCREEN-HEIGHT PIC 9(03)    VALUE 41.
      *    CT-SCREEN MUST ALWAYS BE ONE HIGHER THAN CT-CANVAS!!!
           05  CT-CHANCE-OF-INITIAL-CELL
                                PIC 9V999    VALUE 0,333.
           05  CT-VECTOR   PIC X             VALUE '.'.             
           05  CT-EMPTY    PIC X             VALUE '.'.

       01  WS-VECTOR            OCCURS 100 TIMES.
           05  WS-LINE          PIC X(100)   VALUE SPACES.
           05  WS-ARRAY         OCCURS 100 TIMES.
               10  WS-CELL      PIC 9        VALUE ZEROES.
           05  WS-ARRAY2. 
               10  WS-NEIGHBORS PIC 9        VALUE ZEROES 
                                OCCURS 100 TIMES. 

       01  WS-VARIABLES.
           05  WS-TEMP         OCCURS 5     TIMES 
                               PIC S99999   VALUE ZEROES.
           05  WS-FRAC         OCCURS 5     TIMES
                               PIC 9(5)V9(5).
           05  WS-VECTORS      OCCURS 5     TIMES.
                10  WS-X       PIC S9(03)    VALUE ZEROES.
                10  WS-Y       PIC S9(03)    VALUE ZEROES.
           05  WS-COUNT        PIC 9(03)    VALUE ZEROES.
           05  WS-ANGLE        PIC 9V999    VALUE ZEROES.

       01  WS-FECHA            PIC 9(18)    VALUE ZEROES.

       SCREEN SECTION.

       01  CLEAR-SCREEN.
           05 VALUE SPACES BLANK SCREEN.

       01  GRID-SCREEN.
           05  SC-LINE         PIC X(100)    LINE WS-COUNT COL 2
                               VALUE ALL '.'.
           05  SC-LINE-COUNT   PIC ZZ9       LINE WS-COUNT COL + 1.
       
       01  BLANK-SCREEN.
           05  SC-COUNT        PIC X(100)    LINE 40 COL 1
                               VALUE ALL '0    5    1'.
           05  SC-INFO         PIC X(100)    LINE 41 COL 1.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM 10 TIMES
           PERFORM 1000-INIT  
           THRU  F-1000-INIT 
           PERFORM 2000-PROCESS
           THRU  F-2000-PROCESS
           UNTIL WS-VECTORS(3) = WS-VECTORS(2)
           PERFORM 9999-FINAL   
           THRU  F-9999-FINAL
           END-PERFORM.
       F-MAIN-PROGRAM. GOBACK.

       1000-INIT.

           MOVE FUNCTION RANDOM(FUNCTION CURRENT-DATE (1:16))
                TO WS-ANGLE.

           COMPUTE WS-X(1) = FUNCTION RANDOM(WS-ANGLE * 100) * 100 + 1.
           COMPUTE WS-Y(1) = FUNCTION RANDOM() * 40 + 1.

           COMPUTE WS-X(2) = FUNCTION RANDOM() * 100 + 1.
           COMPUTE WS-Y(2) = FUNCTION RANDOM() * 40 + 1.

           MOVE WS-VECTORS(2) TO WS-VECTORS(4).
           SUBTRACT WS-X(1) FROM WS-X(4).
           SUBTRACT WS-Y(1) FROM WS-Y(4).
           
           MOVE WS-VECTORS(1) TO WS-VECTORS(3).

           MOVE 0 TO WS-FRAC(5).

           PERFORM UNTIL WS-VECTORS(5) = WS-VECTORS(1)
                   SUBTRACT 0,001 FROM WS-FRAC(5)
                   COMPUTE WS-X(5) = WS-FRAC(5) * WS-X(4) + WS-X(1)
                   COMPUTE WS-Y(5) = WS-FRAC(5) * WS-Y(4) + WS-Y(1)
           END-PERFORM.

           MOVE ZEROES TO WS-VECTORS(5).
           MOVE 'A' TO CT-VECTOR.
           MOVE CT-VECTOR TO WS-LINE(WS-Y(1))(WS-X(1):1).
           MOVE 'B' TO CT-VECTOR.
           MOVE CT-VECTOR TO WS-LINE(WS-Y(2))(WS-X(2):1).
      *    DISPLAY WS-X(1) " " WS-Y(1) " | " WS-X(2) " " WS-Y(2) " | ".
      *            WS-X(3) " " WS-Y(3) " | " WS-X(4) " " WS-Y(4) " | "
      *            WS-X(5) " " WS-Y(5) " | " WS-FRAC(5).
       F-1000-INIT. EXIT.

       2000-PROCESS.
           PERFORM 3000-CALC-NEXT-STEP
           THRU  F-3000-CALC-NEXT-STEP.
           IF WS-LINE(WS-Y(3))(WS-X(3):1) = ' '
              MOVE CT-VECTOR TO WS-LINE(WS-Y(3))(WS-X(3):1).

      *            MOVE WS-LINE(WS-Y(1)) TO SC-LINE
      *            MOVE WS-Y(1) TO WS-COUNT
      *            DISPLAY GRID-SCREEN

      *            MOVE WS-LINE(WS-Y(2)) TO SC-LINE
      *            MOVE WS-Y(2) TO WS-COUNT
      *            DISPLAY GRID-SCREEN

      *            MOVE WS-LINE(WS-Y(3)) TO SC-LINE
      *            MOVE WS-Y(3) TO WS-COUNT
      *            DISPLAY GRID-SCREEN

           ADD 0,001 TO WS-FRAC(5).

           MOVE WS-VECTOR(3) TO SC-INFO(30:6).
           MOVE WS-VECTOR(5) TO SC-INFO(40:6).

           DISPLAY BLANK-SCREEN.

      *    MOVE SPACES TO SC-INFO.

      *    CALL 'CBL_OC_NANOSLEEP' USING 100000000.
      *    DISPLAY CLEAR-SCREEN.
       F-2000-PROCESS. EXIT.

       3000-CALC-NEXT-STEP.
      * Next idea: Create a line with both points and calculating all
      * possible combinations between both points.
      * 
      * Formula for a line given two points: a * (Vec2 - Vec1) + Vec1 
      * Vec 4 is Vec2 - Vec1

           MOVE     WS-VECTORS(4) TO WS-VECTORS(5).
           MULTIPLY WS-FRAC(5) BY WS-X(5).
           ADD      WS-X(1)    TO WS-X(5).
           MULTIPLY WS-FRAC(5) BY WS-Y(5).
           ADD      WS-Y(1)    TO WS-Y(5).

           IF WS-X(3) = WS-X(5) AND CT-VECTOR NOT = '|'
                   MOVE '|' TO CT-VECTOR
           ELSE IF WS-Y(3) = WS-Y(5) AND CT-VECTOR NOT = '-'
                   MOVE '-' TO CT-VECTOR
           ELSE IF ((WS-Y(3) > WS-Y(5) AND WS-X(3) > WS-X(5)) OR
                   (WS-Y(3) < WS-Y(5) AND WS-X(3) < WS-X(5))) AND
                   CT-VECTOR = '/'
                   MOVE '/' TO CT-VECTOR
           ELSE    MOVE '\' TO CT-VECTOR.

      *    DISPLAY WS-VECTORS(3) WS-VECTORS(5).
           MOVE    WS-VECTORS(5) TO WS-VECTORS(3).

       F-3000-CALC-NEXT-STEP. EXIT.


       9999-FINAL.
           PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL WS-COUNT > 40
                   MOVE 40 TO WS-TEMP(1)
                   SUBTRACT WS-COUNT FROM WS-TEMP(1)
                   MOVE WS-LINE(WS-TEMP(1)) TO SC-LINE
                   MOVE WS-TEMP(1)          TO SC-LINE-COUNT
                   DISPLAY GRID-SCREEN
      *            ACCEPT BLANK-SCREEN TIMEOUT CT-WAIT
           END-PERFORM.           
       F-9999-FINAL. EXIT.
