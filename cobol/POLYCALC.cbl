       IDENTIFICATION DIVISION.
       PROGRAM-ID. POLYCALC INITIAL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VARIABLES.
           05  WS-BASE-RATE         PIC 9(3)V99 VALUE 100.00.
           05  WS-AGE-FACTOR        PIC V999    VALUE .02.
           05  WS-CAR-FACTOR        PIC V9999   VALUE .001.
           05  WS-COVERAGE-FACTOR   PIC V99     VALUE .0.

       LINKAGE SECTION.
       01  LINKAGE-DATA.
           COPY POLYDATA.

       PROCEDURE DIVISION USING LINKAGE-DATA.

       MAIN-LOGIC.
           MOVE ZEROES TO CALCULATED-PREMIUM

           EVALUATE TRUE
               WHEN DRIVER-AGE < 25
                   COMPUTE CALCULATED-PREMIUM = WS-BASE-RATE *
                   (1 + (25 - DRIVER-AGE) * WS-AGE-FACTOR)
               WHEN OTHER
                   COMPUTE CALCULATED-PREMIUM = WS-BASE-RATE
           END-EVALUATE

           COMPUTE CALCULATED-PREMIUM = CALCULATED-PREMIUM +
             (CAR-VALUE * WS-CAR-FACTOR)

           EVALUATE COVERAGE-LEVEL
               WHEN 'B' *> Basic
                   MOVE 0.20 TO WS-COVERAGE-FACTOR
               WHEN 'S' *> Standard
                   MOVE 0.40 TO WS-COVERAGE-FACTOR
               WHEN 'P' *> Premium
                   MOVE 0.60 TO WS-COVERAGE-FACTOR
               WHEN OTHER 
                   DISPLAY 'ERROR: INVALID COVERAGE LEVEL'
           END-EVALUATE

           COMPUTE CALCULATED-PREMIUM = CALCULATED-PREMIUM *
             (1 + WS-COVERAGE-FACTOR)

           MOVE '00' TO RET-CODE

           GOBACK.
       END PROGRAM POLYCALC.