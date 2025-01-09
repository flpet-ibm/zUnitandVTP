       ID DIVISION.
       PROGRAM-ID. CPRMAIN.
      ***
      *
      *    (C) 2019 IBM FLEMMING PETERSEN
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-CPR         PIC X(10).
       01  WS-AGE         PIC S9(4) BINARY value 0.
       01  WS-AGE-DISP    PIC ZZZZ9 DISPLAY.
       01  WS-AGE2        PIC S9(4) BINARY.
       01 WS-GENDER       PIC X.

       01  WS-RC          PIC X VALUE '0'.
       01  WS-MODULE      PIC X(8) VALUE 'CPRCHECD'.


       PROCEDURE DIVISION.
      *
       MAIN SECTION.
       MAIN1.
           MOVE '1234X27890' TO WS-CPR.
           CALL WS-MODULE USING  WS-CPR WS-AGE WS-GENDER WS-RC.
           PERFORM DISPLAY-RESULTS.

           MOVE '1234567890' TO WS-CPR.
           CALL WS-MODULE USING  WS-CPR WS-AGE WS-GENDER WS-RC.
           PERFORM DISPLAY-RESULTS.

           MOVE '2902080890' TO WS-CPR.
           CALL WS-MODULE USING  WS-CPR WS-AGE WS-GENDER WS-RC.
           PERFORM DISPLAY-RESULTS.

           MOVE '2902090890' TO WS-CPR.
           CALL WS-MODULE USING  WS-CPR WS-AGE WS-GENDER WS-RC.
           PERFORM DISPLAY-RESULTS.

           MOVE '2802090890' TO WS-CPR.
           CALL WS-MODULE USING  WS-CPR WS-AGE WS-GENDER WS-RC.
           PERFORM DISPLAY-RESULTS.

           MOVE '3113180123' TO WS-CPR.
           CALL WS-MODULE USING  WS-CPR WS-AGE WS-GENDER WS-RC.
           PERFORM DISPLAY-RESULTS.

           MOVE '2201680789' TO WS-CPR.
           CALL WS-MODULE USING  WS-CPR WS-AGE WS-GENDER WS-RC.
           PERFORM DISPLAY-RESULTS.

           GOBACK.
           EXIT PROGRAM.

       DISPLAY-RESULTS SECTION.
           MOVE WS-AGE TO WS-AGE-DISP.
           DISPLAY 'CALLED CPRCHECK WITH ' WS-CPR
                                   '. AGE=' WS-AGE-DISP
                                   '. GENDER=' WS-GENDER
                                   '. RC=' WS-RC.
      *    COMPUTE WS-AGE2 = WS-AGE / 10.
      *    MOVE WS-AGE2 TO WS-AGE-DISP.
      *    DISPLAY '  AGE DIVIDED BY 10  ' WS-AGE-DISP.

           EXIT SECTION.

       END PROGRAM CPRMAIN.