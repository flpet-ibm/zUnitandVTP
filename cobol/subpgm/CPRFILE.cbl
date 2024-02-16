       ID DIVISION.
       PROGRAM-ID. CPRFILE.
      *
      *
      *    (C) 2019 IBM FLEMMING PETERSEN
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO FILEIN
               FILE STATUS IS FILEIN-STATUS
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FILEIN RECORDING MODE F.
       01 IN-RECORD.
       COPY CPRRECOR.

       WORKING-STORAGE SECTION.

       01 FI-MARKER         PIC X     VALUE '0'.
          88 FI-EOF                   VALUE '1'.
       01 FILEIN-STATUS  PIC 99.
       01 AMOUNTIN-STATUS  PIC 99.

      *
       01 WS-CPR            PIC X(10).
       01 WS-AGE            PIC S9(4) BINARY
                                      VALUE 0.
       01 WS-AGE-FORMAT     PIC ZZZ.ZZ9,999 DISPLAY.
       01 WS-DISP-AGE       PIC ZZZ.ZZ9,999 DISPLAY.
       01 WS-GENDER         PIC X.
       01 WS-AGE2           PIC S9(4) BINARY.
       01 WS-RC             PIC X     VALUE '0'.
       01 WS-MODULE         PIC X(8)  VALUE 'CPRCHECK'.

       PROCEDURE DIVISION.
      *
       MAIN SECTION.
       MAIN1.

           OPEN INPUT FILEIN.
           IF FILEIN-STATUS NOT = 0 THEN
              DISPLAY 'CPRFILE FILE STATUS AT OPEN FILEIN'
                      FILEIN-STATUS
              MOVE 16 to RETURN-CODE
              STOP RUN
           END-IF.

           READ FILEIN
                AT END SET FI-EOF TO TRUE
           END-READ.

           PERFORM TEST BEFORE until FI-EOF
              MOVE IN-FDATO TO WS-CPR(1:6)
              MOVE IN-CHECKDIGIT TO WS-CPR(7:4)
              DISPLAY 'CPRFILE. Read from file: ' WS-CPR
      *       CALL WS-MODULE USING  WS-CPR WS-AGE WS-GENDER
              CALL 'CPRCHECK' USING  WS-CPR WS-AGE WS-GENDER
                                     WS-RC
      *
              PERFORM DISPLAY-RESULTS
              READ FILEIN
                   AT END SET FI-EOF TO TRUE
              END-READ
           END-PERFORM.

           CLOSE FILEIN .
           GOBACK.


       DISPLAY-RESULTS SECTION.
           MOVE WS-AGE TO WS-AGE-FORMAT.
           DISPLAY 'CALLED CPRCHECK WITH ' WS-CPR
                                   '. AGE=' WS-AGE-FORMAT
                                   '. RC=' WS-RC.
           COMPUTE WS-AGE2 = WS-AGE / 10.
           MOVE WS-AGE2 TO WS-AGE-FORMAT.
           DISPLAY '  AGE DIVIDED BY 10  ' WS-AGE-FORMAT.

           EXIT SECTION.

       END PROGRAM CPRFILE.