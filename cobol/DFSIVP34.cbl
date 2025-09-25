 CBL  APOST
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DFSIVP34.
      *
      ********************************************************@SCPYRT**
      *                                                               *
      *  Licensed Materials - Property of IBM                         *
      *                                                               *
      *  5635-A06                                                     *
      *       xxxxxxXxx                                               *
      *      Copyright IBM Corp. 1991,1998 All Rights Reserved        *
      *                                                               *
      *  US Government Users Restricted Rights - Use, duplication or  *
      *  disclosure restricted by GSA ADP Schedule contract with      *
      *  IBM Corp.                                                    *
      *                                                               *
      ********************************************************@ECPYRT**
      *
      *   APPLICATION  :  CONVERSATIONAL PROGRAM
      *   TRANSACTION  :  IVTCB
      *   PSB          :  DFSIVP34
      *   DATABASE     :  DFSIVD2
      *   INPUT:
      *         TELEPHONE DIRECTORY SYSTEM
      *         PROCESS CODE : CCCCCCCC
      *         LAST NAME    : XXXXXXXXXX
      *         FIRST NAME   : XXXXXXXXXX
      *         EXTENSION#   : N-NNN-NNNN
      *         INTERNAL ZIP : XXX/XXX
      *   CCCCCCCC = COMMAND
      *         ADD = INSERT ENTRY IN DB
      *         DELETE = DELETE ENTRY FROM DB
      *         UPDATE = UPDATE ENTRY FROM DB
      *         DISPLAY = DISPLAY ENTRY
      *         TADD = SAME AS ADD, BUT ALSO WRITE TO OPERATOR
      *         END = TERMINATE CONVERSATION
      *
      *       CHANGES:  THIS MODULE IS NEW IN IMS/ESA 3.2
      *  APAR...  ID  PREREQ.  DATE....  DESCRIPTION...................
      *  KNQ0115  01           11/17/91  ADD COBOL LANG VERSION
      *

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Generated program name variable
       77 FSP34PRO PIC X(8) VALUE 'FSP34PRO'.


      * DL/I FUNCTION CODES

       77  GET-UNIQUE       PICTURE X(4)  VALUE 'GU  '.
       77  GET-NEXT         PICTURE X(4)  VALUE 'GN  '.
       77  ISRT             PICTURE X(4)  VALUE 'ISRT'.

      * DL/I CALL STATUS CODES

       77  MESSAGE-EXIST    PIC X(2) VALUE 'CF'.
       77  NO-MORE-SEGMENT  PIC X(2) VALUE 'QD'.
       77  NO-MORE-MESSAGE  PIC X(2) VALUE 'QC'.


      * VARIABLES

       77  MODNAME PICTURE X(8) VALUE SPACES.
       77  TRAN-CODE  PICTURE X(8) VALUE 'IVTCB'.
       77  REPLY      PICTURE X(16).
       77  TEMP-ONE   PICTURE X(8) VALUE SPACES.
       77  TEMP-TWO   PICTURE X(8) VALUE SPACES.

      * DATA AREA FOR TERMINAL INPUT

       01  INPUT-MSG.
           02  IN-LL          PICTURE S9(3) COMP.
           02  IN-ZZ          PICTURE S9(3) COMP.
           02  IN-FILL        PICTURE X(4).
           02  IN-COMMAND     PICTURE X(8).
           02  TEMP-COMMAND REDEFINES IN-COMMAND.
               04  TEMP-IOCMD    PIC X(3).
               04  TEMP-FILLER   PIC X(5).
           02  IN-LAST-NAME   PICTURE X(10).
           02  IN-FIRST-NAME  PICTURE X(10).
           02  IN-EXTENSION   PICTURE X(10).
           02  IN-ZIP-CODE    PICTURE X(7).

      * DATA AREA OUTPUT

       01  OUTPUT-AREA.
           02  OUT-LL       PICTURE S9(3) COMP VALUE +93.
           02  OUT-ZZ       PICTURE S9(3) COMP VALUE +0.
           02  OUTPUT-LINE  PICTURE X(85) VALUE SPACES.
           02  OUTPUT-DATA REDEFINES OUTPUT-LINE.
               04  OUT-MESSAGE   PIC X(40).
               04  OUT-COMMAND   PIC X(8).
               04  OUT-DATA-TYPE.
                   06  OUT-LAST-NAME   PIC X(10).
                   06  OUT-FIRST-NAME  PIC X(10).
                   06  OUT-EXTENSION   PIC X(10).
                   06  OUT-ZIP-CODE    PIC X(7).
           02  OUT-SEGMENT-NO  PICTURE X(4) VALUE '0001'.

      * I/O AREA FOR DATA BASE HANDLING

       01  IOAREA.
           02  IO-LINE PICTURE X(37) VALUE SPACES.
           02  IO-DATA REDEFINES IO-LINE.
               04  IO-LAST-NAME    PIC X(10).
               04  IO-FIRST-NAME   PIC X(10).
               04  IO-EXTENSION    PIC X(10).
               04  IO-ZIP-CODE     PIC X(7).
           02  IO-FILLER       PIC X(3) VALUE SPACES.
           02  IO-COMMAND      PIC X(8) VALUE SPACES.

      * SCRATCH PAD AREA

       01  SPA.
           02  SPA-LL        PICTURE X(2).
           02  SPA-ZZ        PICTURE X(4).
           02  SPA-TRANCODE  PICTURE X(8).
           02  SPA-CALL      PICTURE X(2).
           02  SPA-COMMAND   PICTURE X(8).
           02  SPA-DATA.
               04  SPA-LAST-NAME    PIC X(10).
               04  SPA-FIRST-NAME   PIC X(10).
               04  SPA-EXTENSION    PIC X(10).
               04  SPA-ZIP-CODE     PIC X(7).
           02  FILLER        PICTURE X(19).

      * DC TEXT FOR ERROR CALL

       01 DC-TEXT.
          02  TEXT1         PIC  X(7) VALUE 'STATUS '.
          02  ERROR-STATUS  PIC  X(2).
          02  TEXT2         PIC  X(12) VALUE 'DLI  CALL = '.
          02  ERROR-CALL    PIC  X(4).


      * COUNTERS

       01 COUNTERS.
          02  SPA-CALL-NO    PIC   9(2) COMP VALUE 0.

       LINKAGE SECTION.

       01  IOPCB.
           02  LTERM-NAME   PICTURE X(8).
           02  FILLER       PICTURE X(2).
           02  TPSTATUS     PICTURE XX.
           02  FILLER       PICTURE X(20).
       01  DBPCB.
           02  DBNAME       PICTURE X(8).
           02  SEG-LEVEL-NO PICTURE X(2).
           02  DBSTATUS     PICTURE XX.
           02  FILLER       PICTURE X(20).

       PROCEDURE DIVISION USING IOPCB, DBPCB.

      * ON ENTRY IMS PASSES ADDRESSES FOR IOPCB AND DBPCB

       MAIN-RTN.
           MOVE GET-UNIQUE TO ERROR-CALL.
           CALL 'CBLTDLI' USING GET-UNIQUE, IOPCB, SPA.
           IF TPSTATUS  = '  ' OR MESSAGE-EXIST
           THEN
             CALL 'CBLTDLI' USING GET-NEXT, IOPCB, INPUT-MSG
             IF TPSTATUS = SPACES
               THEN
                   PERFORM PROCESS-INPUT
                   PERFORM TERM-ROUTINE
             ELSE IF TPSTATUS = NO-MORE-SEGMENT
                  THEN GOBACK
                  ELSE
                    MOVE GET-NEXT TO ERROR-CALL
                    PERFORM WRITE-DC-TEXT
           ELSE IF TPSTATUS = NO-MORE-MESSAGE
                THEN GOBACK
                ELSE PERFORM WRITE-DC-TEXT
           END-IF.
           GOBACK.

      * PROCEDURE PROCESS-INPUT

       PROCESS-INPUT.

           CALL FSP34PRO USING
                   INPUT-MSG,
                   OUTPUT-AREA, IOAREA, SPA, SPA-CALL-NO,
                   DC-TEXT,
                   IOPCB, DBPCB.


      * PROCEDURE TERM-ROUTINE : TERMINAL ROUTINE

       TERM-ROUTINE.
           MOVE SPACES TO MODNAME.
           PERFORM INSERT-SPA.
           IF IN-COMMAND = 'END'
             MOVE TRAN-CODE TO MODNAME.
           PERFORM INSERT-IO

           EXIT.

      * PROCEDURE INSERT-SPA : SPA INSERT FOR IOPCB REQUEST HANDLER

       INSERT-SPA.
           MOVE ISRT TO ERROR-CALL.
           MOVE IO-DATA TO SPA-DATA.
           MOVE IO-COMMAND TO SPA-COMMAND.
           ADD 1 TO SPA-CALL-NO.
           MOVE SPA-CALL-NO TO SPA-CALL.
           CALL 'CBLTDLI' USING ISRT, IOPCB, SPA.
           IF TPSTATUS NOT = SPACES
             THEN PERFORM WRITE-DC-TEXT
           END-IF.

           EXIT.

      * PROCEDURE INSERT-IO : INSERT FOR IOPCB REQUEST HANDLER

       INSERT-IO.
           MOVE ISRT TO ERROR-CALL.
           IF MODNAME EQUAL SPACES
           THEN
              CALL 'CBLTDLI' USING ISRT, IOPCB, OUTPUT-AREA
           ELSE
              CALL 'CBLTDLI' USING ISRT, IOPCB, OUTPUT-AREA, MODNAME
              MOVE SPACES TO MODNAME.
           IF TPSTATUS NOT = SPACES
             THEN PERFORM WRITE-DC-TEXT
           END-IF

           EXIT.

       WRITE-DC-TEXT.
           MOVE TPSTATUS TO ERROR-STATUS.
           DISPLAY DC-TEXT UPON CONSOLE.

           EXIT.

       END PROGRAM DFSIVP34.
