 CBL  APOST                                                             00010000
010010 IDENTIFICATION DIVISION.                                         00020000
010020 PROGRAM-ID.  DFSSAM02.                                           00030000
010023*                                                                 00040000
010024********************************************************@SCPYRT** 00050000
010025*                                                               * 00060000
010026*  Licensed Materials - Property of IBM                         * 00070000
010027*                                                               * 00080000
010028*  5635-A06                                                     * 00090000
010029*                                                               * 00100000
010030*      Copyright IBM Corp. 1974,1998 All Rights Reserved.       * 00110000
010031*                                                               * 00120000
010032*  US Government Users Restricted Rights - Use, duplication or  * 00130000
010033*  disclosure restricted by GSA ADP Schedule Contract with      * 00140000
010034*  IBM Corp.                                                    * 00150000
010036********************************************************@ECPYRT** 00160000
010038*                                                                 00170000
010040*             IMS/VS DEMONSTRATION OF TERMINAL ABILITY            00180000
010050*             TO DISPLAY A PART NUMBER SEGMENT AND                00190000
010060*             SOME OF ITS STANDARD INFORMATION WHICH              00200000
010070*             ARE ON AN IMS ISAM/OSAM INVENTORY DATABASE.         00210000
010071*             THE TRANSACTION CODES WHICH ACTIVATE THE            00220000
010072*             PROGRAM ARE DSPPN AND PART.                         00230000
010080 ENVIRONMENT DIVISION.                                            00240000
010090 CONFIGURATION SECTION.                                           00250000
010100 SOURCE-COMPUTER. IBM-370.                                        00260000
010110 OBJECT-COMPUTER. IBM-370.                                        00270000
010120 DATA DIVISION.                                                   00280000
010130 WORKING-STORAGE SECTION.                                         00280001
       77  INPANAL     pic x(8) value 'INPANAL'.                        00280002
       77  PNEDIT      pic x(8) value 'PNEDIT'.                         00280003
       77  GET-UNIQUE  PICTURE XXXX VALUE 'GU  '.                       00300000
010150 77  GET-NEXT    PICTURE XXXX VALUE 'GN  '.                       00310000
010160 77  IN-SERT     PICTURE XXXX VALUE 'ISRT'.                       00320000
010170 77  SEG-NOT-FOUND PICTURE XX VALUE 'GE'.                         00330000
010180 77  MSG-SEG-CNT PICTURE S99 VALUE +1 COMPUTATIONAL.              00340000
010190 77  NO-MSG-THERE  PICTURE XX  VALUE 'QX'.                        00350000
010200 77  ERROR-SWITCH  PICTURE X   VALUE ' '.                         00360000
020050 01  PARAMETER-TABLE.                                             00370000
020060     02  FILLER  PICTURE S99 VALUE +15 COMPUTATIONAL.             00380000
020070     02  FILLER  PICTURE XX  VALUE 'L '.                          00390000
020080     02  FILLER  PICTURE S99 VALUE ZERO COMPUTATIONAL.            00400000
020090 01  EDITED-MSG.                                                  00410000
020100     02  TRANS-CODE  PICTURE X(8) VALUE SPACES.                   00420000
020110     02  PART-NUMBER PICTURE X(15) VALUE SPACES.                  00430000
020120 01  LINE-INPUT.                                                  00440000
020130     02  INPUT-COUNT PICTURE S99 COMPUTATIONAL.                   00450000
020140     02  FILLER      PICTURE S99 COMPUTATIONAL.                   00460000
020150     02  INPUT-TEXT  PICTURE X(132) VALUE SPACES.                 00470000
020160 01  LINE-OUTPUT.                                                 00480000
020170     02  OUTPUT-COUNT PICTURE S99 COMPUTATIONAL.                  00490000
020180     02  FILLER       PICTURE S99 COMPUTATIONAL VALUE ZERO.       00500000
020185     02  CARR-RETURN PICTURE X VALUE X'15'.                       00510000
020190     02  OUTPUT-TEXT  PICTURE X(132) VALUE SPACES.                00520000
020200 01  PART-REJECTED-MSG.                                           00530000
020210     02  FILLER PICTURE X(17) VALUE 'PART REJECT CODE '.          00540000
020220     02  REJECT-CODE PICTURE X.                                   00550000
020230     02  CR-6  PICTURE X.                                         00560000
030010 01  PART-NOT-FOUND-MSG.                                          00570000
030020     02  FILLER PICTURE X(12) VALUE 'PART NUMBER '.               00580000
030030     02  FILL-PART-1 PICTURE X(15).                               00590000
030040     02  FILLER PICTURE X(16) VALUE ' NOT ON DATABASE'.           00600000
030050     02  CR-1   PICTURE X.                                        00610000
030060 01  FIRST-LINE.                                                  00620000
030070     02  FILLER PICTURE X(5) VALUE 'PART='.                       00630000
030080     02  FILL-PART-2 PICTURE X(15).                               00640000
030090     02  FILLER PICTURE X(7) VALUE '  DESC='.                     00650000
030100     02  FILL-DESCR PICTURE X(20).                                00660000
030110     02  FILLER PICTURE X(12) VALUE '  PROC CODE='.               00670000
030120     02  FILL-PROC-CODE PICTURE XX VALUE SPACES.                  00680000
030130     02  CR-2  PICTURE X.                                         00690000
030140 01  SECOND-LINE.                                                 00700000
030150     02  FILLER  PICTURE X(9) VALUE 'INV CODE='.                  00710000
030160     02  FILL-INV-CODE PICTURE X VALUE SPACE.                     00720000
030170     02  FILLER  PICTURE X(12) VALUE '  MAKE DEPT='.              00730000
030180     02  FILL-MAKE-1 PICTURE XX VALUE SPACES.                     00740000
030190     02  FILLER  PICTURE X  VALUE '-'.                            00750000
030200     02  FILL-MAKE-2 PICTURE XX VALUE SPACES.                     00760000
040010     02  FILLER  PICTURE X(15) VALUE '  PLAN REV NUM='.           00770000
040020     02  FILL-PLAN-REV-NUM PICTURE XX VALUE SPACES.               00780000
040030     02  FILLER  PICTURE X(12) VALUE '  MAKE TIME='.              00790000
040040     02  FILL-MAKE-TIME  PICTURE ZZZ.                             00800000
040050     02  FILLER  PICTURE X(12) VALUE '  COMM CODE='.              00810000
040060     02  FILL-COMM-CODE PICTURE XXXX VALUE SPACES.                00820000
040070     02  CR-3  PICTURE X.                                         00830000
040080 01  NO-STANINFO-MSG.                                             00840000
040090     02  FILLER PICTURE X(29)                                     00850000
040100             VALUE 'THERE IS NO STANINFO SEGMENT.'.               00860000
040110     02  CR-4  PICTURE X.                                         00870000
040120 01  ROOT-FORMAT.                                                 00880000
040130     02  FILLER  PICTURE X(26).                                   00890000
040140     02  DESCRIPTION PICTURE X(20).                               00900000
040150     02  FILLER  PICTURE XXXX.                                    00910000
040160 01  PART-NO-EDIT-TABLE.                                          00920000
040170     02  PART-NO-ENTRY  PICTURE X(17).                            00930000
040180     02  FILLER         PICTURE XXXX.                             00940000
040190     02  PART-NO-REJECT PICTURE X.                                00950000
040200 01  STANINFO-FORMAT-DEFINITION PICTURE X(85) VALUE SPACES.       00960000
050010 01  STANINFO-FORMAT REDEFINES STANINFO-FORMAT-DEFINITION.        00970000
050020     02  FILLER  PICTURE X(18).                                   00980000
050030     02  PROCUREMENT-CODE  PICTURE XX.                            00990000
050040     02  INVENTORY-CODE  PICTURE X.                               01000000
050050     02  PLANNING-REVISION-NUMBER  PICTURE XX.                    01010000
050060     02  FILLER  PICTURE X(24).                                   01020000
050070     02  MAKE-DEPT  PICTURE XX.                                   01030000
050080     02  MAKE-COST-CTR  PICTURE XX.                               01040000
050090     02  FILLER  PICTURE XX.                                      01050000
050100     02  COMMODITY-CODE  PICTURE XXXX.                            01060000
050110     02  FILLER  PICTURE XXXX.                                    01070000
050120     02  MAKE-SPAN  PICTURE S999.                                 01080000
050130     02  FILLER  PICTURE X(21).                                   01090000
050140 01  ROOT-SSA.                                                    01100000
050150     02  FILLER  PICTURE X(21) VALUE 'PARTROOT(PARTKEY  =02'.     01110000
050160     02  FILL-PART-3 PICTURE X(15).                               01120000
050170     02  FILLER  PICTURE X VALUE ')'.                             01130000
050180 01  STANINFO-SSA PICTURE X(22)  VALUE                            01140000
050190                'STANINFO(STANKEY  =02)'.                         01150000
060010 01  STATUS-CODE-MSG.                                             01160000
060020     02  FILLER PICTURE X(23) VALUE 'UNRESOLVED STATUS CODE '.    01170000
060030     02  FILL-STATUS  PICTURE XX.                                 01180000
060040     02  FILLER PICTURE X(4) VALUE ' ON '.                        01190000
060050     02  FILL-FUNCTION PICTURE X(4).                              01200000
060060     02  CR-5  PICTURE X.                                         01210000
060110 LINKAGE SECTION.                                                 01220000
060120 01  TERM-NAL.                                                    01230000
060130     02  LTERM-NAME  PICTURE X(8).                                01240000
060140     02  FILLER      PICTURE XX.                                  01250000
060150     02  TERM-STATUS PICTURE XX.                                  01260000
060160     02  TERM-PREFIX.                                             01270000
060170         03  FILLER  PICTURE X.                                   01280000
060180         03  JULIAN-DATE PICTURE S9(5) COMPUTATIONAL-3.           01290000
060190         03  TIME-O-DAY  PICTURE S9(7) COMPUTATIONAL-3.           01300000
060200         03  FILLER  PICTURE XXXX.                                01310000
070010 01  DATABASE.                                                    01320000
070020     02  DBASE-NAME  PICTURE X(8).                                01330000
070030     02  SEGMENT-INDR PICTURE XX.                                 01340000
070040     02  DBASE-STATUS PICTURE XX.                                 01350000
070050     02  PROC-OPTIONS PICTURE XXXX.                               01360000
070060     02  DLI-RESERVED PICTURE XXXX.                               01370000
070070     02  SEG-FEEDBACK PICTURE X(8).                               01380000
070080     02  KEY-FEEDBACK-LENGTH  PICTURE XXXX.                       01390000
070090     02  NO-OF-SENSEG-TYPES   PICTURE XXXX.                       01400000
070100     02  KEY-FEEDBACK.                                            01410000
070110         03  ROOT-KEY.                                            01420000
070120             04  FILLER  PICTURE XX.                              01430000
070130             04  PARTNUM PICTURE X(15).                           01440000
070140         03  STANINFO-KEY PICTURE XX.                             01450000
070150     02  PART-SEGNAME  PICTURE X(8).                              01460000
070160     02  STAN-SEGNAME  PICTURE X(8).                              01470000
080010 PROCEDURE DIVISION.                                              01480000
080020 START-OUT.                                                       01490000
080040      ENTRY 'DLITCBL' USING TERM-NAL, DATABASE.                   01500000
080100      CALL 'CBLTDLI' USING GET-UNIQUE, TERM-NAL, LINE-INPUT.      01510000
080120     IF TERM-STATUS = '  ' GO TO TERM-OK.                         01520000
080130     IF TERM-STATUS NOT = NO-MSG-THERE GO TO ERROR-HANDLER.       01530000
080140 EXIT-RTN.                                                        01540000
080160     GOBACK.                                                      01550000
080180 TERM-OK.                                                         01560000
080200      CALL INPANAL USING PARAMETER-TABLE, LINE-INPUT,             01570000
090010                 EDITED-MSG, MSG-SEG-CNT.                         01580000
090030     MOVE 0 TO MSG-SEG-CNT. MOVE SPACES TO INPUT-TEXT.            01590000
090050      CALL INPANAL USING PARAMETER-TABLE, LINE-INPUT,             01600000
090060                 EDITED-MSG, MSG-SEG-CNT.                         01610000
090080     MOVE 1 TO MSG-SEG-CNT.                                       01620000
090090     MOVE PART-NUMBER TO PART-NO-ENTRY.                           01630000
090110      CALL PNEDIT USING PART-NO-EDIT-TABLE.                       01640000
090130     IF  PART-NO-REJECT NOT = ' ' GO TO PART-REJECTED-RTN.        01650000
090140     MOVE PART-NO-ENTRY TO FILL-PART-3, PART-NUMBER.              01660000
090160      CALL 'CBLTDLI' USING GET-UNIQUE, DATABASE, ROOT-FORMAT,     01670000
090170                             ROOT-SSA.                            01680000
090190     IF  DBASE-STATUS = '  ' GO TO PROCESS-FIRST-LINE.            01690000
090200     IF  DBASE-STATUS NOT = SEG-NOT-FOUND GO TO DBASE-ERROR.      01700000
100010     MOVE PART-NUMBER TO FILL-PART-1.                             01710000
100020     MOVE CARR-RETURN TO CR-1.                                    01720000
100030     MOVE PART-NOT-FOUND-MSG TO OUTPUT-TEXT.                      01730000
100040     MOVE 49 TO OUTPUT-COUNT.                                     01740000
100050 TERM-OUT-RTN.                                                    01750000
100060     MOVE IN-SERT TO FILL-FUNCTION.                               01760000
100070     CALL 'CBLTDLI' USING IN-SERT, TERM-NAL, LINE-OUTPUT.         01770000
100085 EXIT-FROM-TERM-OUT. EXIT.                                        01780000
100086 PTCH-1.                                                          01790000
100090     IF TERM-STATUS = '  ' GO TO EXIT-RTN.                        01800000
100100 ERROR-HANDLER.  MOVE TERM-STATUS TO FILL-STATUS.                 01810000
100110     IF ERROR-SWITCH NOT = ' ' DISPLAY STATUS-CODE-MSG UPON       01820000
100120          CONSOLE, GO TO EXIT-RTN.                                01830000
100130     MOVE 'E' TO ERROR-SWITCH.                                    01840000
100135 COMMON-ERROR.                                                    01850000
100140     MOVE CARR-RETURN TO CR-5.                                    01860000
100150     MOVE STATUS-CODE-MSG TO OUTPUT-TEXT.                         01870000
100160     MOVE 39 TO OUTPUT-COUNT.                                     01880000
100165 EXIT-FROM-ERROR-HANDLER. EXIT.                                   01890000
100166 PTCH-2.                                                          01900000
100170     GO TO TERM-OUT-RTN.                                          01910000
100180 DBASE-ERROR.                                                     01920000
100190     MOVE DBASE-STATUS TO FILL-STATUS.                            01930000
100200     GO TO COMMON-ERROR.                                          01940000
110010 PART-REJECTED-RTN.                                               01950000
110020     MOVE PART-NO-REJECT TO REJECT-CODE.                          01960000
110030     MOVE CARR-RETURN TO CR-6.                                    01970000
110040     MOVE PART-REJECTED-MSG TO OUTPUT-TEXT.                       01980000
110050     MOVE 24 TO OUTPUT-COUNT.                                     01990000
110060     GO TO TERM-OUT-RTN.                                          02000000
110070 PROCESS-FIRST-LINE.                                              02010000
110080     MOVE PART-NUMBER TO FILL-PART-2.                             02020000
110090     MOVE DESCRIPTION TO FILL-DESCR.                              02030000
110100     MOVE GET-NEXT TO FILL-FUNCTION.                              02040000
110120      CALL 'CBLTDLI' USING GET-NEXT, DATABASE, STANINFO-FORMAT,   02050000
110130                             ROOT-SSA, STANINFO-SSA.              02060000
110150     IF DBASE-STATUS = '  ' MOVE PROCUREMENT-CODE TO              02070000
110160         FILL-PROC-CODE, GO TO PROCESS-SECOND-LINE.               02080000
110170     IF TERM-STATUS NOT = '  ' GO TO ERROR-HANDLER.               02090000
110180     IF DBASE-STATUS = SEG-NOT-FOUND PERFORM PROCESS-SECOND-LINE. 02100000
110190     IF TERM-STATUS NOT = '  ' PERFORM ERROR-HANDLER,             02110000
110200         GO TO EXIT-RTN.                                          02120000
120010 PROCESS-SECOND-LINE.                                             02130000
120020     MOVE CARR-RETURN TO CR-2.                                    02140000
120030     MOVE FIRST-LINE TO OUTPUT-TEXT.                              02150000
120040     MOVE 67 TO OUTPUT-COUNT.                                     02160000
120050     PERFORM TERM-OUT-RTN.                                        02170000
120060 EXIT-FROM-PROCESS-2ND-LINE. EXIT.                                02180000
120061 PTCH-3.                                                          02190000
120070     MOVE INVENTORY-CODE TO FILL-INV-CODE.                        02200000
120080     MOVE MAKE-DEPT TO FILL-MAKE-1.                               02210000
120090     MOVE MAKE-COST-CTR TO FILL-MAKE-2.                           02220000
120100     MOVE PLANNING-REVISION-NUMBER TO FILL-PLAN-REV-NUM.          02230000
120110     MOVE MAKE-SPAN TO FILL-MAKE-TIME.                            02240000
120120     MOVE COMMODITY-CODE TO FILL-COMM-CODE.                       02250000
120130     MOVE CARR-RETURN TO CR-3.                                    02260000
120140     MOVE SECOND-LINE TO OUTPUT-TEXT.                             02270000
120150     MOVE 81 TO OUTPUT-COUNT.                                     02280000
120160     GO TO TERM-OUT-RTN.                                          02290000