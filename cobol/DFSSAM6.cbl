 CBL  APOST                                                             00010000
001000 IDENTIFICATION DIVISION.                                         00020000
001100 PROGRAM-ID.  DFSSAM06.                                           00030000
001200*                                                                 00040000
001300********************************************************@SCPYRT** 00050000
001400*                                                               * 00060000
001500*  Licensed Materials - Property of IBM                         * 00070000
001600*                                                               * 00080000
001700*  5635-A06                                                     * 00090000
001800*                                                               * 00100000
001900*      Copyright IBM Corp. 1974,1998 All Rights Reserved.       * 00110000
002000*                                                               * 00120000
002100*  US Government Users Restricted Rights - Use, duplication or  * 00130000
002200*  disclosure restricted by GSA ADP Schedule Contract with      * 00140000
002300*  IBM Corp.                                                    * 00150000
002400********************************************************@ECPYRT** 00160000
002500*                                                                 00170000
002600*             TRANSACTION DISBURSE.                               00180000
002700 ENVIRONMENT DIVISION.                                            00190000
002800 CONFIGURATION SECTION.                                           00200000
002900 SOURCE-COMPUTER. IBM-370.                                        00210000
003000 OBJECT-COMPUTER. IBM-370.                                        00220000
003100                                                                  00230000
003200 DATA DIVISION.                                                   00240000
003300                                                                  00250000
003400 WORKING-STORAGE SECTION.                                         00260000
003500                                                                  00270000
003600 77  GET-UNIQUE, PICTURE XXXX, VALUE 'GU  '.                      00280000
003700 77  GET-NEXT, PICTURE XXXX, VALUE 'GN  '.                        00290000
003800 77  GET-HOLD-UNIQUE, PICTURE XXXX, VALUE 'GHU '.                 00300000
003900 77  GET-HOLD-NEXT, PICTURE XXXX, VALUE 'GHN '.                   00310000
004000 77  REPLACE1, PICTURE XXXX, VALUE 'REPL'.                        00320000
004100 77  IN-SERT,  PICTURE XXXX, VALUE 'ISRT'.                        00330000
004200                                                                  00340000
004300 01  SSA1.                                                        00350000
004400     02  SSA1-NAME, PICTURE X(8), VALUE 'PARTROOT'.               00360000
004500     02  SSA1-BEGIN, PICTURE X, VALUE '('.                        00370000
004600     02  SSA1-KEY, PICTURE X(8), VALUE 'PARTKEY '.                00380000
004700     02  SSA1-OPERATOR, PICTURE XX, VALUE ' ='.                   00390000
004800     02  SSA1-VALUE.                                              00400000
004900         03  PREFIX-1, PICTURE 99, VALUE 02.                      00410000
005000         03  VALUE-1, PICTURE X(15).                              00420000
005100     02  SSA1-END, PICTURE X, VALUE ')'.                          00430000
005200                                                                  00440000
005300 01  SSA2.                                                        00450000
005400     02  SSA2-NAME, PICTURE X(8), VALUE 'STOKSTAT'.               00460000
005500     02  SSA2-BEGIN, PICTURE X, VALUE '('.                        00470000
005600     02  SSA2-KEY, PICTURE X(8), VALUE 'STOCKEY '.                00480000
005700     02  SSA2-OPERATOR, PICTURE XX, VALUE ' ='.                   00490000
005800     02  SSA2-VALUE.                                              00500000
005900         03  PREFIX-2, PICTURE 99, VALUE 00.                      00510000
006000         03  VALUE-2, PICTURE X(8).                               00520000
006100         03  FILLER, PICTURE X(6)  VALUE SPACES.                  00530000
006200     02  SSA2-END, PICTURE X, VALUE ')'.                          00540000
006300                                                                  00550000
006400 01  I-O-AREA1, PICTURE X(136), VALUE SPACES.                     00560000
006500                                                                  00570000
006600 01  TAR-CALCULATION-AREA.                                        00580000
006700     02 TOT-REQMTS           PICTURE S9(8)V9     VALUE ZEROS.     00590000
006800                                                                  00600000
006900 01  TAR-HEADER.                                                  00610000
007000     02  TH-CHAR-CNT         PICTURE S99 COMPUTATIONAL VALUE +48. 00620000
007100     02  FILLER              PICTURE S99 COMPUTATIONAL VALUE +0.  00630000
007200     02  FILLER              PICTURE X(01) VALUE X'15'.           00640000
007300     02  FILLER              PICTURE X(04) VALUE 'PN= '.          00650000
007400     02  TH-PN               PICTURE X(15).                       00660000
007500     02  FILLER              PICTURE X(12) VALUE '; INVTY KEY='.  00670000
007600     02  TH-INV              PICTURE X(08).                       00680000
007700                                                                  00690000
007800 01  TAR-MESSAGE.                                                 00700000
007900     02 TM-CHAR-CNT          PICTURE S99 COMPUTATIONAL.           00710000
008000     02  FILLER              PICTURE S99 COMPUTATIONAL VALUE +0.  00720000
008100     02 TM-T-XT              PICTURE X(120), VALUE SPACES.        00730000
008200                                                                  00740000
008300 01  TM-T-XT01.                                                   00750000
008400     02  FILLER              PICTURE X(01) VALUE X'15'.           00760000
008500     02  FILLER              PICTURE X(22)                        00770000
008600         VALUE 'NEGATIVE STOCK BALANCE'.                          00780000
008700     02  FILLER              PICTURE X(01) VALUE X'15'.           00790000
008800     02  FILLER              PICTURE X(01) VALUE X'15'.           00800000
008900 01  TM-T-XT02.                                                   00810000
009000     02  FILLER              PICTURE X(01) VALUE X'15'.           00820000
009100     02  FILLER              PICTURE X(26)                        00830000
009200         VALUE 'NEGATIVE ON-ORDER POSITION'.                      00840000
009300     02  FILLER              PICTURE X(01) VALUE X'15'.           00850000
009400     02  FILLER              PICTURE X(01) VALUE X'15'.           00860000
009500 01  TM-T-XT03.                                                   00870000
009600     02  FILLER              PICTURE X(01) VALUE X'15'.           00880000
009700     02  FILLER              PICTURE X(20)                        00890000
009800         VALUE 'EXCESS STOCK ON HAND'.                            00900000
009900     02  FILLER              PICTURE X(01) VALUE X'15'.           00910000
010000     02  FILLER              PICTURE X(01) VALUE X'15'.           00920000
010100 01  TM-T-XT04.                                                   00930000
010200     02  FILLER              PICTURE X(01) VALUE X'15'.           00940000
010300     02  FILLER              PICTURE X(21)                        00950000
010400         VALUE 'OVER-ORDERED POSITION'.                           00960000
010500     02  FILLER              PICTURE X(01) VALUE X'15'.           00970000
010600     02  FILLER              PICTURE X(01) VALUE X'15'.           00980000
010700 01  TM-T-XT05.                                                   00990000
010800     02  FILLER              PICTURE X(01) VALUE X'15'.           01000000
010900     02  FILLER              PICTURE X(09)                        01010000
011000         VALUE 'ORDER DUE'.                                       01020000
011100     02  FILLER              PICTURE X(01) VALUE X'15'.           01030000
011200     02  FILLER              PICTURE X(01) VALUE X'15'.           01040000
011300                                                                  01050000
011400 01  OUT-MSG.                                                     01060000
011500     02  CHAR-COUNT, PICTURE S99, COMPUTATIONAL.                  01070000
011600     02  FILLER, PICTURE S99, COMPUTATIONAL, VALUE ZEROES.        01080000
011700     02  T-XT, PICTURE X(132), VALUE SPACES.                      01090000
011800                                                                  01100000
011900 01  OUT-MSG01.                                                   01110000
012000     02  FILLER              PICTURE X(01) VALUE X'15'.           01120000
012100     02  FILLER              PICTURE X(20)                        01130000
012200         VALUE 'QUANTITY NOT NUMERIC'.                            01140000
012300     02  FILLER              PICTURE X(01) VALUE X'15'.           01150000
012400 01  OUT-MSG02.                                                   01160000
012500     02  FILLER              PICTURE X(01) VALUE X'15'.           01170000
012600     02  FILLER              PICTURE X(19)                        01180000
012700         VALUE 'INVALID PART NUMBER'.                             01190000
012800     02  FILLER              PICTURE X(01) VALUE X'15'.           01200000
012900 01  OUT-MSG03.                                                   01210000
013000     02  FILLER              PICTURE X(01) VALUE X'15'.           01220000
013100     02  FILLER              PICTURE X(16)                        01230000
013200         VALUE 'RECORD NOT FOUND'.                                01240000
013300     02  FILLER              PICTURE X(01) VALUE X'15'.           01250000
013400 01  OUT-MSG04.                                                   01260000
013500     02  FILLER              PICTURE X(01) VALUE X'15'.           01270000
013600     02  FILLER              PICTURE X(15)                        01280000
013700         VALUE 'UPDATE COMPLETE'.                                 01290000
013800     02  FILLER              PICTURE X(01) VALUE X'15'.           01300000
013900 01  OUT-MSG05.                                                   01310000
014000     02  FILLER              PICTURE X(01) VALUE X'15'.           01320000
014100     02  FILLER              PICTURE X(52) VALUE                  01330000
014200         'UNABLE TO PROCESS YOUR TRANSACTION. PLEASE REINQUIRE'.  01340000
014300     02  FILLER              PICTURE X(01) VALUE X'15'.           01350000
014400                                                                  01360000
014500 01  STOK-SRA.                                                    01370000
014600     02  FILLER, PICTURE 99.                                      01380000
014700     02  PLAC, PICTURE X.                                         01390000
014800     02  INV-DEPT, PICTURE XX.                                    01400000
014900     02  PROJ-CONTRACT, PICTURE XXX.                              01410000
015000     02  DIV, PICTURE XX.                                         01420000
015100     02  FILLER, PICTURE X(6), VALUE SPACES.                      01430000
015200     02  FILLER, PICTURE X(4), VALUE SPACES.                      01440000
015300     02  UNIT-PRICE, PICTURE 9(6)V999.                            01450000
015400     02  FILLER, PICTURE X(5), VALUE SPACES.                      01460000
015500     02  UNIT-OF-MEAS, PICTURE X(4).                              01470000
015600     02  FILLER, PICTURE X(12), VALUE SPACES.                     01480000
015700     02  ATTRITION.                                               01490000
015800         03   COAP, PICTURE 9V99.                                 01500000
015900         03  PLAN, PICTURE 9V99.                                  01510000
016000         03  COAD, PICTURE X.                                     01520000
016100     02  FILLER, PICTURE X(14), VALUE SPACES.                     01530000
016200     02  LAST-CYC-MDAY, PICTURE XXX.                              01540000
016300     02  LAST-TRANS-MDAY, PICTURE XXX.                            01550000
016400     02  FILLER, PICTURE X(12).                                   01560000
016500     02  CUR-REM-REQ, PICTURE S9(7)V9.                            01570000
016600     02  UNP-REM-REQ, PICTURE S9(7)V9.                            01580000
016700     02  ON-ORDER, PICTURE S9(7)V9.                               01590000
016800     02  TOT-STOCK, PICTURE S9(7)V9.                              01600000
016900     02  PLANNED-DISB, PICTURE S9(7)V9.                           01610000
017000     02  UNP-DISB, PICTURE S9(7)V9.                               01620000
017100     02  SPARES, PICTURE S9(7)V9.                                 01630000
017200     02  DIVERSION, PICTURE S9(7)V9.                              01640000
017300     02  FILLER, PICTURE X(7).                                    01650000
017400                                                                  01660000
017500 01  EDITED-MSG.                                                  01670000
017600     02  TRANSACTION-CODE, PICTURE X(8).                          01680000
017700     02  FIELD-1, PICTURE X(15), VALUE SPACES.                    01690000
017800     02  KEY2.                                                    01700000
017900         03  FIELD-2, PICTURE X, VALUE SPACES.                    01710000
018000         03  FIELD-3, PICTURE XX, VALUE SPACES.                   01720000
018100         03  FIELD-4, PICTURE XXX, VALUE SPACES.                  01730000
018200         03  FIELD-5, PICTURE XX, VALUE SPACES.                   01740000
018300     02  FIELD-6, PICTURE X, VALUE SPACES.                        01750000
018400     02  FIELD-7, PICTURE 9(8), VALUE ZEROES.                     01760000
018500     02  NUM-TEST-QTY REDEFINES FIELD-7, PICTURE X(8).            01770000
018600                                                                  01780000
018700 01  MSG-SEG-CNT, PICTURE S9, VALUE ZEROES, COMPUTATIONAL.        01790000
018800                                                                  01800000
018900 01  PARAM-TABLE.                                                 01810000
019000     02  FILLER, PICTURE S99, VALUE  +15, COMPUTATIONAL.          01820000
019100     02  FILLER, PICTURE XX, VALUE 'L '.                          01830000
019200     02  FILLER, PICTURE S99, VALUE  +8,  COMPUTATIONAL.          01840000
019300     02  FILLER, PICTURE XX, VALUE 'L '.                          01850000
019400     02  FILLER, PICTURE S99, VALUE  +1,  COMPUTATIONAL.          01860000
019500     02  FILLER, PICTURE XX, VALUE 'L '.                          01870000
019600     02  FILLER, PICTURE S99, VALUE  +8,  COMPUTATIONAL.          01880000
019700     02  FILLER, PICTURE XX, VALUE 'R0'.                          01890000
019800     02  END-TABLE, PICTURE S99, VALUE ZEROES, COMPUTATIONAL.     01900000
019900                                                                  01910000
020000 01  PART-LINK.                                                   01920000
020100     02  PART-NO, PICTURE X(17).                                  01930000
020200     02  FILLER, PICTURE X(4).                                    01940000
020300     02  REJECT-CODE, PICTURE X.                                  01950000
020400                                                                  01960000
020500 LINKAGE SECTION.                                                 01970000
020600                                                                  01980000
020700 01  TERM-1-PCB.                                                  01990000
020800     02  I-O-TERMINAL, PICTURE X(8).                              02000000
020900     02  I-O-RESERVE, PICTURE XX.                                 02010000
021000     02  I-O-STATUS, PICTURE XX.                                  02020000
021100     02  IN-PREFIX.                                               02030000
021200         03  FILLER, PICTURE X.                                   02040000
021300         03  I-JULIAN-DATE, PICTURE S9(5), COMPUTATIONAL-3.       02050000
021400         03  INPUT-TIME, PICTURE S9(7), COMPUTATIONAL-3.          02060000
021500         03  FILLER, PICTURE X(4).                                02070000
021600                                                                  02080000
021700 01  TERM-2-PCB.                                                  02090000
021800     02  I-O-TERMINAL2, PICTURE X(8).                             02100000
021900     02  I-O-RESERVE2, PICTURE XX.                                02110000
022000     02  I-O-STATUS2, PICTURE XX.                                 02120000
022100     02  IN-PREFIX2.                                              02130000
022200         03  FILLER, PICTURE X.                                   02140000
022300         03  I-JULIAN-DATE2, PICTURE S9(5), COMPUTATIONAL-3.      02150000
022400         03  INPUT-TIME2, PICTURE S9(7), COMPUTATIONAL-3.         02160000
022500         03  FILLER, PICTURE X(4).                                02170000
022600                                                                  02180000
022700 01  DB-PCB.                                                      02190000
022800     02  DBD-NAME, PICTURE X(8).                                  02200000
022900     02  SEG-LEVEL, PICTURE XX.                                   02210000
023000     02  STATUS-CODE, PICTURE XX.                                 02220000
023100     02  PROC-OPTIONS, PICTURE XXXX.                              02230000
023200     02  RESERVE-DLI, PICTURE S9(5), COMPUTATIONAL.               02240000
023300     02  SEG-NAME-FB, PICTURE X(8).                               02250000
023400     02  LENGTH-FE-KEY, PICTURE S9(5), COMPUTATIONAL.             02260000
023500     02  NUMB-SENS-SEGS, PICTURE S9(5), COMPUTATIONAL.            02270000
023600     02  KEY-FB-AREA.                                             02280000
023700         03  ROOT-SEG-KEY, PICTURE X(17).                         02290000
023800         03  SECOND-SEG-KEY, PICTURE X(16).                       02300000
023900     02  ROOT-SEG-NAME, PICTURE X(8).                             02310000
024000     02  SECOND-SEG-NAME, PICTURE X(8).                           02320000
024100                                                                  02330000
024200 PROCEDURE DIVISION.                                              02340000
024300 ENTRY-POINT.                                                     02350000
024400     ENTRY 'DLITCBL' USING TERM-1-PCB, TERM-2-PCB, DB-PCB.        02360000
024500 GET-TRANSACTION.                                                 02370000
024600     CALL 'CBLTDLI' USING GET-UNIQUE, TERM-1-PCB, I-O-AREA1.      02380000
024700     IF I-O-STATUS NOT = '  ', GO TO ERROR-ANALYSIS.              02390000
024800     MOVE 0 TO MSG-SEG-CNT.                                       02400000
024900 INPUTANAL.                                                       02410000
025000     CALL 'INPANAL' USING PARAM-TABLE, I-O-AREA1, EDITED-MSG      02420000
025100         MSG-SEG-CNT.                                             02430000
025200     IF NUM-TEST-QTY NOT NUMERIC, MOVE OUT-MSG01                  02440000
025300         TO T-XT, MOVE 26 TO CHAR-COUNT, GO TO WRITE-ROUTINE.     02450000
025400 PN-EDIT.                                                         02460000
025500     MOVE FIELD-1 TO PART-NO.                                     02470000
025600     CALL 'PNEDIT' USING PART-LINK.                               02480000
025700     MOVE PART-NO TO FIELD-1.                                     02490000
025800     IF REJECT-CODE NOT = ' ', MOVE OUT-MSG02 TO                  02500000
025900         T-XT, MOVE 25 TO CHAR-COUNT, GO TO WRITE-ROUTINE.        02510000
026000 START-PROCESS.                                                   02520000
026100     MOVE FIELD-1 TO VALUE-1.                                     02530000
026200     MOVE KEY2 TO VALUE-2.                                        02540000
026300     CALL 'CBLTDLI' USING GET-HOLD-UNIQUE, DB-PCB, STOK-SRA,      02550000
026400         SSA1, SSA2.                                              02560000
026500     IF STATUS-CODE = 'GE', MOVE OUT-MSG03 TO                     02570000
026600         T-XT, MOVE 22 TO CHAR-COUNT, GO TO WRITE-ROUTINE.        02580000
026700     IF STATUS-CODE NOT = '  ', GO TO ERROR-ANALYSIS.             02590000
026800     SUBTRACT FIELD-7 FROM TOT-STOCK.                             02600000
026900     IF FIELD-6 = 'P', ADD FIELD-7 TO PLANNED-DISB,               02610000
027000             SUBTRACT FIELD-7 FROM CUR-REM-REQ.                   02620000
027100     IF FIELD-6 = 'U', ADD FIELD-7 TO UNP-DISB,                   02630000
027200             SUBTRACT FIELD-7 FROM UNP-REM-REQ.                   02640000
027300     IF UNP-REM-REQ < 0, MOVE ZEROES TO UNP-REM-REQ.              02650000
027400     CALL 'CBLTDLI' USING REPLACE1, DB-PCB, STOK-SRA.             02660000
027500     IF STATUS-CODE NOT = '  ', GO TO ERROR-ANALYSIS.             02670000
027600     MOVE OUT-MSG04 TO T-XT, MOVE 21 TO CHAR-COUNT,               02680000
027700         PERFORM TAR-CALCULATIONS THRU TAR-EXIT.                  02690000
027800 WRITE-ROUTINE.                                                   02700000
027900     IF TM-T-XT = SPACES GO TO WRITE-MESSAGE.                     02710000
028000     MOVE VALUE-1 TO TH-PN.                                       02720000
028100     MOVE VALUE-2 TO TH-INV.                                      02730000
028200     CALL 'CBLTDLI' USING IN-SERT, TERM-2-PCB, TAR-HEADER.        02740000
028300     CALL 'CBLTDLI' USING IN-SERT, TERM-2-PCB, TAR-MESSAGE.       02750000
028400 WRITE-MESSAGE.                                                   02760000
028500     CALL 'CBLTDLI' USING IN-SERT, TERM-1-PCB, OUT-MSG.           02770000
028600 EOJ.                                                             02780000
028700         GOBACK.                                                  02790000
028800 ERROR-ANALYSIS.                                                  02800000
028900     MOVE OUT-MSG05                                               02810000
029000         TO T-XT, MOVE 58 TO CHAR-COUNT, GO TO WRITE-MESSAGE.     02820000
029100 TAR-CALCULATIONS.                                                02830000
029200     MOVE SPACES TO TM-T-XT.                                      02840000
029300     IF TOT-STOCK NEGATIVE  MOVE TM-T-XT01 TO                     02850000
029400         TM-T-XT, MOVE 29 TO TM-CHAR-CNT  GO TO TAR-EXIT.         02860000
029500     IF ON-ORDER NEGATIVE MOVE TM-T-XT02 TO                       02870000
029600         TM-T-XT, MOVE 33 TO TM-CHAR-CNT, GO TO TAR-EXIT.         02880000
029700     IF CUR-REM-REQ NEGATIVE GO TO TAR-EXIT.                      02890000
029800     COMPUTE TOT-REQMTS ROUNDED =                                 02900000
029900         (CUR-REM-REQ * (1 + PLAN) + UNP-REM-REQ).                02910000
030000     IF TOT-REQMTS LESS THAN TOT-STOCK, MOVE 27 TO TM-CHAR-CNT,   02920000
030100     MOVE TM-T-XT03 TO TM-T-XT, GO TO TAR-EXIT.                   02930000
030200     IF ON-ORDER EQUAL TO ZERO GO TO EXCESS-STOCK-TEST.           02940000
030300     IF TOT-REQMTS < (TOT-STOCK + ON-ORDER) MOVE                  02950000
030400         TM-T-XT04 TO TM-T-XT,                                    02960000
030500         MOVE 28 TO TM-CHAR-CNT,  GO TO TAR-EXIT.                 02970000
030600 EXCESS-STOCK-TEST.                                               02980000
030700     IF (CUR-REM-REQ + UNP-REM-REQ) > (TOT-STOCK + ON-ORDER),     02990000
030800         MOVE TM-T-XT05 TO TM-T-XT, MOVE 15 TO TM-CHAR-CNT.       03000000
030900 TAR-EXIT.                                                        03010000
031000     EXIT.                                                        03020000