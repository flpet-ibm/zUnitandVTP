 CBL  APOST                                                             00010000
001000 IDENTIFICATION DIVISION.                                         00020000
001100 PROGRAM-ID.  DFSSAM07.                                           00030000
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
002600 ENVIRONMENT DIVISION.                                            00180000
002700 CONFIGURATION SECTION.                                           00190000
002800 SOURCE-COMPUTER.  IBM-370.                                       00200000
002900 OBJECT-COMPUTER.  IBM-370.                                       00210000
003000 DATA DIVISION.                                                   00220000
003100 WORKING-STORAGE SECTION.                                         00230000
       77  INPANAL     pic x(8) value 'INPANAL'.                        00230001
       77  PNEDIT      pic x(8) value 'PNEDIT'.
       77  GET-UNIQUE              PICTURE XXXX   VALUE 'GU  '.
003300 77  GET-NEXT                PICTURE XXXX   VALUE 'GN  '.         00250000
003400 77  DLI-INSERT              PICTURE XXXX   VALUE 'ISRT'.         00260000
003500 01  PARTROOT-SSA.                                                00270000
003600     02  ROOT-NAME           PICTURE X(8)  VALUE 'PARTROOT'.      00280000
003700     02  BEGIN-OP            PICTURE X     VALUE '('.             00290000
003800     02  KEY-NAME            PICTURE X(8)  VALUE 'PARTKEY '.      00300000
003900     02  RELATION-OP         PICTURE XX    VALUE ' ='.            00310000
004000     02  KEY-VALUE           PICTURE X(17).                       00320000
004100     02  END-OP              PICTURE X     VALUE ')'.             00330000
004200 01  TERM-IN-AREA.                                                00340000
004300     02 CHAR-COUNT           PICTURE S99  COMPUTATIONAL.          00350000
004400     02 FILLER               PICTURE S99  COMPUTATIONAL.          00360000
004500     02 TRANS-CODE           PICTURE X(7).                        00370000
004600     02 MESSAGE-TEXT-START   PICTURE X(125).                      00380000
004700 01  REFORM-MESSAGE.                                              00390000
004800     02 REFORM-TRANS-CD      PICTURE X(8).                        00400000
004900     02 REFORM-PN            PICTURE X(15).                       00410000
005000     02 FILLER               PICTURE X(109).                      00420000
005100 01  WORK-AREAS.                                                  00430000
005200     02  ROOT-KEY-WA.                                             00440000
005300         04 ROOT-PREFIX      PICTURE XX  VALUE '02'.              00450000
005400         04 PN-WORK          PICTURE X(15).                       00460000
005500     02  MSG-SEG-CNT         PICTURE S9 COMPUTATIONAL VALUE ZERO. 00470000
005600     02  BACK-ORDER-CNT      PICTURE 99, VALUE ZEROES.            00480000
005700     02  REQMTS-POT          PICTURE S9(6)V9.                     00490000
005800     02  DISB-POT            PICTURE S9(6)V9.                     00500000
005900     02  DET-LINE-CNT        PICTURE 99.                          00510000
006000 01  PARAM-TABLE.                                                 00520000
006100     02  FILLER              PICTURE S9(2) VALUE +15 COMP.        00530000
006200     02 FILLER               PICTURE XX    VALUE 'L '.            00540000
006300     02 END-TABLE            PICTURE S99 VALUE ZERO COMPUTATIONAL.00550000
006400 01  PART-LINK.                                                   00560000
006500     02 PART-NO-EDIT         PICTURE X(17).                       00570000
006600     02 FILLER               PICTURE XXXX.                        00580000
006700     02 REJECT-CODE          PICTURE X.                           00590000
006800 01  HEADER-1-AREA.                                               00600000
006900     02  H1-CHAR-CNT         PICTURE S99 VALUE +63 COMP.          00610000
007000     02  FILLER              PICTURE S99 VALUE +0  COMP.          00620000
007100     02 FILLER               PICTURE X(01)  VALUE X'15'.          00630000
007200     02 FILLER               PICTURE XXXXX  VALUE 'PART='.        00640000
007300     02 OUT-PN               PICTURE X(15).                       00650000
007400     02 FILLER               PICTURE X(7) VALUE '; DESC='.        00660000
007500     02 OUT-DESCRIPTION      PICTURE X(15).                       00670000
007600     02 FILLER               PICTURE X(12) VALUE '; PROC CODE='.  00680000
007700     02 OUT-PROCUREMENT-CODE PICTURE XX.                          00690000
007800     02 FILLER               PICTURE X  VALUE ';'.                00700000
007900     02 CARRIAGE-RET         PICTURE X   VALUE X'15'.             00710000
008000 01  HEADER-2-AREA.                                               00720000
008100     02  H2-CHAR-CNT         PICTURE S99 VALUE +86 COMP.          00730000
008200     02  FILLER              PICTURE S99 VALUE +0  COMP.          00740000
008300     02 FILLER               PICTURE X    VALUE X'15'.            00750000
008400     02 FILLER               PICTURE X(40) VALUE                  00760000
008500     '    AREA  INV  PROJ  DIV    UNIT   CURRE'.                  00770000
008600     02 FILLER               PICTURE X(40) VALUE                  00780000
008700     'NT    ON       IN      TOTAL  COUNT BACK'.                  00790000
008800     02 CARRIAGE-RET         PICTURE X     VALUE X'15'.           00800000
008900 01  HEADER-3-AREA.                                               00810000
009000     02  H3-CHAR-CNT         PICTURE S99 VALUE +86 COMP.          00820000
009100     02  FILLER              PICTURE S99 VALUE +0  COMP.          00830000
009200     02 FILLER               PICTURE X(40) VALUE                  00840000
009300     '         DEPT   CD          PRICE  REQMT'.                  00850000
009400     02 FILLER               PICTURE X(40) VALUE                  00860000
009500     'S    ORDER    STOCK  DISBURSE TAKEN ORDR'.                  00870000
009600     02 CARRIAGE-RET         PICTURE XX    VALUE X'15'.           00880000
009700 01  DETAIL-OUT.                                                  00890000
009800     02  DO-CHAR-CNT         PICTURE S99 VALUE +84 COMP.          00900000
009900     02  FILLER              PICTURE S99 VALUE +0  COMP.          00910000
010000     02 DO-LINE-CNT          PICTURE Z9.                          00920000
010100     02 FILLER               PICTURE X  VALUE '.'.                00930000
010200     02 FILLER               PICTURE XX   VALUE SPACES.           00940000
010300     02 DO-AREA              PICTURE X.                           00950000
010400     02 FILLER               PICTURE XXXX VALUE SPACES.           00960000
010500     02 DO-DEPT              PICTURE XX.                          00970000
010600     02 FILLER               PICTURE XXX  VALUE SPACES.           00980000
010700     02 DO-PROJ              PICTURE XXX.                         00990000
010800     02 FILLER               PICTURE XXX  VALUE SPACES.           01000000
010900     02 DO-DIV               PICTURE XX.                          01010000
011000     02 FILLER               PICTURE XX   VALUE SPACES.           01020000
011100     02 DO-UNIT-PRICE        PICTURE ZZZZ.ZZZ.                    01030000
011200     02 FILLER               PICTURE X    VALUE SPACES.           01040000
011300     02 DO-REQMTS            PICTURE ZZZZZ9-.                     01050000
011400     02 FILLER               PICTURE XX VALUE SPACES.             01060000
011500     02 DO-ON-ORDER          PICTURE ZZZZZ9-.                     01070000
011600     02 FILLER               PICTURE XX VALUE SPACES.             01080000
011700     02 DO-IN-STOCK          PICTURE ZZZZZ9-.                     01090000
011800     02 FILLER               PICTURE XX VALUE SPACES.             01100000
011900     02 DO-DISB              PICTURE ZZZZZ9-.                     01110000
012000     02 FILLER               PICTURE XX VALUE SPACES.             01120000
012100     02 FILLER               PICTURE XX   VALUE SPACES.           01130000
012200     02 DO-CYCLE             PICTURE X.                           01140000
012300     02 FILLER               PICTURE XXXX VALUE SPACES.           01150000
012400     02 DO-BACK-ORDER-CNT    PICTURE Z9.                          01160000
012500     02 CARRIAGE-RET         PICTURE X    VALUE X'15'.            01170000
012600 01  SEG-RET-AREA.                                                01180000
012700     02 FILLER               PICTURE X(26).                       01190000
012800     02 PN-DESCRIPTION       PICTURE X(15).                       01200000
012900     02 FILLER               PICTURE X(119).                      01210000
013000 01  STAN-INFO-RET  REDEFINES SEG-RET-AREA.                       01220000
013100     02 FILLER               PICTURE X(18).                       01230000
013200     02 SI-PROCUREMENT-CODE  PICTURE XX.                          01240000
013300     02 FILLER               PICTURE X(140).                      01250000
013400 01  STOCK-STATUS-RET  REDEFINES STAN-INFO-RET.                   01260000
013500     02 FILLER               PICTURE XX.                          01270000
013600     02 SS-AREA              PICTURE X.                           01280000
013700     02 SS-DEPT              PICTURE XX.                          01290000
013800     02 SS-PROJ              PICTURE XXX.                         01300000
013900     02 SS-DIV               PICTURE XX.                          01310000
014000     02  FILLER              PICTURE  X(12).                      01320000
014100     02  SS-UNIT-PRICE       PICTURE  9(4)V999.                   01330000
014200     02 FILLER               PICTURE X(60).                       01340000
014300     02 SS-CUR-REQMTS        PICTURE S9(7)V9.                     01350000
014400     02 SS-UNPL-REQMTS       PICTURE S9(7)V9.                     01360000
014500     02  FILLER              PICTURE X.                           01370000
014600     02  SS-ON-ORDER         PICTURE  S9(6)V9.                    01380000
014700     02  FILLER              PICTURE X.                           01390000
014800     02  SS-IN-STOCK         PICTURE  S9(6)V9.                    01400000
014900     02 SS-PLAN-DISB         PICTURE S9(7)V9.                     01410000
015000     02 SS-UNPL-DISB         PICTURE S9(7)V9.                     01420000
015100     02 FILLER               PICTURE X(23).                       01430000
015200 01  REJECT-MESSAGE.                                              01440000
015300     02 REJ-CHAR-CNT         PICTURE S99 COMPUTATIONAL.           01450000
015400     02  FILLER              PICTURE S99 COMPUTATIONAL VALUE +0.  01460000
015500     02 FILLER               PICTURE X(1)  VALUE X'15'.           01470000
015600     02 FILLER               PICTURE X(7)  VALUE 'PART = '.       01480000
015700     02 REJ-PN               PICTURE X(16) VALUE SPACES.          01490000
015800     02 REJECT-REASON        PICTURE X(110).                      01500000
015900                                                                  01510000
016000 01  REJECT-MSG01.                                                01520000
016100     02  FILLER              PICTURE X(19)                        01530000
016200         VALUE 'INVALID PART NUMBER'.                             01540000
016300     02  FILLER              PICTURE X(01) VALUE X'15'.           01550000
016400 01  REJECT-MSG02.                                                01560000
016500     02  FILLER              PICTURE X(21)                        01570000
016600         VALUE 'PART NOT IN DATA BASE'.                           01580000
016700     02  FILLER              PICTURE X(01) VALUE X'15'.           01590000
016800 01  REJECT-MSG03.                                                01600000
016900     02  FILLER              PICTURE X(17)                        01610000
017000         VALUE 'ITEM NOT IN STOCK'.                               01620000
017100     02  FILLER              PICTURE X(01) VALUE X'15'.           01630000
017200                                                                  01640000
017300 LINKAGE SECTION.                                                 01650000
017400 01  IO-TERM-PCB.                                                 01660000
017500     02 IO-TERMINAL          PICTURE X(8).                        01670000
017600     02 IO-RESERVE           PICTURE XX.                          01680000
017700     02 IO-STATUS            PICTURE XX.                          01690000
017800     02 INPUT-PREFIX         PICTURE X(12).                       01700000
017900 01  PARTFILE-PCB.                                                01710000
018000     02 PN-DBD-NAME          PICTURE X(8).                        01720000
018100     02 PN-SEG-LEVEL         PICTURE XX.                          01730000
018200     02 PN-STATUS-CODE       PICTURE XX.                          01740000
018300     02 PN-PROC-OPTIONS      PICTURE XXXX.                        01750000
018400     02 RESERVE-DLI          PICTURE S9(5) COMPUTATIONAL.         01760000
018500     02 PN-SEG-NAME-FB       PICTURE X(8).                        01770000
018600     02 PN-SEG-FB-LENGTH     PICTURE S9(5) COMPUTATIONAL.         01780000
018700     02 PN-NUMB-SENS-SEGS    PICTURE S9(5) COMPUTATIONAL.         01790000
018800     02 PN-KEY-FB-AREA.                                           01800000
018900         03 PARTROOT-KEY     PICTURE X(17).                       01810000
019000         03 STOKSTAT-KEY     PICTURE X(16).                       01820000
019100         03 BACKORDR-KEY     PICTURE X(10).                       01830000
019200     02 PARTROOT-NAME        PICTURE X(8).                        01840000
019300     02 STOKSTAT-NAME        PICTURE X(8).                        01850000
019400     02 CYCCOUNT-NAME        PICTURE X(8).                        01860000
019500     02 BACKORDR-NAME        PICTURE X(8).                        01870000
019600 PROCEDURE DIVISION.                                              01880000
019700 ENTRY-POINT.                                                     01890000
019800     ENTRY 'DLITCBL' USING IO-TERM-PCB, PARTFILE-PCB.             01900000
019900 GET-TRANSACTION.                                                 01910000
020000     CALL 'CBLTDLI' USING GET-UNIQUE, IO-TERM-PCB, TERM-IN-AREA.  01920000
020100     IF IO-STATUS NOT EQUAL TO '  ' GO TO ABEND.                  01930000
020200 MSG-COMPLETE.                                                    01940000
020300     MOVE ZEROS TO  MSG-SEG-CNT.                                  01950000
020400 CALL-REFORMAT.                                                   01960000
020500     CALL INPANAL USING PARAM-TABLE, TERM-IN-AREA,                01970000
020600                          REFORM-MESSAGE, MSG-SEG-CNT.            01980000
020700 MSG-COMPLETE-EDIT-PN.                                            01990000
020800     MOVE REFORM-PN          TO PART-NO-EDIT.                     02000000
020900     CALL PNEDIT USING PART-LINK.                                 02010000
021000     IF REJECT-CODE NOT EQUAL TO ' '                              02020000
021100         MOVE REJECT-MSG01                                        02030000
021200        TO REJECT-REASON MOVE +46 TO REJ-CHAR-CNT                 02040000
021300         GO TO REJ-TERM-MESSAGE.                                  02050000
021400 GOOD-PART-NUMBER.                                                02060000
021500     MOVE PART-NO-EDIT       TO   PN-WORK.                        02070000
021600     MOVE ROOT-KEY-WA        TO  KEY-VALUE.                       02080000
021700     CALL 'CBLTDLI' USING GET-UNIQUE, PARTFILE-PCB, SEG-RET-AREA, 02090000
021800                          PARTROOT-SSA.                           02100000
021900     IF PN-STATUS-CODE NOT EQUAL TO SPACES,                       02110000
022000          MOVE REJECT-MSG02 TO REJECT-REASON                      02120000
022100       MOVE +50 TO REJ-CHAR-CNT  GO TO REJ-TERM-MESSAGE.          02130000
022200 ROOT-FOUND.                                                      02140000
022300     MOVE PN-DESCRIPTION     TO OUT-DESCRIPTION.                  02150000
022400     MOVE PN-WORK            TO OUT-PN.                           02160000
022500 GET-STANINFO.                                                    02170000
022600     PERFORM GET-NEXT-RTN.                                        02180000
022700     IF  PN-STATUS-CODE NOT EQUAL TO SPACES GO TO ABEND.          02190000
022800     IF PN-SEG-NAME-FB NOT EQUAL TO 'STANINFO' GO TO ABEND.       02200000
022900     MOVE SI-PROCUREMENT-CODE TO OUT-PROCUREMENT-CODE.            02210000
023000 GET-FIRST-STOKSTAT.                                              02220000
023100     MOVE ZERO TO DET-LINE-CNT                                    02230000
023200     PERFORM GET-NEXT-RTN.                                        02240000
023300     IF PN-STATUS-CODE NOT EQUAL TO 'GK'                          02250000
023400     MOVE +46 TO REJ-CHAR-CNT                                     02260000
023500     MOVE REJECT-MSG03 TO REJECT-REASON                           02270000
023600        GO TO REJ-TERM-MESSAGE.                                   02280000
023700     PERFORM SETUP-STOKSTAT-INFO.                                 02290000
023800 WRITE-HEADERS.                                                   02300000
023900     CALL 'CBLTDLI' USING DLI-INSERT, IO-TERM-PCB, HEADER-1-AREA. 02310000
024000     IF IO-STATUS NOT EQUAL TO SPACES  GO TO ABEND.               02320000
024100     CALL 'CBLTDLI' USING DLI-INSERT, IO-TERM-PCB, HEADER-2-AREA. 02330000
024200     IF IO-STATUS NOT EQUAL TO SPACES GO TO ABEND.                02340000
024300     CALL 'CBLTDLI' USING DLI-INSERT, IO-TERM-PCB, HEADER-3-AREA. 02350000
024400     IF IO-STATUS NOT EQUAL TO SPACES GO TO ABEND.                02360000
024500 STOKSTAT-ACCUM-LOOP.                                             02370000
024600     PERFORM GET-NEXT-RTN.                                        02380000
024700     IF PN-STATUS-CODE EQUAL TO 'GA' AND PN-SEG-NAME-FB NOT EQUAL 02390000
024800         TO 'STOKSTAT', PERFORM WRITE-DETAIL, GO TO WRAP-UP.      02400000
024900     IF IO-STATUS NOT EQUAL TO SPACES GO TO ABEND.                02410000
025000     IF PN-STATUS-CODE EQUAL TO 'GB',                             02420000
025100        PERFORM WRITE-DETAIL, GO TO WRAP-UP.                      02430000
025200     IF PN-STATUS-CODE     EQUAL TO 'GA' GO TO NEW-STOKSTAT-FOUND.02440000
025300     IF PN-STATUS-CODE     EQUAL TO '  ' GO TO CHECK-IT.          02450000
025400     IF PN-STATUS-CODE     EQUAL TO 'GK' GO TO CHECK-IT.          02460000
025500     GO TO ABEND.                                                 02470000
025600 CHECK-IT.                                                        02480000
025700     IF PN-SEG-NAME-FB EQUAL TO 'CYCCOUNT'                        02490000
025800        MOVE 'Y' TO  DO-CYCLE   GO TO STOKSTAT-ACCUM-LOOP.        02500000
025900     IF PN-SEG-NAME-FB EQUAL TO 'BACKORDR'                        02510000
026000        ADD  +1    TO BACK-ORDER-CNT                              02520000
026100          GO TO STOKSTAT-ACCUM-LOOP.                              02530000
026200 NEW-STOKSTAT-FOUND.                                              02540000
026300     PERFORM WRITE-DETAIL.                                        02550000
026400     IF IO-STATUS NOT EQUAL TO ' ' GO TO ABEND.                   02560000
026500     MOVE 'N' TO  DO-CYCLE.                                       02570000
026600     MOVE ZEROS TO BACK-ORDER-CNT.                                02580000
026700     PERFORM SETUP-STOKSTAT-INFO.                                 02590000
026800     GO TO STOKSTAT-ACCUM-LOOP.                                   02600000
026900 GET-NEXT-RTN.                                                    02610000
027000     CALL 'CBLTDLI' USING GET-NEXT, PARTFILE-PCB, SEG-RET-AREA.   02620000
027100 REJ-TERM-MESSAGE.                                                02630000
027200     MOVE PART-NO-EDIT       TO REJ-PN.                           02640000
027300     CALL 'CBLTDLI' USING DLI-INSERT, IO-TERM-PCB, REJECT-MESSAGE.02650000
027400     IF IO-STATUS NOT EQUAL TO SPACES  GO TO ABEND.               02660000
027500     GO TO WRAP-UP.                                               02670000
027600 SETUP-STOKSTAT-INFO.                                             02680000
027700     MOVE SS-AREA            TO  DO-AREA.                         02690000
027800     MOVE SS-DEPT            TO  DO-DEPT.                         02700000
027900     MOVE SS-PROJ            TO  DO-PROJ.                         02710000
028000     MOVE SS-DIV             TO  DO-DIV.                          02720000
028100     MOVE SS-UNIT-PRICE      TO  DO-UNIT-PRICE.                   02730000
028200     MOVE SS-ON-ORDER        TO  DO-ON-ORDER.                     02740000
028300     MOVE SS-IN-STOCK        TO  DO-IN-STOCK.                     02750000
028400     MOVE  ZEROS             TO REQMTS-POT, DISB-POT.             02760000
028500     ADD  SS-CUR-REQMTS, SS-UNPL-REQMTS  GIVING  REQMTS-POT.      02770000
028600     ADD  SS-PLAN-DISB,  SS-UNPL-DISB    GIVING  DISB-POT.        02780000
028700     MOVE REQMTS-POT         TO  DO-REQMTS.                       02790000
028800     MOVE DISB-POT           TO  DO-DISB.                         02800000
028900     ADD +1                  TO DET-LINE-CNT.                     02810000
029000     MOVE DET-LINE-CNT       TO  DO-LINE-CNT.                     02820000
029100     MOVE 'N'                TO DO-CYCLE.                         02830000
029200     MOVE ZEROS              TO BACK-ORDER-CNT.                   02840000
029300 WRITE-DETAIL.                                                    02850000
029400     MOVE BACK-ORDER-CNT     TO DO-BACK-ORDER-CNT.                02860000
029500     CALL 'CBLTDLI' USING DLI-INSERT, IO-TERM-PCB, DETAIL-OUT.    02870000
029600 WRAP-UP.                                                         02880000
029700     MOVE ZEROS TO BACK-ORDER-CNT, REQMTS-POT, DISB-POT,          02890000
029800                   DET-LINE-CNT.                                  02900000
029900 END-IT.                                                          02910000
030000         GOBACK.                                                  02920000
030100 ABEND.                                                           02930000
030200     DISPLAY             IO-STATUS PN-STATUS-CODE PN-SEG-NAME-FB, 02940000
030300     UPON CONSOLE.                                                02950000
030400     GO TO WRAP-UP.                                               02960000