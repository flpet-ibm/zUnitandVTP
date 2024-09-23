 CBL  APOST                                                             00010000
000002 IDENTIFICATION DIVISION.                                         00020000
000005 PROGRAM-ID.    DFSSAM03.                                         00030000
000006*                                                                 00040000
000007********************************************************@SCPYRT** 00050000
000008*                                                               * 00060000
000009*  Licensed Materials - Property of IBM                         * 00070000
000010*                                                               * 00080000
000011*  5635-A06                                                     * 00090000
000012*                                                               * 00100000
000013*      Copyright IBM Corp. 1974,1998 All Rights Reserved.       * 00110000
000014*                                                               * 00120000
000015*  US Government Users Restricted Rights - Use, duplication or  * 00130000
000016*  disclosure restricted by GSA ADP Schedule Contract with      * 00140000
000017*  IBM Corp.                                                    * 00150000
000018********************************************************@ECPYRT** 00160000
000019*                                                                 00170000
000020*          SINGLE-LOCATION INVENTORY DISPLAY PROGRAM.             00180000
000021*          THE TRANSACTION CODE WHICH ACTIVATES THE               00190000
000022*          PROGRAM IS DSPINV.                                     00200000
000023 ENVIRONMENT DIVISION.                                            00210000
000024 CONFIGURATION SECTION.                                           00220000
000040 SOURCE-COMPUTER.  IBM-370.                                       00230000
000060 OBJECT-COMPUTER.  IBM-370.                                       00240000
000080 DATA DIVISION.                                                   00250000
000100 WORKING-STORAGE SECTION.                                         00260000
000120 01  NEXT-FUNC               PICTURE X(04)  VALUE 'GN  '.         00270000
000140 01  UNIQ-FUNC               PICTURE X(04)  VALUE 'GU  '.         00280000
000160 01  ISRT-FUNC               PICTURE X(04)  VALUE 'ISRT'.         00290000
000180 01  STOKSTAT-WRITE-SW       PICTURE X(02) VALUE SPACES.          00300000
000200 01  PARTROOT-SSA.                                                00310000
000220     02  ROOT-NAME           PICTURE X(8)  VALUE 'PARTROOT'.      00320000
000240     02  BEGIN-OP            PICTURE X     VALUE '('.             00330000
000260     02  KEY-NAME            PICTURE X(8)  VALUE 'PARTKEY '.      00340000
000280     02  RELATION-OP         PICTURE XX    VALUE ' ='.            00350000
000300     02  KEY-VALUE           PICTURE X(17).                       00360000
000320     02  END-OP              PICTURE X     VALUE ')'.             00370000
000340 01  STOKSTAT-SSA.                                                00380000
000360     02 FILLER               PICTURE X(08)  VALUE 'STOKSTAT'.     00390000
000380     02 FILLER               PICTURE X(01)  VALUE '('.            00400000
000400     02 FILLER               PICTURE X(08)  VALUE 'STOCKEY'.      00410000
000420     02 FILLER               PICTURE X(02)  VALUE ' ='.           00420000
000440     02 SS-SSA-KEY.                                               00430000
000460        03 FILLER            PICTURE X(02)  VALUE ZEROS.          00440000
000480        03 SS-SSA-KEY-VALUE  PICTURE X(08).                       00450000
000500        03 FILLER            PICTURE X(06)  VALUE SPACES.         00460000
000520     02 FILLER               PICTURE X(01)  VALUE ')'.            00470000
000540 01  TERM-IN-AREA.                                                00480000
000560     02 FILLER               PICTURE X(140)  VALUE SPACES.        00490000
000580 01  REFORM-MESSAGE.                                              00500000
000600     02 REFORM-TRANS-CD      PICTURE X(8).                        00510000
000620     02 PART-NO              PICTURE X(15).                       00520000
000640     02 INPUT-SS-KEY         PICTURE X(08).                       00530000
000660     02 FILLER               PICTURE X(109).                      00540000
000680 01  WORK-AREAS.                                                  00550000
000700     02  ROOT-KEY-WA.                                             00560000
000720         04 ROOT-PREFIX      PICTURE XX  VALUE '02'.              00570000
000740         04 PN-WORK          PICTURE X(15).                       00580000
000760     02  MSG-SEG-CNT         PICTURE S9 COMPUTATIONAL VALUE ZERO. 00590000
000780 01  PARAM-TABLE.                                                 00600000
000800     02  FILLER              PICTURE S9(2) VALUE +15 COMP.        00610000
000820     02 FILLER               PICTURE XX    VALUE 'L '.            00620000
000840     02  FILLER              PICTURE S99 VALUE +8 COMP.           00630000
000860     02 FILLER               PICTURE X(02)  VALUE 'L '.           00640000
000880     02 END-TABLE            PICTURE S99 VALUE ZERO COMPUTATIONAL.00650000
000900 01  PART-LINK.                                                   00660000
000920     02 PART-NO-EDIT         PICTURE X(17).                       00670000
000940     02 FILLER               PICTURE XXXX.                        00680000
000960     02 REJECT-CODE          PICTURE X.                           00690000
000980 01  SEG-RET-AREA.                                                00700000
001000     02 FILLER1              PICTURE X(02).                       00710000
001020     02 PART-NO              PICTURE X(15).                       00720000
001040     02 FILLER2              PICTURE X(09).                       00730000
001060     02 DESC                 PICTURE X(15).                       00740000
001080     02 FILLER3              PICTURE X(119).                      00750000
001100 01  STAN-INFO-RET  REDEFINES SEG-RET-AREA.                       00760000
001120     02 FILLER1              PICTURE X(18).                       00770000
001140     02 PROC-CODE            PICTURE XX.                          00780000
001160 01  STOCK-STATUS-RET  REDEFINES STAN-INFO-RET.                   00790000
001180     02 FILLER1              PICTURE XX.                          00800000
001200     02 SS-AREA              PICTURE X.                           00810000
001220     02 SS-DEPT              PICTURE XX.                          00820000
001240     02 SS-PROJ              PICTURE XXX.                         00830000
001260     02 SS-DIV               PICTURE XX.                          00840000
001280     02 FILLER2              PICTURE X(10).                       00850000
001300     02 SS-UNIT-PRICE        PICTURE 9(6)V999.                    00860000
001320     02 FILLER3              PICTURE X(05).                       00870000
001340     02 SS-UNIT-OF-MEAS      PICTURE X(04).                       00880000
001360     02 FILLER4              PICTURE X(33).                       00890000
001380     02 SS-STOCK-DATE        PICTURE X(03).                       00900000
001400     02 FILLER5              PICTURE X(15).                       00910000
001420     02 SS-CUR-REQMTS        PICTURE S9(7)V9.                     00920000
001440     02 SS-UNPL-REQMTS       PICTURE S9(7)V9.                     00930000
001460     02 SS-ON-ORDER          PICTURE S9(7)V9.                     00940000
001480     02 SS-IN-STOCK          PICTURE S9(7)V9.                     00950000
001500     02 SS-PLAN-DISB         PICTURE S9(7)V9.                     00960000
001520     02 SS-UNPL-DISB         PICTURE S9(7)V9.                     00970000
001540     02 FILLER6              PICTURE X(23).                       00980000
001560 01  BACK-ORDER-RET          REDEFINES STOCK-STATUS-RET.          00990000
001580     02 FILLER1              PICTURE X(02).                       01000000
001600     02 WORK-ORDER           PICTURE X(08).                       01010000
001620     02 FILLER2              PICTURE X(53).                       01020000
001640     02 WO-QTY               PICTURE S9(07)V9.                    01030000
001660 01  CYCLE-COUNT-RET         REDEFINES BACK-ORDER-RET.            01040000
001680     02 FILLER               PICTURE X(02).                       01050000
001700     02 PHYSICAL-COUNT       PICTURE S9(07)V9.                    01060000
001720     02 FILLER               PICTURE X(04).                       01070000
001740     02 TOTAL-STOCK          PICTURE S9(07)V9.                    01080000
001760 01  LINE-1-AREA.                                                 01090000
001780     02 FILLER               PICTURE S99 COMPUTATIONAL VALUE +62. 01100000
001800     02  FILLER              PICTURE S99 VALUE ZERO               01110000
001810                                           COMPUTATIONAL.         01120000
001820     02 FILLER               PICTURE X(01)  VALUE X'15'.          01130000
001840     02 FILLER               PICTURE X(05)  VALUE  'PART='.       01140000
001860     02 PART-NO              PICTURE X(15).                       01150000
001880     02 FILLER               PICTURE X(7) VALUE '; DESC='.        01160000
001900     02 DESC                 PICTURE X(15).                       01170000
001920     02 FILLER               PICTURE X(12) VALUE '; PROC CODE='.  01180000
001940     02 PROC-CODE            PICTURE XX.                          01190000
001960     02 CARR-RET             PICTURE X(01)  VALUE X'15'.          01200000
001980 01  LINE-2-AREA.                                                 01210000
002000     02 FILLER               PICTURE S9(02)  VALUE +88            01220000
002020                                             COMPUTATIONAL.       01230000
002040     02 FILLER               PICTURE S9(02)  VALUE ZERO           01240000
002041                                             COMPUTATIONAL.       01250000
002060     02 FILLER               PICTURE X(01)  VALUE X'15'.          01260000
002080     02 FILLER               PICTURE X(05)  VALUE  'AREA='.       01270000
002100     02 SS-AREA              PICTURE X(01).                       01280000
002120     02 FILLER               PICTURE X(11)  VALUE '; INV DEPT='.  01290000
002140     02 SS-DEPT              PICTURE X(02).                       01300000
002160     02 FILLER               PICTURE X(06)  VALUE '; PRJ='.       01310000
002180     02 SS-PROJ              PICTURE X(03).                       01320000
002200     02 FILLER               PICTURE X(06)  VALUE '; DIV='.       01330000
002220     02 SS-DIV               PICTURE X(02).                       01340000
002240     02 FILLER               PICTURE X(08)  VALUE '; PRICE='.     01350000
002260     02 SS-UNIT-PRICE        PICTURE Z(6).999.                    01360000
002280     02 FILLER               PICTURE X(14) VALUE '; STK CT DATE='.01370000
002300     02 SS-STOCK-DATE        PICTURE X(03).                       01380000
002320     02 FILLER               PICTURE X(07) VALUE '; UNIT='.       01390000
002340     02 SS-UNIT-OF-MEAS      PICTURE X(04).                       01400000
002360     02 CARR-RET             PICTURE X(01)  VALUE X'15'.          01410000
002380 01  LINE-3-AREA.                                                 01420000
002400     02 FILLER               PICTURE S9(02)   VALUE +67           01430000
002420                                             COMPUTATIONAL.       01440000
002440     02 FILLER               PICTURE S9(02)  VALUE ZERO           01450000
002441                                             COMPUTATIONAL.       01460000
002460     02 FILLER               PICTURE X(01)  VALUE X'15'.          01470000
002480     02 FILLER               PICTURE X(12) VALUE  'CURR REQMTS='. 01480000
002500     02 SS-CUR-REQMTS        PICTURE Z(06)9-.                     01490000
002520     02 FILLER               PICTURE X(11) VALUE '; ON ORDER='.   01500000
002540     02 SS-ON-ORDER          PICTURE Z(06)9-.                     01510000
002560     02 FILLER               PICTURE X(14) VALUE '; TOTAL STOCK='.01520000
002580     02 SS-IN-STOCK          PICTURE Z(06)9-.                     01530000
002600     02 CARR-RET             PICTURE X(01)  VALUE X'15'.          01540000
002620 01  LINE-4-AREA.                                                 01550000
002640     02 FILLER               PICTURE S9(02)   VALUE +79           01560000
002660                                             COMPUTATIONAL.       01570000
002680     02 FILLER               PICTURE S9(02)  VALUE ZERO           01580000
002681                                             COMPUTATIONAL.       01590000
002700     02 FILLER               PICTURE X(01)  VALUE X'15'.          01600000
002720     02 FILLER               PICTURE X(13) VALUE  'DISB PLANNED='.01610000
002740     02 SS-PLAN-DISB         PICTURE Z(06)9-.                     01620000
002760     02 FILLER               PICTURE X(17) VALUE                  01630000
002780                                           '; DISB UNPLANNED='.   01640000
002800     02 SS-UNPL-DISB         PICTURE Z(06)9-.                     01650000
002820     02 FILLER               PICTURE X(18) VALUE                  01660000
002840                                           '; STK CT VARIANCE='.  01670000
002860     02  STOCK-VAR           PICTURE Z(07)9-.                     01680000
002880     02 CARR-RET             PICTURE X(01)  VALUE X'15'.          01690000
002900 01  LINE-5-AREA.                                                 01700000
002920     02 FILLER               PICTURE S9(02)   VALUE +57           01710000
002940                                             COMPUTATIONAL.       01720000
002960     02 FILLER               PICTURE S9(02)  VALUE ZERO           01730000
002961                                             COMPUTATIONAL.       01740000
002980     02 FILLER               PICTURE X(01)    VALUE X'15'.        01750000
003000     02 DESC-1               PICTURE X(24).                       01760000
003020     02 WORK-ORDER           PICTURE X(08).                       01770000
003040     02 DESC-2               PICTURE X(11).                       01780000
003060     02 WO-QTY               PICTURE Z(06)9-.                     01790000
003080     02 CARR-RET             PICTURE X(01)  VALUE X'15'.          01800000
003100 01  NO-PARTROOT-MSG.                                             01810000
003120     02 FILLER               PICTURE S9(02)   VALUE +48           01820000
003140                                            COMPUTATIONAL.        01830000
003160     02 FILLER               PICTURE S9(02)  VALUE ZERO           01840000
003161                                             COMPUTATIONAL.       01850000
003180     02 FILLER               PICTURE X(01)  VALUE X'15'.          01860000
003200     02 FILLER               PICTURE X(10) VALUE 'PART NO.  '.    01870000
003220     02 PART-NO              PICTURE X(15).                       01880000
003240     02 FILLER               PICTURE X(17) VALUE                  01890000
003260                                           ' NOT IN DATA BASE'.   01900000
003280     02 CARR-RET             PICTURE X(01)  VALUE X'15'.          01910000
003300 01  NO-STOKSTAT-MSG.                                             01920000
003320     02 FILLER               PICTURE S9(02)   VALUE +45           01930000
003340                                            COMPUTATIONAL.        01940000
003360     02 FILLER               PICTURE S9(02)  VALUE ZERO           01950000
003361                                             COMPUTATIONAL.       01960000
003380     02 FILLER               PICTURE X(01)  VALUE X'15'.          01970000
003400     02 FILLER               PICTURE X(14) VALUE 'STOCK RECORD  '.01980000
003420     02 STOCK-KEY            PICTURE X(08).                       01990000
003440     02 FILLER               PICTURE X(17)  VALUE                 02000000
003460                                           ' NOT IN DATA BASE'.   02010000
003480     02 CARR-RET             PICTURE X(01)  VALUE X'15'.          02020000
003500 LINKAGE SECTION.                                                 02030000
003520 01  IO-TERM-PCB.                                                 02040000
003540     02 IO-TERMINAL          PICTURE X(8).                        02050000
003560     02 IO-RESERVE           PICTURE XX.                          02060000
003580     02 IO-STATUS            PICTURE XX.                          02070000
003600     02 INPUT-PREFIX         PICTURE X(12).                       02080000
003620 01  PARTFILE-PCB.                                                02090000
003640     02 PN-DBD-NAME          PICTURE X(8).                        02100000
003660     02 PN-SEG-LEVEL         PICTURE XX.                          02110000
003680     02 PN-STATUS-CODE       PICTURE XX.                          02120000
003700     02 PN-PROC-OPTIONS      PICTURE XXXX.                        02130000
003720     02 RESERVE-DLI          PICTURE S9(5) COMPUTATIONAL.         02140000
003740     02 PN-SEG-NAME-FB       PICTURE X(8).                        02150000
003760 PROCEDURE DIVISION.                                              02160000
003780 ENTRY-POINT.                                                     02170000
003800     ENTRY 'DLITCBL' USING IO-TERM-PCB, PARTFILE-PCB.             02180000
003840*                                                                 02190000
003860     MOVE SPACES TO STOKSTAT-WRITE-SW.                            02200000
003880     MOVE 'OUTSTANDING WORK ORDERS=' TO DESC-1 OF LINE-5-AREA.    02210000
003900     MOVE '; QUANTITY=' TO              DESC-2 OF LINE-5-AREA.    02220000
003920 GET-TRANSACTION.                                                 02230000
003960     CALL 'CBLTDLI' USING UNIQ-FUNC, IO-TERM-PCB, TERM-IN-AREA.   02240000
004000 CALL-INPUT-ANALYZER.                                             02250000
004040     CALL 'INPANAL' USING PARAM-TABLE, TERM-IN-AREA,              02260000
004060                          REFORM-MESSAGE, MSG-SEG-CNT.            02270000
004100 CALL-PART-EDIT.                                                  02280000
004120     MOVE PART-NO OF REFORM-MESSAGE   TO PART-NO-EDIT.            02290000
004160     CALL 'PNEDIT' USING PART-LINK.                               02300000
004200 FIND-PART-IN-DATA-BASE.                                          02310000
004220     MOVE PART-NO-EDIT       TO   PN-WORK.                        02320000
004240     MOVE ROOT-KEY-WA        TO  KEY-VALUE.                       02330000
004280     CALL 'CBLTDLI' USING UNIQ-FUNC,  PARTFILE-PCB, SEG-RET-AREA, 02340000
004300                          PARTROOT-SSA.                           02350000
004340     IF PN-STATUS-CODE NOT EQUAL TO SPACES,                       02360000
004360         GO TO PARTROOT-NOT-FOUND.                                02370000
004380 PARTROOT-FOUND.                                                  02380000
004400     MOVE CORRESPONDING SEG-RET-AREA TO LINE-1-AREA.              02390000
004420 FIND-STANINFO-IF-PRESENT.                                        02400000
004460     CALL 'CBLTDLI' USING NEXT-FUNC, PARTFILE-PCB, SEG-RET-AREA.  02410000
004500     IF (PN-STATUS-CODE EQUAL TO 'GB')                            02420000
004520                      OR                                          02430000
004540        (PN-SEG-NAME-FB NOT EQUAL TO 'STANINFO')                  02440000
004560         MOVE SPACES TO PROC-CODE OF LINE-1-AREA                  02450000
004580                          ELSE                                    02460000
004600         MOVE CORRESPONDING STAN-INFO-RET TO LINE-1-AREA.         02470000
004620     PERFORM WRITE-LINE-1 THRU WRITE-LINE-1-EXIT.                 02480000
004640 GET-UNIQUE-STOKSTAT.                                             02490000
004660     MOVE INPUT-SS-KEY TO SS-SSA-KEY-VALUE.                       02500000
004700     CALL 'CBLTDLI' USING UNIQ-FUNC, PARTFILE-PCB, SEG-RET-AREA,  02510000
004720                                 PARTROOT-SSA, STOKSTAT-SSA.      02520000
004760     IF PN-STATUS-CODE EQUAL TO 'GE'                              02530000
004780         GO TO STOKSTAT-NOT-FOUND.                                02540000
004800 STOKSTAT-FOUND.                                                  02550000
004820     MOVE CORRESPONDING STOCK-STATUS-RET TO LINE-2-AREA.          02560000
004840     PERFORM WRITE-LINE-2 THRU WRITE-LINE-2-EXIT.                 02570000
004860     MOVE CORRESPONDING STOCK-STATUS-RET TO LINE-3-AREA.          02580000
004880     PERFORM WRITE-LINE-3 THRU WRITE-LINE-3-EXIT.                 02590000
004900     MOVE CORRESPONDING STOCK-STATUS-RET TO LINE-4-AREA.          02600000
004920     MOVE 'ON' TO STOKSTAT-WRITE-SW.                              02610000
004940     MOVE ZEROS TO STOCK-VAR OF LINE-4-AREA.                      02620000
004960 GET-NEXT.                                                        02630000
005000     CALL 'CBLTDLI' USING NEXT-FUNC, PARTFILE-PCB, SEG-RET-AREA.  02640000
005040     IF PN-STATUS-CODE EQUAL TO 'GB'                              02650000
005060         GO TO END-CURR-ROOT.                                     02660000
005080     IF PN-SEG-NAME-FB EQUAL TO 'PARTROOT'  GO TO END-CURR-ROOT.  02670000
005100     IF PN-SEG-NAME-FB EQUAL TO 'STOKSTAT'  GO TO END-CURR-ROOT.  02680000
005120     IF PN-SEG-NAME-FB EQUAL TO 'CYCCOUNT'  GO TO CYCCOUNT-FOUND. 02690000
005140     IF PN-SEG-NAME-FB EQUAL TO 'BACKORDR'  GO TO BACKORDR-FOUND. 02700000
005160     GO TO GET-NEXT.                                              02710000
005180 CYCCOUNT-FOUND.                                                  02720000
005200     COMPUTE STOCK-VAR OF LINE-4-AREA = PHYSICAL-COUNT  OF        02730000
005220                                        CYCLE-COUNT-RET    -      02740000
005240                                        TOTAL-STOCK OF            02750000
005260                                        CYCLE-COUNT-RET.          02760000
005280     PERFORM WRITE-LINE-4 THRU WRITE-LINE-4-EXIT.                 02770000
005300     GO TO GET-NEXT.                                              02780000
005320 BACKORDR-FOUND.                                                  02790000
005340     IF STOKSTAT-WRITE-SW EQUAL TO 'ON'                           02800000
005360         PERFORM WRITE-LINE-4 THRU WRITE-LINE-4-EXIT.             02810000
005380     MOVE CORRESPONDING BACK-ORDER-RET TO LINE-5-AREA.            02820000
005400     PERFORM WRITE-LINE-5 THRU WRITE-LINE-5-EXIT.                 02830000
005420     MOVE SPACES TO DESC-1 OF LINE-5-AREA.                        02840000
005440     MOVE SPACES TO DESC-2 OF LINE-5-AREA.                        02850000
005460     GO TO GET-NEXT.                                              02860000
005480 END-CURR-ROOT.                                                   02870000
005500     IF STOKSTAT-WRITE-SW EQUAL TO 'ON'                           02880000
005520         PERFORM WRITE-LINE-4 THRU WRITE-LINE-4-EXIT.             02890000
005540     GO TO END-IT.                                                02900000
005560 PARTROOT-NOT-FOUND.                                              02910000
005580     MOVE PN-WORK TO PART-NO OF NO-PARTROOT-MSG.                  02920000
005620     CALL 'CBLTDLI' USING ISRT-FUNC, IO-TERM-PCB, NO-PARTROOT-MSG.02930000
005660     GO TO END-IT.                                                02940000
005680 STOKSTAT-NOT-FOUND.                                              02950000
005700     MOVE INPUT-SS-KEY TO STOCK-KEY OF NO-STOKSTAT-MSG.           02960000
005740     CALL 'CBLTDLI' USING ISRT-FUNC, IO-TERM-PCB, NO-STOKSTAT-MSG.02970000
005780     GO TO END-IT.                                                02980000
005800 WRITE-LINE-1.                                                    02990000
005840     CALL 'CBLTDLI' USING ISRT-FUNC, IO-TERM-PCB, LINE-1-AREA.    03000000
005880 WRITE-LINE-1-EXIT.  EXIT.                                        03010000
005900 WRITE-LINE-2.                                                    03020000
005940     CALL 'CBLTDLI' USING ISRT-FUNC, IO-TERM-PCB, LINE-2-AREA.    03030000
005980 WRITE-LINE-2-EXIT.  EXIT.                                        03040000
006000 WRITE-LINE-3.                                                    03050000
006040     CALL 'CBLTDLI' USING ISRT-FUNC, IO-TERM-PCB, LINE-3-AREA.    03060000
006080 WRITE-LINE-3-EXIT.  EXIT.                                        03070000
006100 WRITE-LINE-4.                                                    03080000
006140     CALL 'CBLTDLI' USING ISRT-FUNC, IO-TERM-PCB, LINE-4-AREA.    03090000
006180     MOVE SPACES TO STOKSTAT-WRITE-SW.                            03100000
006200 WRITE-LINE-4-EXIT.  EXIT.                                        03110000
006220 WRITE-LINE-5.                                                    03120000
006260     CALL 'CBLTDLI' USING ISRT-FUNC, IO-TERM-PCB, LINE-5-AREA.    03130000
006300 WRITE-LINE-5-EXIT.  EXIT.                                        03140000
006320 END-IT.                                                          03150000
006360     GOBACK.                                                      03160000