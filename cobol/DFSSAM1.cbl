 CBL  APOST                                                             00010000
000010 IDENTIFICATION DIVISION.                                         00020000
000020 PROGRAM-ID.    DFSSAM01.                                         00030000
000022*                                                                 00040000
000024********************************************************@SCPYRT** 00050000
000026*                                                               * 00060000
000028*  Licensed Materials - Property of IBM                         * 00070000
000030*                                                               * 00080000
000032*  5635-A06                                                     * 00090000
000034*                                                               * 00100000
000036*      Copyright IBM Corp. 1974,1998 All Rights Reserved.       * 00110000
000038*                                                               * 00120000
000039*  US Government Users Restricted Rights - Use, duplication or  * 00130000
000040*  disclosure restricted by GSA ADP Schedule Contract with      * 00140000
000041*  IBM Corp.                                                    * 00150000
000042********************************************************@ECPYRT** 00160000
000044*                                                                 00170000
000050*               DATA BASE LOAD PROGRAM.                           00180000
000060 ENVIRONMENT DIVISION.                                            00190000
000080 CONFIGURATION SECTION.                                           00200000
000100 SOURCE-COMPUTER.    IBM-370.                                     00210000
000120 OBJECT-COMPUTER.    IBM-370.                                     00220000
000140 INPUT-OUTPUT SECTION.                                            00230000
000160 FILE-CONTROL.                                                    00240000
000180     SELECT  INPUT-FILE      ASSIGN TO UT-S-INPUT.                00250000
000200 DATA DIVISION.                                                   00260000
000220 FILE SECTION.                                                    00270000
000240 FD  INPUT-FILE                                                   00280000
000250     RECORD CONTAINS 80 CHARACTERS                                00290000
000255     BLOCK CONTAINS 0 RECORDS                                     00300000
000260     RECORDING MODE IS F                                          00310000
000280     LABEL RECORDS ARE OMITTED                                    00320000
000300     DATA RECORD IS INPUT-RECORD.                                 00330000
000320 01  INPUT-RECORD.                                                00340000
000340     02 INP-SEG-NAME         PICTURE X(08).                       00350000
000360     02 FILLER               PICTURE X(01).                       00360000
000380     02 INP-DATA             PICTURE X(67).                       00370000
000400     02 INP-SEQUENCE-NO      PICTURE X(04).                       00380000
000420 WORKING-STORAGE SECTION.                                         00390000
000440 01  DL1-FUNCTION            PICTURE X(04).                       00400000
000460 01  PREV-SEG-NAME           PICTURE X(08)            VALUE SPACE.00410000
000480 01  PREV-SEQUENCE-NO        PICTURE X(04)            VALUE SPACE.00420000
000500 01  BUILD-SEGMENT-AREA.                                          00430000
000520     02 BUILD-DATA-AREA      OCCURS 14 TIMES                      00440000
000540                             PICTURE X(67).                       00450000
000560 01  MISC-ARITHMETIC-FIELDS  USAGE COMPUTATIONAL.                 00460000
000580     02 SUB-1                PICTURE S9(02)           VALUE ZEROS.00470000
000600 01  SEG00010-SSA.                                                00480000
000620     02 SEG-NAME-00010       PICTURE X(08) VALUE 'PARTROOT'.      00490000
000640     02 BEGIN-OP-00010       PICTURE X(01) VALUE '('.             00500000
000660     02 KEY-NAME-00010       PICTURE X(08) VALUE 'PARTKEY '.      00510000
000680     02 REL-OPER-00010       PICTURE X(02) VALUE ' ='.            00520000
000700     02 KEY-VALUE-00010      PICTURE X(17).                       00530000
000720     02 END-OP-00010         PICTURE X(01) VALUE ')'.             00540000
000740 01  SEG00060-SSA.                                                00550000
000760     02 SEG-NAME-00060       PICTURE X(08) VALUE 'STANINFO'.      00560000
000780     02 BEGIN-OP-00060       PICTURE X(01) VALUE '('.             00570000
000800     02 KEY-NAME-00060       PICTURE X(08) VALUE 'STANKEY '.      00580000
000820     02 REL-OPER-00060       PICTURE X(02) VALUE ' ='.            00590000
000840     02 KEY-VALUE-00060      PICTURE X(02).                       00600000
000860     02 END-OP-00060         PICTURE X(01) VALUE ')'.             00610000
000880 01  SEG02000-SSA.                                                00620000
000900     02 SEG-NAME-02000       PICTURE X(08) VALUE 'STOKSTAT'.      00630000
000920     02 BEGIN-OP-02000       PICTURE X(01) VALUE '('.             00640000
000940     02 KEY-NAME-02000       PICTURE X(08) VALUE 'STOCKEY '.      00650000
000960     02 REL-OPER-02000       PICTURE X(02) VALUE ' ='.            00660000
000980     02 KEY-VALUE-02000      PICTURE X(16).                       00670000
001000     02 END-OP-02000         PICTURE X(01) VALUE ')'.             00680000
001020 01  SEG02200-SSA.                                                00690000
001040     02 SEG-NAME-02200       PICTURE X(08) VALUE 'CYCCOUNT'.      00700000
001060     02 BEGIN-OP-02200       PICTURE X(01) VALUE '('.             00710000
001080     02 KEY-NAME-02200       PICTURE X(08) VALUE 'CYCLKEY '.      00720000
001100     02 REL-OPER-02200       PICTURE X(02) VALUE ' ='.            00730000
001120     02 KEY-VALUE-02200      PICTURE X(02).                       00740000
001140     02 END-OP-02200         PICTURE X(01) VALUE ')'.             00750000
001160 01  SEG02300-SSA.                                                00760000
001180     02 SEG-NAME-02300       PICTURE X(08) VALUE 'BACKORDR'.      00770000
001200     02 BEGIN-OP-02300       PICTURE X(01) VALUE '('.             00780000
001220     02 KEY-NAME-02300       PICTURE X(08) VALUE 'BACKKEY '.      00790000
001240     02 REL-OPER-02300       PICTURE X(02) VALUE ' ='.            00800000
001260     02 KEY-VALUE-02300      PICTURE X(10).                       00810000
001280     02 END-OP-02300         PICTURE X(01) VALUE ')'.             00820000
001300 01  SEG00010-INSERT-AREA.                                        00830000
001320     02 FILLER               PICTURE X(050).                      00840000
001340 01  SEG00060-INSERT-AREA.                                        00850000
001360     02 FILLER               PICTURE X(61).                       00860000
001362     02 RIGHT-MAKE-SPAN      PICTURE S9(03).                      00870000
001364     02 FILLER               PICTURE X(06).                       00880000
001366     02 WRONG-MAKE-SPAN      PICTURE 9(03).                       00890000
001368     02 FILLER               PICTURE X(12).                       00900000
001380 01  SEG02000-INSERT-AREA.                                        00910000
001400     02 FILLER               PICTURE X(160).                      00920000
001420 01  SEG02200-INSERT-AREA.                                        00930000
001440     02 FILLER               PICTURE X(025).                      00940000
001460 01  SEG02300-INSERT-AREA.                                        00950000
001480     02 FILLER               PICTURE X(075).                      00960000
001500 LINKAGE SECTION.                                                 00970000
001520 01  PCB-AREA-1.                                                  00980000
001540     02 DBD-NAME             PICTURE  X(08).                      00990000
001560     02 SEGMENT-LEVEL        PICTURE  X(02).                      01000000
001580     02 STATUS-CODES         PICTURE  X(02).                      01010000
001600     02 PROCESS-OPTIONS      PICTURE X(04).                       01020000
001620     02 FILLER               PICTURE S9(05)  COMPUTATIONAL.       01030000
001640     02 SEG-NAME-FEEDBACK    PICTURE  X(08).                      01040000
001660 PROCEDURE DIVISION.                                              01050000
001680 ENTRY-POINT.                                                     01060000
001700     ENTRY 'DLITCBL' USING  PCB-AREA-1.                           01070000
001740     DISPLAY 'START DB LOAD'  UPON CONSOLE.                       01080000
001760     OPEN  INPUT  INPUT-FILE.                                     01090000
001780     MOVE 'ISRT'  TO DL1-FUNCTION.                                01100000
001800 READ-INPUT-FILE.                                                 01110000
001820     READ INPUT-FILE         AT END                               01120000
001840                             GO TO END-INP-FILE.                  01130000
001860 BUILD-SEGMENT.                                                   01140000
001880     IF INP-SEG-NAME NOT EQUAL TO SPACES                          01150000
001900         PERFORM WRITE-BUILT-SEGMENT THRU WRITE-SEGMENT-EXIT      01160000
001920         MOVE ZEROS TO SUB-1                                      01170000
001940         MOVE SPACES TO BUILD-SEGMENT-AREA                        01180000
001960         MOVE INP-SEG-NAME TO PREV-SEG-NAME.                      01190000
001980     ADD 1 TO SUB-1.                                              01200000
002000     IF SUB-1 IS GREATER THAN 14                                  01210000
002020         DISPLAY 'MORE THAN 14 CARDS PER SEGMENT'  UPON CONSOLE   01220000
002040         DISPLAY 'SEGMENT IS   '  PREV-SEG-NAME   UPON CONSOLE    01230000
002060         GO TO LOCKED-HALT.                                       01240000
002080     MOVE INP-DATA TO BUILD-DATA-AREA (SUB-1).                    01250000
002100     GO TO READ-INPUT-FILE.                                       01260000
002120 WRITE-BUILT-SEGMENT.                                             01270000
002140     IF PREV-SEG-NAME EQUAL TO SPACES                             01280000
002160         GO TO WRITE-SEGMENT-EXIT.                                01290000
002180     IF PREV-SEG-NAME = 'PARTROOT'  GO TO SEGMENT-IS-SEG00010.    01300000
002200     IF PREV-SEG-NAME = 'STANINFO'  GO TO SEGMENT-IS-SEG00060.    01310000
002220     IF PREV-SEG-NAME = 'STOKSTAT'  GO TO SEGMENT-IS-SEG02000.    01320000
002240     IF PREV-SEG-NAME = 'CYCCOUNT'  GO TO SEGMENT-IS-SEG02200.    01330000
002260     IF PREV-SEG-NAME = 'BACKORDR'  GO TO SEGMENT-IS-SEG02300.    01340000
002280 INVALID-SEGMENT-NAME.                                            01350000
002300     DISPLAY 'INVALID SEGMENT NAME =  ' PREV-SEG-NAME.            01360000
002320     GO TO LOCKED-HALT.                                           01370000
002340 SEGMENT-IS-SEG00010.                                             01380000
002360     MOVE BUILD-SEGMENT-AREA TO SEG00010-INSERT-AREA.             01390000
002380     MOVE BUILD-SEGMENT-AREA TO KEY-VALUE-00010.                  01400000
002400     MOVE SPACE TO BEGIN-OP-00010.                                01410000
002420     CALL  'CBLTDLI' USING DL1-FUNCTION, PCB-AREA-1,              01420000
002440                           SEG00010-INSERT-AREA, SEG00010-SSA.    01430000
002460     MOVE '(' TO BEGIN-OP-00010.                                  01440000
002480     IF STATUS-CODES NOT = SPACES, GO TO SEGMENT-INSERT-ERROR.    01450000
002500     GO TO WRITE-SEGMENT-EXIT.                                    01460000
002520 SEGMENT-IS-SEG00060.                                             01470000
002540     MOVE BUILD-SEGMENT-AREA TO SEG00060-INSERT-AREA.             01480000
002550     MOVE WRONG-MAKE-SPAN TO RIGHT-MAKE-SPAN.                     01490000
002560     MOVE BUILD-SEGMENT-AREA TO KEY-VALUE-00060.                  01500000
002580     MOVE SPACE TO BEGIN-OP-00060.                                01510000
002600     CALL 'CBLTDLI' USING DL1-FUNCTION, PCB-AREA-1,               01520000
002620                            SEG00060-INSERT-AREA, SEG00010-SSA,   01530000
002640                                                  SEG00060-SSA.   01540000
002660     MOVE '(' TO BEGIN-OP-00060.                                  01550000
002680     IF STATUS-CODES NOT = SPACES, GO TO SEGMENT-INSERT-ERROR.    01560000
002700     GO TO WRITE-SEGMENT-EXIT.                                    01570000
002720 SEGMENT-IS-SEG02000.                                             01580000
002740     MOVE BUILD-SEGMENT-AREA TO SEG02000-INSERT-AREA.             01590000
002760     MOVE BUILD-SEGMENT-AREA TO KEY-VALUE-02000.                  01600000
002780     MOVE SPACE TO BEGIN-OP-02000.                                01610000
002800     CALL 'CBLTDLI' USING DL1-FUNCTION, PCB-AREA-1,               01620000
002820                            SEG02000-INSERT-AREA, SEG00010-SSA,   01630000
002840                                                  SEG02000-SSA.   01640000
002860     MOVE '(' TO BEGIN-OP-02000.                                  01650000
002880     IF STATUS-CODES NOT = SPACES, GO TO SEGMENT-INSERT-ERROR.    01660000
002900     GO TO WRITE-SEGMENT-EXIT.                                    01670000
002920 SEGMENT-IS-SEG02200.                                             01680000
002940     MOVE BUILD-SEGMENT-AREA TO SEG02200-INSERT-AREA.             01690000
002960     MOVE BUILD-SEGMENT-AREA TO KEY-VALUE-02200.                  01700000
002980     MOVE SPACE TO BEGIN-OP-02200.                                01710000
003000     CALL 'CBLTDLI' USING DL1-FUNCTION, PCB-AREA-1,               01720000
003020                            SEG02200-INSERT-AREA, SEG00010-SSA,   01730000
003040                                                  SEG02000-SSA,   01740000
003060                                                  SEG02200-SSA.   01750000
003080     MOVE '(' TO BEGIN-OP-02200.                                  01760000
003100     IF STATUS-CODES NOT = SPACES, GO TO SEGMENT-INSERT-ERROR.    01770000
003120     GO TO WRITE-SEGMENT-EXIT.                                    01780000
003140 SEGMENT-IS-SEG02300.                                             01790000
003160     MOVE BUILD-SEGMENT-AREA TO SEG02300-INSERT-AREA.             01800000
003180     MOVE BUILD-SEGMENT-AREA TO KEY-VALUE-02300.                  01810000
003200     MOVE SPACE TO BEGIN-OP-02300.                                01820000
003220     CALL 'CBLTDLI' USING DL1-FUNCTION, PCB-AREA-1,               01830000
003240                            SEG02300-INSERT-AREA, SEG00010-SSA,   01840000
003260                                                  SEG02000-SSA,   01850000
003280                                                  SEG02300-SSA.   01860000
003300     MOVE '(' TO BEGIN-OP-02300.                                  01870000
003320     IF STATUS-CODES NOT = SPACES, GO TO SEGMENT-INSERT-ERROR.    01880000
003340     GO TO WRITE-SEGMENT-EXIT.                                    01890000
003360 WRITE-SEGMENT-EXIT.   EXIT.                                      01900000
003380 SEGMENT-INSERT-ERROR.                                            01910000
003400     DISPLAY  'SEGMENT '                                          01920000
003420              PREV-SEG-NAME                                       01930000
003440              ' INSERT ERROR, '                                   01940000
003460              ' STATUS CODE= '                                    01950000
003480              STATUS-CODES                 UPON CONSOLE.          01960000
003500     GO TO WRITE-SEGMENT-EXIT.                                    01970000
003520 END-INP-FILE.                                                    01980000
003540     CLOSE INPUT-FILE.                                            01990000
003560     PERFORM WRITE-BUILT-SEGMENT THRU WRITE-SEGMENT-EXIT.         02000000
003580     DISPLAY 'END   DB LOAD'  UPON CONSOLE.                       02010000
003600 LOCKED-HALT.                                                     02020000
003620     GOBACK.                                                      02030000