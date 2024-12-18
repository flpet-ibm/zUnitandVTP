 CBL  APOST                                                             00010000
010010 IDENTIFICATION DIVISION.                                         00020000
010020 PROGRAM-ID.  DFSSAM04.                                           00030000
010022*                                                                 00040000
010024********************************************************@SCPYRT** 00050000
010026*                                                               * 00060000
010028*  Licensed Materials - Property of IBM                         * 00070000
010030*                                                               * 00080000
010032*  5635-A06                                                     * 00090000
010034*                                                               * 00100000
010036*      Copyright IBM Corp. 1974,1998 All Rights Reserved.       * 00110000
010038*                                                               * 00120000
010039*  US Government Users Restricted Rights - Use, duplication or  * 00130000
010040*  disclosure restricted by GSA ADP Schedule Contract with      * 00140000
010041*  IBM Corp.                                                    * 00150000
010045********************************************************@ECPYRT** 00160000
010050*                                                                 00170000
010051*             IMS/VS DEMONSTRATION OF TERMINAL ABILITY            00180000
010052*             TO ADD OR DELETE AN ENTIRE PART NUMBER              00190000
010053*             OR ONE OF ITS INVENTORY SEGMENTS FROM               00200000
010054*             AN EXISTING INVENTORY DATABASE.                     00210000
010055*             THE TRANSACTION CODES WHICH ACTIVATE THIS           00220000
010056*             PROGRAM ARE ADDI ADDINV ADDPN ADDPART               00230000
010057*             DLETI DLETINV DLETPART AND DLETPN.                  00240000
010060 ENVIRONMENT DIVISION.                                            00250000
010070 CONFIGURATION SECTION.                                           00260000
010080 SOURCE-COMPUTER. IBM-370.                                        00270000
010090 OBJECT-COMPUTER. IBM-370.                                        00280000
010100 DATA DIVISION.                                                   00290000
010110 WORKING-STORAGE SECTION.                                         00300000
       77  INPANAL     pic x(8) value 'INPANAL'.                        00300001
       77  PNEDIT      pic x(8) value 'PNEDIT'.                         00300002
       77  GET-UNIQUE, PICTURE X(4), VALUE 'GU  '.                      00310000
010130 77  GET-HOLD-UNIQUE, PICTURE X(4), VALUE 'GHU '.                 00320000
010140 77  GET-NEXT, PICTURE X(4), VALUE 'GN  '.                        00330000
010150 77  DELEET, PICTURE X(4), VALUE 'DLET'.                          00340000
010160 77  IN-SERT, PICTURE XXXX VALUE 'ISRT'.                          00350000
010161 77  DELETE-MSG PICTURE X(13) VALUE 'DELETE       '.              00360000
010162 77  ADD-INV-MSG PICTURE X(13) VALUE 'ADD INVENTORY'.             00370000
010163 77  DEL-INV-MSG PICTURE X(13) VALUE 'DELETE INVTRY'.             00380000
010164 77  SEG-NOT-FOUND PICTURE XX VALUE 'GE'.                         00390000
010165 77  NO-MORE-INPUT PICTURE XX VALUE 'QC'.                         00400000
010166 77  END-OF-MSG    PICTURE XX VALUE 'QD'.                         00410000
010167 77  NO-ROOT-ISRT  PICTURE XX VALUE 'GE'.                         00420000
010168 77  DUPL-SEGMENT PICTURE XX VALUE 'II'.                          00430000
010170 77  MSG-SEG-CNT PICTURE S99 VALUE +1 COMPUTATIONAL.              00440000
010171 77  ERROR-SWITCH PICTURE X VALUE ' '.                            00450000
010180 01  PART-NO-EDIT-TABLE.                                          00460000
010190     02  PART-NO-ENTRY  PICTURE X(17).                            00470000
010200     02  FILLER         PICTURE X(4).                             00480000
010210     02  PART-NO-REJECT PICTURE X(1).                             00490000
020010 01  ADD-PART-NO-PARAM-TABLE.                                     00500000
020020     02  PN1     PICTURE S9(2) VALUE +15 COMPUTATIONAL.           00510000
020030     02  FILLER  PICTURE X(2)  VALUE 'L '.                        00520000
020040     02  DGSCR   PICTURE S9(2) VALUE +20 COMPUTATIONAL.           00530000
020050     02  FILLER  PICTURE X(2)  VALUE 'L '.                        00540000
020060     02  PROCOD  PICTURE S9(2) VALUE +2 COMPUTATIONAL.            00550000
020070     02  FILLER  PICTURE X(2)  VALUE 'L '.                        00560000
020080     02  INDICATE-END PICTURE S9(2) VALUE ZERO  COMPUTATIONAL.    00570000
020090 01  ALL-OTHERS-PARAM-TABLE.                                      00580000
020100     02  PN2     PICTURE S9(2) VALUE +15 COMPUTATIONAL.           00590000
020110     02  FILLER  PICTURE X(2)  VALUE 'L '.                        00600000
020120     02  AREA2   PICTURE S9(2) VALUE +8 COMPUTATIONAL.            00610000
020130     02  FILLER  PICTURE X(2)  VALUE 'L '.                        00620000
020200     02  END-INDICATR PICTURE S9(2) VALUE ZERO  COMPUTATIONAL.    00630000
030030 01  EDITED-ADDPN-MSG.                                            00640000
030035     02  TRANS-ADDPN PICTURE X(8) VALUE SPACES.                   00650000
030040     02  PART-NO PICTURE X(15) VALUE SPACES.                      00660000
030050     02  DESCRPTN PICTURE X(20) VALUE SPACES.                     00670000
030060     02  PROC-CODE PICTURE XX VALUE SPACES.                       00680000
030070 01  EDITED-ALL-OTHERS-MSG.                                       00690000
030075     02  TRANS-CODE  PICTURE X(8) VALUE SPACES.                   00700000
030080     02  PART-NUM PICTURE X(15) VALUE SPACES.                     00710000
030090     02  STOKSTAT-KEY.                                            00720000
030100         03  AREAX    PICTURE X(8) VALUE SPACES.                  00730000
030140         03  FILLER   PICTURE X(6) VALUE SPACES.                  00740000
030150 01  LINE-INPUT.                                                  00750000
030160     02  INPUT-COUNT  PICTURE S9(2).                              00760000
030170     02  FILLER       PICTURE S9(2).                              00770000
030180     02  INPUT-TEXT   PICTURE X(132).                             00780000
030181     02  INPT-TXT REDEFINES INPUT-TEXT.                           00790000
030182         03  FIRST-6 PICTURE X(6).                                00800000
030183         03  REST PICTURE X(126).                                 00810000
030190 01  LINE-OUTPUT.                                                 00820000
030200     02  OUTPUT-COUNT PICTURE S9(2) VALUE +5 COMPUTATIONAL.       00830000
030210     02  FILLER       PICTURE S9(2) VALUE ZEROS COMPUTATIONAL.    00840000
030215     02  CARR-RETURN PICTURE X VALUE X'15'.                       00850000
030220     02  OUTPUT-TEXT  PICTURE X(132) VALUE SPACES.                00860000
030250 01  PART-REJECTED-MSG.                                           00870000
030260     02  A PICTURE X(24) VALUE 'PART NUMBER REJECT CODE '.        00880000
030270     02  REJECT-CODE PICTURE X.                                   00890000
030280     02  CR-1 PICTURE X.                                          00900000
030340 01  STATUS-CODE-MSG.                                             00910000
030350     02  D PICTURE X(23) VALUE 'UNRESOLVED STATUS CODE '.         00920000
030360     02  FILL-STATUS PICTURE XX.                                  00930000
030370     02  E PICTURE X(4) VALUE ' ON '.                             00940000
030380     02  FILL-FUNCTION PICTURE X(4).                              00950000
030390     02  CR-2 PICTURE X.                                          00960000
030410 01  PART-ALREADY-THERE.                                          00970000
030420     02  F PICTURE X(12) VALUE 'PART NUMBER '.                    00980000
030430     02  FILL-PART PICTURE X(15).                                 00990000
030440     02  G PICTURE X(20) VALUE ' ALREADY ON DATABASE'.            01000000
030450     02  CR-3 PICTURE X.                                          01010000
030460 01  PART-NOT-FOUND.                                              01020000
030470     02  H PICTURE X(12) VALUE 'PART NUMBER '.                    01030000
030480     02  FILL-PART-2 PICTURE X(15).                               01040000
030490     02  I PICTURE X(21) VALUE                                    01050000
030500              ' NOT FOUND-UNABLE TO '.                            01060000
030510     02  FILL-ERR-1 PICTURE X(13).                                01070000
030520     02  CR-4 PICTURE X.                                          01080000
030530 01  INV-NOT-FOUND.                                               01090000
030540     02  J PICTURE X(10) VALUE 'INVENTORY '.                      01100000
030550     02  FILL-INV PICTURE X(8).                                   01110000
030560     02  K PICTURE X(27) VALUE ' NOT FOUND FOR PART NUMBER '.     01120000
030570     02  FILL-PART-3 PICTURE X(15).                               01130000
030580     02  L PICTURE X(20) VALUE ' SO UNABLE TO DELETE'.            01140000
030590     02  CR-5 PICTURE X.                                          01150000
030610 01  PART-ADD-SUCCESS.                                            01160000
030620     02  M PICTURE X(12) VALUE 'PART NUMBER '.                    01170000
030630     02  FILL-PART-4 PICTURE X(15).                               01180000
030640     02  N PICTURE X(19) VALUE ' ADDED TO DATA BASE'.             01190000
030650     02  CR-6 PICTURE X.                                          01200000
030660 01  INV-ADD-SUCCESS.                                             01210000
030670     02  O PICTURE X(10) VALUE 'INVENTORY '.                      01220000
030680     02  FILL-INV-2 PICTURE X(8).                                 01230000
030690     02  P PICTURE X(22) VALUE ' ADDED TO PART NUMBER '.          01240000
030700     02  FILL-PART-5 PICTURE X(15).                               01250000
030710     02  CR-7 PICTURE X.                                          01260000
030720 01  PART-DEL-SUCCESS.                                            01270000
030730     02  Q PICTURE X(12) VALUE 'PART NUMBER '.                    01280000
030740     02  FILL-PART-6 PICTURE X(15).                               01290000
030750     02  R PICTURE X(23) VALUE ' DELETED FROM DATA BASE'.         01300000
030760     02  CR-8 PICTURE X.                                          01310000
030770 01  INV-DEL-SUCCESS.                                             01320000
030780     02  S PICTURE X(10) VALUE 'INVENTORY '.                      01330000
030790     02  FILL-INV-3 PICTURE X(8).                                 01340000
030800     02  T PICTURE X(26) VALUE ' DELETED FROM PART NUMBER '.      01350000
030810     02  FILL-PART-7 PICTURE X(15).                               01360000
030820     02  CR-9 PICTURE X.                                          01370000
030830 01  STOCK-BAL-ERROR.                                             01380000
030840     02  Y PICTURE X(10) VALUE 'INVENTORY '.                      01390000
030850     02  FILL-INV-4 PICTURE X(8).                                 01400000
030860     02  Z PICTURE X(38) VALUE                                    01410000
030870                ' HAS NON-ZERO BALANCE-UNABLE TO DELETE'.         01420000
030880     02  CR-10 PICTURE X.                                         01430000
030890 01  INVALID-PN-MSG.                                              01440000
030900     02  AA PICTURE X(12) VALUE 'PART NUMBER '.                   01450000
030910     02  FILL-PART-8 PICTURE X(15).                               01460000
030920     02  BB PICTURE X(17) VALUE ' INVALID-RESUBMIT'.              01470000
030930     02  CR-11 PICTURE X.                                         01480000
040010 01  PARTNO-SSA.                                                  01490000
040020     02  SEGM-NT PICTURE X(8) VALUE 'PARTROOT'.                   01500000
040030     02  PARTLEFT PICTURE X   VALUE '('.                          01510000
040040     02  PART-KEY PICTURE X(10) VALUE 'PARTKEY  ='.               01520000
040050     02  PART-KEY-VALUE.                                          01530000
040060         03  CONST PICTURE XX VALUE '02'.                         01540000
040070         03  VAR-PART PICTURE X(15).                              01550000
040080     02  PARTRIGHT PICTURE X VALUE ')'.                           01560000
040090 01  STANINFO-SSA.                                                01570000
040100     02  SEGNAME PICTURE X(8) VALUE 'STANINFO'.                   01580000
040110     02  STANINFO-LEFT PICTURE X VALUE '('.                       01590000
040120     02  REST-OF-IT PICTURE X(13) VALUE 'STANKEY  =02)'.          01600000
040130 01  STOKSTAT-SSA.                                                01610000
040140     02  STOK-SEGMENT PICTURE X(8) VALUE 'STOKSTAT'.              01620000
040150     02  STOKSTAT-LEFT PICTURE X VALUE '('.                       01630000
040160     02  STOK-KEY PICTURE X(10) VALUE 'STOCKEY  ='.               01640000
040170     02  STOK-KEY-VALUE.                                          01650000
040180         03  CON PICTURE XX  VALUE '00'.                          01660000
040190         03  VAR-STOK  PICTURE X(14).                             01670000
040200     02  STOKSTAT-RIGHT PICTURE X VALUE ')'.                      01680000
041010 01  INV-ALREADY-THERE-MSG.                                       01690000
041020     02  CC PICTURE X(10) VALUE 'INVENTORY '.                     01700000
041030     02  FILL-INV-5 PICTURE X(8).                                 01710000
041040     02  DD PICTURE X(33) VALUE                                   01720000
041050               ' ALREADY PRESENT FOR PART NUMBER '.               01730000
041060     02  FILL-PART-9  PICTURE X(15).                              01740000
041070     02  CR-12 PICTURE X.                                         01750000
041080 01  HAS-AN-INV.                                                  01760000
041090     02  EE  PICTURE X(12) VALUE 'PART NUMBER '.                  01770000
041100     02  FILL-PART-10 PICTURE X(15).                              01780000
041110     02  FF  PICTURE X(37) VALUE                                  01790000
041120                ' HAS INVENTORY SEGMENTS-CANNOT DELETE'.          01800000
041130     02  CR-13 PICTURE X.                                         01810000
042010 01  ROOT-FORMAT.                                                 01820000
042020     02 U PICTURE XX VALUE '02'.                                  01830000
042030     02 ROOT-KEY PICTURE X(15).                                   01840000
042040     02 FILLER PICTURE X(9) VALUE SPACES.                         01850000
042050     02 ROOT-DESCR PICTURE X(20).                                 01860000
042060     02 FILLER PICTURE X(4) VALUE SPACES.                         01870000
042070 01  STANINFO-FORMAT.                                             01880000
042080     02 V PICTURE XX VALUE '02'.                                  01890000
042090     02 FILLER PICTURE X(16) VALUE SPACES.                        01900000
042100     02 STAN-PROC-CODE PICTURE XX VALUE SPACES.                   01910000
042101     02 INVENTORY-CODE PICTURE  X VALUE SPACE.                    01920000
042102     02 PLANNING-REVISION-NUMBER PICTURE XX VALUE SPACES.         01930000
042103     02 FILLER PICTURE X(24) VALUE SPACES.                        01940000
042104     02 MAKE-DEPT PICTURE XX VALUE SPACES.                        01950000
042105     02 MAKE-COST-CTR PICTURE XX VALUE SPACES.                    01960000
042106     02 FILLER PICTURE XX VALUE SPACES.                           01970000
042107     02 COMMODITY-CODE PICTURE XXXX VALUE SPACES.                 01980000
042108     02 FILLER PICTURE XXXX VALUE SPACES.                         01990000
042109     02 MAKE-SPAN PICTURE S999 VALUE ZERO.                        02000000
042110     02 FILLER PICTURE X(21) VALUE SPACES.                        02010000
042120 01  STOKSTAT-FORMAT.                                             02020000
042130     02  W PICTURE XX VALUE '00'.                                 02030000
042140     02  STOKSTATUS-KEY.                                          02040000
042150         03  X PICTURE X(8).                                      02050000
042160         03  FILLER PICTURE X(6) VALUE SPACES.                    02060000
042170     02  FILLER PICTURE XXXX  VALUE SPACES.                       02070000
042171     02  U-P   PICTURE S9(6)V999 VALUE ZERO.                      02080000
042172     02  FILLER PICTURE X(21) VALUE SPACES.                       02090000
042173     02  ATTRITION.                                               02100000
042174         03  COAP    PICTURE S999 VALUE ZERO.                     02110000
042175         03  PLANNED PICTURE S999 VALUE ZERO.                     02120000
042176         03  COAD PICTURE X VALUE SPACE.                          02130000
042177     02  FILLER PICTURE X(32) VALUE SPACES.                       02140000
042178     02  REQUIREMENTS.                                            02150000
042179         03  CURRNT     PICTURE S9(7)V9 VALUE ZERO.               02160000
042180         03  UNPLANNED  PICTURE S9(7)V9 VALUE ZERO.               02170000
042181     02  ON-ORDER       PICTURE S9(7)V9 VALUE ZERO.               02180000
042182     02  TOTAL-STOCK    PICTURE S9(7)V9 VALUE ZERO.               02190000
042183     02  DISBURSEMENTS.                                           02200000
042184         03  PLANNED PICTURE S9(7)V9 VALUE ZERO.                  02210000
042185         03  UNPLAN  PICTURE S9(7)V9 VALUE ZERO.                  02220000
042186         03  SPARES  PICTURE S9(7)V9 VALUE ZERO.                  02230000
042187         03  DIVERS  PICTURE S9(7)V9 VALUE ZERO.                  02240000
042188     02  FILLER PICTURE X(7) VALUE SPACES.                        02250000
050010 LINKAGE SECTION.                                                 02260000
050020 01  TERM-NAL.                                                    02270000
050030     02  LTERM-NAME   PICTURE X(8).                               02280000
050040     02  TERM-RESERVE PICTURE XX.                                 02290000
050050     02  TERM-STATUS  PICTURE XX.                                 02300000
050060     02  TERM-PREFIX.                                             02310000
050061         03  FILLER PICTURE X.                                    02320000
050062         03  JULIAN-DATE PICTURE S9(5) COMPUTATIONAL-3.           02330000
050063         03  TIME-O-DAY  PICTURE S9(7) COMPUTATIONAL-3.           02340000
050064         03  FILLER PICTURE XXXX.                                 02350000
050070 01  DATABASE.                                                    02360000
050080     02  DBASE-NAME   PICTURE X(8).                               02370000
050090     02  SEGMENT-INDR PICTURE XX.                                 02380000
050100     02  DBASE-STATUS PICTURE XX.                                 02390000
050110     02  PROC-OPTIONS PICTURE XXXX.                               02400000
050120     02  DLI-RESERVED PICTURE XXXX.                               02410000
050130     02  SEG-FEEDBACK PICTURE X(8).                               02420000
050140     02  KEY-FEEDBACK-LENGTH PICTURE XXXX.                        02430000
050150     02  NO-OF-SENSEG-TYPES  PICTURE XXXX.                        02440000
050160     02  KEY-FEEDBACK.                                            02450000
050170         03  ROOT-KEY.                                            02460000
050180             04  CONS    PICTURE XX.                              02470000
050190             04  VARY    PICTURE X(15).                           02480000
050200         03  STOCKKEY.                                            02490000
050210             04  CONS1   PICTURE XX.                              02500000
050220             04  VARY1   PICTURE X(8).                            02510000
050230             04  CONS2   PICTURE X(6).                            02520000
060010     02  PARTROOT-SEGNAME PICTURE X(8).                           02530000
060020     02  STANINFO-SEGNAME PICTURE X(8).                           02540000
060030     02  STOKSTAT-SEGNAME PICTURE X(8).                           02550000
070060 PROCEDURE DIVISION.                                              02560000
070070 GO-BABY-GO.                                                      02570000
070090      ENTRY 'DLITCBL' USING TERM-NAL, DATABASE.                   02580000
070105     MOVE GET-UNIQUE      TO FILL-FUNCTION.                       02590000
070106     MOVE SPACES TO INPUT-TEXT.                                   02600000
070120      CALL 'CBLTDLI' USING GET-UNIQUE, TERM-NAL, LINE-INPUT.      02610000
070140     IF TERM-STATUS NOT = '  ' GO TO ERROR-HANDLER.               02620000
070150     IF FIRST-6 = 'ADDPN ' GO TO ADDPN-RTN.                       02630000
070151     IF FIRST-6 = 'ADDPAR' GO TO ADDPN-RTN.                       02640000
070160 LP.                                                              02650000
070170      CALL INPANAL USING ALL-OTHERS-PARAM-TABLE, LINE-INPUT,      02660000
070180                EDITED-ALL-OTHERS-MSG, MSG-SEG-CNT.               02670000
070190     ADD 1 TO MSG-SEG-CNT.                                        02680000
070200 GN.                                                              02690000
070210     MOVE GET-NEXT        TO FILL-FUNCTION.                       02700000
070220     MOVE SPACES TO INPUT-TEXT.                                   02710000
080020      CALL 'CBLTDLI' USING GET-NEXT, TERM-NAL, LINE-INPUT.        02720000
080035 GN-1. EXIT.                                                      02730000
080040 EX. IF TERM-STATUS = '  ' GO TO LP.                              02740000
080050     IF TERM-STATUS NOT = END-OF-MSG GO TO ERROR-HANDLER.         02750000
080060     MOVE 0 TO MSG-SEG-CNT.                                       02760000
080065     MOVE SPACES TO INPUT-TEXT.                                   02770000
080080      CALL INPANAL USING ALL-OTHERS-PARAM-TABLE, LINE-INPUT,      02780000
080090                            EDITED-ALL-OTHERS-MSG, MSG-SEG-CNT.   02790000
080110     MOVE 1 TO MSG-SEG-CNT.                                       02800000
080120     GO TO BRANCH-TABLE.                                          02810000
080130 ADDPN-RTN.                                                       02820000
080150      CALL INPANAL USING ADD-PART-NO-PARAM-TABLE, LINE-INPUT,     02830000
080160                            EDITED-ADDPN-MSG, MSG-SEG-CNT.        02840000
080180     ADD 1 TO MSG-SEG-CNT.                                        02850000
080190     PERFORM GN.                                                  02860000
080200     IF TERM-STATUS = '  ' GO TO ADDPN-RTN.                       02870000
090010     IF TERM-STATUS NOT = END-OF-MSG GO TO ERROR-HANDLER.         02880000
090020     MOVE 0 TO MSG-SEG-CNT.                                       02890000
090025     MOVE SPACES TO INPUT-TEXT.                                   02900000
090040      CALL INPANAL USING ADD-PART-NO-PARAM-TABLE, LINE-INPUT,     02910000
090050                            EDITED-ADDPN-MSG, MSG-SEG-CNT.        02920000
090070     MOVE 1 TO MSG-SEG-CNT.  MOVE PART-NO TO PART-NO-ENTRY.       02930000
090080 PART-NO-EDIT.                                                    02940000
090083      CALL PNEDIT USING PART-NO-EDIT-TABLE.                       02950000
090090     IF PART-NO-REJECT NOT = ' ' GO TO REJECT-MSG.                02960000
090100 RTN-TO-ADDPN.                                                    02970000
090110     MOVE PART-NO-ENTRY TO PART-NO.                               02980000
090115     IF PART-NO =  SPACES GO TO INVALID-PART.                     02990000
090120     MOVE PART-NO TO ROOT-KEY OF ROOT-FORMAT.                     03000000
090130     MOVE DESCRPTN TO ROOT-DESCR.                                 03010000
090140     MOVE PROC-CODE TO STAN-PROC-CODE.                            03020000
090150     MOVE PART-NO TO VAR-PART.  MOVE SPACE TO PARTLEFT.           03030000
090155     MOVE IN-SERT            TO FILL-FUNCTION.                    03040000
090170     CALL 'CBLTDLI' USING IN-SERT,                                03050000
090175                                DATABASE, ROOT-FORMAT, PARTNO-SSA.03060000
090180     MOVE '(' TO PARTLEFT.                                        03070000
090190     IF DBASE-STATUS NOT = '  ' GO TO ADD-PN-ERROR.               03080000
090191     MOVE SPACE TO STANINFO-LEFT.                                 03090000
090193     CALL 'CBLTDLI' USING IN-SERT, DATABASE, STANINFO-FORMAT,     03100000
090194                             PARTNO-SSA, STANINFO-SSA.            03110000
090196     MOVE '(' TO STANINFO-LEFT.                                   03120000
090197     IF DBASE-STATUS NOT = '  ' GO TO DBASE-ERROR.                03130000
090200     MOVE PART-NO TO FILL-PART-4.                                 03140000
100010     MOVE CARR-RETURN TO CR-6.                                    03150000
100014     MOVE PART-ADD-SUCCESS TO OUTPUT-TEXT.                        03160000
100015     MOVE 52 TO OUTPUT-COUNT.                                     03170000
100016 TERM-OUT-RTN.                                                    03180000
100030     CALL 'CBLTDLI' USING IN-SERT, TERM-NAL, LINE-OUTPUT.         03190000
100045     MOVE IN-SERT TO FILL-FUNCTION.                               03200000
100050     IF TERM-STATUS NOT = '  ' GO TO ERROR-HANDLER.               03210000
100060 EXIT-ROUTINE.                                                    03220000
100080         GOBACK.                                                  03230000
100100 BRANCH-TABLE.                                                    03240000
100101     MOVE PART-NUM TO PART-NO-ENTRY.                              03250000
100103      CALL PNEDIT USING PART-NO-EDIT-TABLE.                       03260000
100105     IF PART-NO-REJECT NOT = ' ' GO TO REJECT-MSG.                03270000
100106     MOVE PART-NO-ENTRY TO PART-NUM.                              03280000
100110     IF TRANS-CODE = 'DLETPN  ' GO TO DELETE-PN-RTN.              03290000
100111     IF TRANS-CODE = 'DLETPART' GO TO DELETE-PN-RTN.              03300000
100120     IF TRANS-CODE = 'ADDI    ' GO TO ADD-INV-RTN.                03310000
100121     IF TRANS-CODE = 'ADDINV  ' GO TO ADD-INV-RTN.                03320000
100130 DELETE-INV-RTN.                                                  03330000
100140     MOVE PART-NUM TO VAR-PART.                                   03340000
110010     MOVE STOKSTAT-KEY TO VAR-STOK.                               03350000
110015     MOVE GET-HOLD-UNIQUE TO FILL-FUNCTION.                       03360000
110030      CALL 'CBLTDLI' USING GET-HOLD-UNIQUE, DATABASE,             03370000
110040             STOKSTAT-FORMAT, PARTNO-SSA, STOKSTAT-SSA.           03380000
110060     IF DBASE-STATUS NOT = '  ' GO TO NO-PART-ON-DEL-INV.         03390000
110070     IF TOTAL-STOCK NOT = ZERO GO TO NON-ZERO-STOCK-ERROR.        03400000
110075     MOVE DELEET             TO FILL-FUNCTION.                    03410000
110090     CALL 'CBLTDLI' USING DELEET, DATABASE, STOKSTAT-FORMAT.      03420000
110110     IF DBASE-STATUS NOT = '  ' GO TO DBASE-ERROR.                03430000
110120     MOVE X TO FILL-INV-3.                                        03440000
110130     MOVE PART-NUM TO FILL-PART-7.                                03450000
110140     MOVE CARR-RETURN TO CR-9.                                    03460000
110150     MOVE INV-DEL-SUCCESS TO OUTPUT-TEXT.                         03470000
110160     MOVE 65 TO OUTPUT-COUNT.                                     03480000
110170     GO TO TERM-OUT-RTN.                                          03490000
110180 NON-ZERO-STOCK-ERROR.                                            03500000
110190     MOVE X TO FILL-INV-4.                                        03510000
110200     MOVE CARR-RETURN TO CR-10.                                   03520000
110210     MOVE STOCK-BAL-ERROR TO OUTPUT-TEXT.                         03530000
110220     MOVE 62 TO OUTPUT-COUNT.                                     03540000
110230     GO TO TERM-OUT-RTN.                                          03550000
120150 INVALID-PART.                                                    03560000
120160     MOVE PART-NO TO FILL-PART-8.                                 03570000
120170     MOVE CARR-RETURN TO CR-11.                                   03580000
120180     MOVE INVALID-PN-MSG TO OUTPUT-TEXT.                          03590000
120190     MOVE 50 TO OUTPUT-COUNT.                                     03600000
120200     GO TO TERM-OUT-RTN.                                          03610000
130010 DELETE-PN-RTN.                                                   03620000
130020     MOVE PART-NUM TO VAR-PART.                                   03630000
130025     MOVE GET-UNIQUE      TO FILL-FUNCTION.                       03640000
130040      CALL 'CBLTDLI' USING GET-UNIQUE,      DATABASE,             03650000
130050                 ROOT-FORMAT, PARTNO-SSA.                         03660000
130070     IF DBASE-STATUS NOT = '  ' GO TO NO-PART-ON-DEL.             03670000
130071     MOVE SPACE TO STOKSTAT-LEFT.                                 03680000
130072     MOVE GET-NEXT TO FILL-FUNCTION.                              03690000
130073      CALL 'CBLTDLI' USING GET-NEXT,   DATABASE, STOKSTAT-FORMAT, 03700000
130074                 PARTNO-SSA, STOKSTAT-SSA.                        03710000
130075     MOVE '(' TO STOKSTAT-LEFT.                                   03720000
130076     IF DBASE-STATUS NOT = SEG-NOT-FOUND GO TO HAS-INV-ERR.       03730000
130077     MOVE GET-HOLD-UNIQUE TO FILL-FUNCTION.                       03740000
130078      CALL 'CBLTDLI' USING GET-HOLD-UNIQUE, DATABASE,             03750000
130079                         ROOT-FORMAT, PARTNO-SSA.                 03760000
130081     IF DBASE-STATUS NOT = '  ' GO TO DBASE-ERROR.                03770000
130085     MOVE DELEET             TO FILL-FUNCTION.                    03780000
130090     CALL 'CBLTDLI' USING DELEET, DATABASE, ROOT-FORMAT.          03790000
130110     IF DBASE-STATUS NOT = '  ' GO TO DBASE-ERROR.                03800000
130120     MOVE PART-NUM TO FILL-PART-6.                                03810000
130130     MOVE CARR-RETURN TO CR-8.                                    03820000
130140     MOVE PART-DEL-SUCCESS TO OUTPUT-TEXT.                        03830000
130150     MOVE 56 TO OUTPUT-COUNT.                                     03840000
130160     GO TO TERM-OUT-RTN.                                          03850000
130170 ADD-INV-RTN.                                                     03860000
130180     MOVE PART-NUM TO VAR-PART.                                   03870000
130190     MOVE STOKSTAT-KEY TO STOKSTATUS-KEY.                         03880000
130200     MOVE STOKSTAT-KEY TO VAR-STOK.                               03890000
130210     MOVE SPACE TO STOKSTAT-LEFT.                                 03900000
130230     MOVE IN-SERT            TO FILL-FUNCTION.                    03910000
140110     CALL 'CBLTDLI' USING IN-SERT, DATABASE, STOKSTAT-FORMAT,     03920000
140120                         PARTNO-SSA, STOKSTAT-SSA.                03930000
140140     MOVE '(' TO STOKSTAT-LEFT.                                   03940000
140150     IF DBASE-STATUS NOT = '  ' GO TO INV-THERE-BEFORE.           03950000
140160     MOVE X TO FILL-INV-2.                                        03960000
140170     MOVE PART-NUM TO FILL-PART-5.                                03970000
140180     MOVE CARR-RETURN TO CR-7.                                    03980000
140190     MOVE INV-ADD-SUCCESS TO OUTPUT-TEXT.                         03990000
140200     MOVE 61 TO OUTPUT-COUNT.                                     04000000
140210     GO TO TERM-OUT-RTN.                                          04010000
150130 HAS-INV-ERR.                                                     04020000
150140     IF DBASE-STATUS NOT = '  ' GO TO DBASE-ERROR.                04030000
150150     MOVE PART-NUM TO FILL-PART-10.                               04040000
150160     MOVE CARR-RETURN TO CR-13.                                   04050000
150170     MOVE HAS-AN-INV TO OUTPUT-TEXT.                              04060000
150180     MOVE 70 TO OUTPUT-COUNT.                                     04070000
150190     GO TO TERM-OUT-RTN.                                          04080000
160010 ERROR-HANDLER.                                                   04090000
160020     MOVE TERM-STATUS     TO FILL-STATUS.                         04100000
160023     IF ERROR-SWITCH NOT = ' ' DISPLAY STATUS-CODE-MSG UPON       04110000
160024         CONSOLE, GO TO EXIT-ROUTINE.                             04120000
160025     MOVE 'E' TO ERROR-SWITCH.                                    04130000
160030     GO TO COMMON-ERROR.                                          04140000
160040 DBASE-ERROR.                                                     04150000
160050     MOVE DBASE-STATUS TO FILL-STATUS.                            04160000
160060 COMMON-ERROR.                                                    04170000
160070     MOVE CARR-RETURN TO CR-2.                                    04180000
160080     MOVE STATUS-CODE-MSG TO OUTPUT-TEXT.                         04190000
160090     MOVE 39 TO OUTPUT-COUNT.                                     04200000
160100     GO TO TERM-OUT-RTN.                                          04210000
160110 REJECT-MSG.                                                      04220000
160120     MOVE PART-NO-REJECT TO REJECT-CODE.                          04230000
160130     MOVE CARR-RETURN TO CR-1.                                    04240000
160140     MOVE PART-REJECTED-MSG TO OUTPUT-TEXT.                       04250000
160150     MOVE 31 TO OUTPUT-COUNT.                                     04260000
160160     GO TO TERM-OUT-RTN.                                          04270000
160170 ADD-PN-ERROR.                                                    04280000
160175     IF DBASE-STATUS NOT = DUPL-SEGMENT GO TO DBASE-ERROR.        04290000
160180     MOVE PART-NO TO FILL-PART.                                   04300000
160190     MOVE CARR-RETURN TO CR-3.                                    04310000
160200     MOVE PART-ALREADY-THERE TO OUTPUT-TEXT.                      04320000
160210     MOVE 53 TO OUTPUT-COUNT.                                     04330000
160220     GO TO TERM-OUT-RTN.                                          04340000
170010 NO-PART-ON-DEL-INV.                                              04350000
170020     IF DBASE-STATUS NOT = SEG-NOT-FOUND GO TO DBASE-ERROR.       04360000
170030     IF SEGMENT-INDR LESS THAN '01' GO TO STEP-BELOW.             04370000
170040     MOVE PART-NUM TO FILL-PART-3.                                04380000
170050     MOVE AREAX TO FILL-INV.                                      04390000
170060     MOVE CARR-RETURN TO CR-5.                                    04400000
170070     MOVE INV-NOT-FOUND TO OUTPUT-TEXT.                           04410000
170080     MOVE 86 TO OUTPUT-COUNT.                                     04420000
170090     GO TO TERM-OUT-RTN.                                          04430000
170100 STEP-BELOW.                                                      04440000
170110     MOVE PART-NUM TO FILL-PART-2.                                04450000
170120     MOVE DEL-INV-MSG TO FILL-ERR-1.                              04460000
170130     MOVE CARR-RETURN TO CR-4.                                    04470000
170140     MOVE PART-NOT-FOUND TO OUTPUT-TEXT.                          04480000
170150     MOVE 67 TO OUTPUT-COUNT.                                     04490000
170160     GO TO TERM-OUT-RTN.                                          04500000
170170 NO-PART-ON-DEL.                                                  04510000
170180     IF DBASE-STATUS NOT = SEG-NOT-FOUND GO TO DBASE-ERROR.       04520000
170190     MOVE PART-NUM TO FILL-PART-2.                                04530000
170200     MOVE DELETE-MSG TO FILL-ERR-1.                               04540000
170210     MOVE CARR-RETURN TO CR-4.                                    04550000
170220     MOVE PART-NOT-FOUND TO OUTPUT-TEXT.                          04560000
170230     MOVE 67 TO OUTPUT-COUNT.                                     04570000
170240     GO TO TERM-OUT-RTN.                                          04580000
180010 INV-THERE-BEFORE.                                                04590000
180020     IF DBASE-STATUS = NO-ROOT-ISRT GO TO NO-ROOT-FND-ON-INV-ISRT.04600000
180030     IF DBASE-STATUS NOT = DUPL-SEGMENT GO TO DBASE-ERROR.        04610000
180040     MOVE AREAX TO FILL-INV-5.                                    04620000
180050     MOVE PART-NUM TO FILL-PART-9.                                04630000
180060     MOVE CARR-RETURN TO CR-12.                                   04640000
180070     MOVE INV-ALREADY-THERE-MSG TO OUTPUT-TEXT.                   04650000
180080     MOVE 72 TO OUTPUT-COUNT.                                     04660000
180090     GO TO TERM-OUT-RTN.                                          04670000
180100 NO-ROOT-FND-ON-INV-ISRT.                                         04680000
180110     MOVE PART-NUM TO FILL-PART-2.                                04690000
180120     MOVE ADD-INV-MSG TO FILL-ERR-1.                              04700000
180130     MOVE CARR-RETURN TO CR-4.                                    04710000
180140     MOVE PART-NOT-FOUND TO OUTPUT-TEXT.                          04720000
180150     MOVE 67 TO OUTPUT-COUNT.                                     04730000
180160     GO TO TERM-OUT-RTN.                                          04740000