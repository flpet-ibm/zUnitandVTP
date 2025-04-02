BMPDLI   TITLE 'INSTALLATION VERIFICATION PROCEDURE - BMP/DLI'          00010000
DFSIVA6  CSECT                                                          00020000
        SPACE 1                                                         00030000
**************************** INSTALL/IVP ****************************** 00040000
*                                                                     * 00050000
*                   M O D U L E    P R O L O G U E                    * 00060000
*                                                                     * 00070000
*********************************************************************** 00080000
*                                                                     * 00090000
*          NAME:  DFSIVA6                                             * 00100000
*                                                                     * 00110000
*   DESCRIPTION:  BATCH AND BMP PROGRAM FOR HIDAM/OSAM                * 00120000
*                                                                     * 00130000
**************************************************************@SCPYRT** 00140000
*                                                                     * 00150000
*  Licensed Materials - Property of IBM                               * 00160000
*                                                                     * 00170000
*  5635-A06                                                           * 00180000
*                                                                     * 00190000
*      Copyright IBM Corp. 1974,1998 All Rights Reserved.             * 00200000
*                                                                     * 00210000
*  US Government Users Restricted Rights - Use, duplication or        * 00220000
*  disclosure restricted by GSA ADP Schedule Contract with            * 00230000
*  IBM Corp.                                                          * 00240000
**************************************************************@ECPYRT** 00250000
*                                                                     * 00260000
*        STATUS:  IMS 3.1                                             * 00270000
*                                                                     * 00280000
*   MODULE TYPE:  ASSEMBLER                                           * 00290000
*                                                                     * 00300000
*       CHANGES:  PROLOGUE ADDED AND MINOR CHANGES FOR 3.1            * 00310000
*                                                                     * 00320000
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7* 00330000
*---------------------------------------------------------------------* 00340000
*                                                                     * 00350000
*  TRANSACTION  :  NONE (BMP/DLI)                                     * 00360000
*                                                                     * 00370000
*  PSB          :  DFSIVP6                                            * 00380000
*                                                                     * 00390000
*  DATABASE     :  DFSIVD1                                            * 00400000
*                                                                     * 00410000
*  INPUT:                                                             * 00420000
*                                                                     * 00430000
*         TELEPHONE DIRECTORY SYSTEM                                  * 00440000
*         PROCESS CODE : CCCCCCC                                      * 00450000
*         LAST NAME  : XXXXXXXXXX                                     * 00460000
*         FIRST NAME   : XXXXXXXXXX                                   * 00470000
*         EXTENTION#   : N-NNN-NNNN                                   * 00480000
*         INTERNAL ZIP : XXX/XXX                                      * 00490000
*                                                                     * 00500000
*  CCCCCC = COMMAND                                                   * 00510000
*         ADD = INSERT ENTRY IN DB                                    * 00520000
*         DELETE = DELETE ENTRY FROM DB                               * 00530000
*         UPDATE = UPDATE ENTRY FROM DB                               * 00540000
*         DISPLAY = DISPLAY ENTRY                                     * 00550000
*         TADD = SAME AS ADD, BUT WITH WTO                            * 00560000
*                                                                     * 00570000
*---------------------------------------------------------------------* 00580000
         SPACE 2                                                        00590000
*---------------------------------------------------------------------* 00600000
*        EQUATE REGISTERS AND USAGE OF REGISTERS                      * 00610000
*---------------------------------------------------------------------* 00620000
         SPACE 1                                                        00630000
R0       EQU   0                                                        00640000
R1       EQU   1              ORIGINAL PCBLIST ADDRESS                  00650000
R2       EQU   2                                                        00660000
R3       EQU   3                                                        00670000
R4       EQU   4                                                        00680000
R5       EQU   5                                                        00690000
R6       EQU   6                                                        00700000
R7       EQU   7                                                        00710000
R8       EQU   8              I/O OR DB PCB ADDRESS                     00720000
R9       EQU   9              BAL REGISTER                              00730000
R10      EQU   10                                                       00740000
R11      EQU   11             SECOND BASE REGISTER                      00750000
R12      EQU   12             FIRST BASE REGISTER                       00760000
R13      EQU   13             SAVE AREA ADDRESS                         00770000
R14      EQU   14                                                       00780000
R15      EQU   15                                                       00790000
         EJECT                                                          00800000
*---------------------------------------------------------------------* 00810000
*      HOUSE  KEEPING                                                 * 00820000
*---------------------------------------------------------------------* 00830000
         SPACE 1                                                        00840000
         USING DFSIVA6,R12,R11 BASE REGISTER SETUP                      00850000
         SAVE  (14,12)        SAVE REGISTER                             00860000
         LR    R12,R15        LOAD FIRST BASE REGISTER                  00870000
         LA    R11,2048(,R12)                                           00880000
         LA    R11,2048(,R11) GET SECOND BASE                           00890000
         ST    R13,SAVEAREA+4 SET BACKWARD POINTER                      00900000
         LA    R2,SAVEAREA    NEW SAVEAREA                              00910000
         ST    R2,8(,R13)     FORWARD CHAIN                             00920000
         LR    R13,R2                                                   00930000
         MVI   SW1,X'00'      INITIALIZE SW1                            00940000
         SPACE 1                                                        00950000
*---------------------------------------------------------------------* 00960000
*      SAVE  PCB  ADDRESSES                                           * 00970000
*---------------------------------------------------------------------* 00980000
         SPACE 1                                                        00990000
         ST    R1,PCBALST     SAVE PCB ADDRESS LIST                     01000000
         USING PCBNAME,R8     MAP PCB DB,INPUT, AND OUTPUT              01010000
         L     R8,0(,R1)      GET FIRST ENTRY                           01020000
         LA    R8,0(,R8)      MAKE SURE HIGHEST BIT IS OFF              01030000
         ST    R8,IOPCB       SAVE IO PCB ADDRESS                       01040000
         L     R8,4(,R1)      GET SECOND ENTRY                          01050000
         LA    R8,0(,R8)      MAKE SURE HIGHEST BIT IS OFF              01060000
         ST    R8,DBPCB       SAVE DB PCB ADDRESS                       01070000
         L     R8,8(,R1)      GET THIRD ENTRY                           01080000
         LA    R8,0(,R8)      MAKE SURE HIGHEST BIT IS OFF              01090000
         ST    R8,GIPCB       SAVE GSAM INPUT PCB ADDRESS               01100000
         L     R8,12(,R1)     GET THIRD ENTRY                           01110000
         LA    R8,0(,R8)      MAKE SURE HIGHEST BIT IS OFF              01120000
         ST    R8,GOPCB       SAVE GSAM OUTPUT PCB ADDRESS              01130000
         SPACE 1                                                        01140000
*---------------------------------------------------------------------* 01150000
*      OBTAIN DATE                                                    * 01160000
*---------------------------------------------------------------------* 01170000
         SPACE 1                                                        01180000
         TIME  ,               GET DATE                                 01190000
         ST    R1,PDATE           SAVE THE PACKED DATE                  01200000
         SRL   R1,SIXTEEN         LEAVE YY PART OF DATE                 01210000
         SLL   R1,FOUR            GET 4 BITS FOR SIGN                   01220000
         O     R1,F12             SET SIGN OF 'C'                       01230000
         STH   R1,PEDIT           PUT IT IN WORK SLOT                   01240000
         UNPK  PRNTYR,PEDIT       CONVERT YY TO ZONED CHARS             01250000
         OI    PRNTYR1,XF0        CHANGE SIGN TO ZONED                  01260000
         CVB   R1,CVBWORK         CONVERT YYS TO BINARY                 01270000
         ST    R1,CVBRIGHT                                              01280000
         LA    R3,TABDATE1        POINT TO TABLE FOR LEAP YEAR          01290000
         TM    PEDIT1,X03         DETERMINE IF LEAP YEAR                01300000
         BZ    GETMDD             YES, LAST 2 BITS OFF                  01310000
         LA    R3,TABDATE         NO, TABLE FOR NOT LEAP YEAR           01320000
GETMDD   EQU   *                                                        01330000
*        THE FOLLOWING ROUTINE CALCULATES MM AND DD FROM DDD.           01340000
         L     R1,PDATE           GET BACK 00YYDDDS                     01350000
         N     R1,FW64K           MASK OUT DDDS                         01360000
         STH   R1,PEDIT           SAVE IT FOR COMPARE                   01370000
         LA    R2,ONE             INITIALIZE R2 TO FIRST MONTH          01380000
GETDLOOP CP    PEDIT,DISP2(TLENTH,R3)     COMPARE INPUT DDD TO NEXT     01390000
*                                 TABLE ENTRY                           01400000
         BH    INCR               INPUT DDD HIGHER-SEARCH FURTHER       01410000
*        INPUT DDD IS EQUAL OR LESS THEN THE NEXT ENTRY IN TABLE        01420000
         SP    PEDIT,DISP0(TLENTH,R3)    DAY OF THE MONTH =             01430000
*                             INPUT DDD - CURRENT TAB ENTRY DDD.        01440000
         UNPK  PRNTDAY,PEDIT  CHANGE DD TO ZONED                        01450000
         OI    PRNTDAY1,XF0   CHANGE SIGN TO ZONED                      01460000
         CVD   R2,CVBWORK     CONVERT MM TO PACKED DEC.                 01470000
         UNPK  PRNTMO,PEDIT   CONVERT MM TO ZONED                       01480000
         OI    PRNTMO1,XF0    CHANGE SIGN TO ZONED                      01490000
         B     READIN                                                   01500000
INCR     DS    0H                                                       01510000
         LA    R2,ONE(R2)     INCREMENT MM                              01520000
         LA    R3,DISP2(R3)                                             01530000
         CH    R2,H12         IF MM > 12                                01540000
         BH    ZDATE          BAD INPUT DATE                            01550000
         B     GETDLOOP                                                 01560000
ZDATE    DS    0H                                                       01570000
         MVC   DATEPTRN,ZERODATE  PUT IN A ZERO DATE                    01580000
         SPACE 1                                                        01590000
*---------------------------------------------------------------------* 01600000
*      OBTAIN INPUT  DATA                                             * 01610000
*---------------------------------------------------------------------* 01620000
         SPACE 1                                                        01630000
READIN   DS    0H                                                       01640000
         MVC   GSECALL,GN     SAVE CALL TYPE                            01650000
         L     R8,GIPCB       POINT INPUT PCB                           01660000
         CALL  ASMTDLI,(GN,(R8),INPUT),VL  GET INPUT MESSAGE            01670000
         CLC   STATUS,BLANK   CHECK STATUS BYTES                        01680000
         BE    PROGMSG        INPUT READ                                01690000
         CLC   STATUS,=CL2'GB'  END OF INPUT?                           01700000
         BNE   GSERR          ELSE, SOMETHING IS WRONG...               01710000
         B     FINISH                                                   01720000
         SPACE 1                                                        01730000
*---------------------------------------------------------------------* 01740000
*      CLEAR OUTPUT AREA                                              * 01750000
*---------------------------------------------------------------------* 01760000
         SPACE 1                                                        01770000
PROGMSG  DS    0H                                                       01780000
         MVC   OUTPUT,BLANK   CLEAR OUTPUT AREA                         01790000
         MVC   IODATA,BLANK   CLEAR I/O WORK AREA                       01800000
         SPACE 1                                                        01810000
*---------------------------------------------------------------------* 01820000
*      DELETE  LEADING  BLANK  FROM  THE  INPUT  DATA                 * 01830000
*---------------------------------------------------------------------* 01840000
         SPACE 1                                                        01850000
         LA    R5,INCMD       POINT PROCESS REQUEST CODE FIELD          01860000
         LA    R14,L'INCMD    SET MAX LOOP COUNTER                      01870000
         BAL   R9,SHIFT1      STRIP BLANK OFF                           01880000
         LA    R5,INNAME1     POINT LAST NAME FIELD                     01890000
         LA    R14,L'INNAME1  SET MAX LOOP COUNTER                      01900000
         BAL   R9,SHIFT1      STRIP BLANK OFF                           01910000
         LA    R5,INNAME2     POINT LAST NAME FIELD                     01920000
         LA    R14,L'INNAME2  SET MAX LOOP COUNTER                      01930000
         BAL   R9,SHIFT1      STRIP BLANK OFF                           01940000
         LA    R5,INEXT#      POINT EXTENTION# FIELD                    01950000
         LA    R14,L'INEXT#   SET MAX LOOP COUNTER                      01960000
         BAL   R9,SHIFT1      STRIP BLANK OFF                           01970000
         LA    R5,INZIP       POINT INTERNAL ZIP CODE FIELD             01980000
         LA    R14,L'INZIP    SET MAX LOOP COUNTER                      01990000
         BAL   R9,SHIFT1      STRIP BLANK OFF                           02000000
         MVC   IONAME1,INNAME1 SET LAST NAME                            02010000
         MVC   IOCMD,INCMD    SET REQUEST CODE                          02020000
         SPACE 1                                                        02030000
*---------------------------------------------------------------------* 02040000
*      CHECK  FOR PROCESS CODE AND LAST NAME SPECIFIED                * 02050000
*---------------------------------------------------------------------* 02060000
         SPACE 1                                                        02070000
         CLI   IOCMD,C' '     IF PROCESS CODE SPECIFIED?                02080000
         BE    INVREQ1        ...ELSE                                   02090000
         CLI   IONAME1,C' '   IF NAME SPECIFIED ?                       02100000
         BE    NONAME         ...ELSE                                   02110000
         SPACE 1                                                        02120000
*---------------------------------------------------------------------* 02130000
*      ROUTE TO REQUEST HANDLER                                       * 02140000
*---------------------------------------------------------------------* 02150000
         SPACE 1                                                        02160000
         CLC   KADD,IOCMD     IF COMMAND ADD ENTERED ?                  02170000
         BE    TOADD          ...THEN, GOTO INSERT ENTRY                02180000
         CLC   KUPD,IOCMD     IF COMMAND UPDATE ENTERED ?               02190000
         BE    TOUPD          ...THEN, GOTO UPDATE ENTRY                02200000
         CLC   KDEL,IOCMD     IF COMMAND DEL ENTERED ?                  02210000
         BE    TODEL          ...THEN, GOTO DELETE ENTRY                02220000
         CLC   KDIS,IOCMD     IF COMMAND DIS ENTERED ?                  02230000
         BE    TODIS          ...THEN, GOTO DISPLAY ENTRY               02240000
         CLC   KTAD,IOCMD     IF TEST ADD WITH REPLY ?                  02250000
         BE    TOTAD          ...THEN,                                  02260000
         MVC   OUTCMD,INCMD   SETUP OUTPUT AREA                         02270000
         MVC   OUTNAME1,INNAME1   FOR ERROR MESSAGE                     02280000
         B     INVREQ1        INVALID REQUEST                           02290000
         SPACE 1                                                        02300000
SHIFT1   DS    0H                                                       02310000
         BCTR  R14,0                                                    02320000
         LR    R15,R14           SET LOOP COUNT                         02330000
         BCTR  R14,0             SET MVC LENGTH VALUE FOR 'EX' INST.    02340000
SHIFT2   DS    0H                                                       02350000
         CLI   0(R5),C' '        IF BLANK                               02360000
         BNER  R9                ...ELSE, RETURN TO CALLER              02370000
         EX    R14,MVC1          DROP 1ST LEADING BYTE                  02380000
         BCT   R15,SHIFT2        LOOP                                   02390000
         BR    R9                RETURN TO CALLER                       02400000
         SPACE 1                                                        02410000
MVC1     MVC   0(R14-R14,R5),1(R5)  DROP 1ST LEADING BYTE               02420000
         SPACE 1                                                        02430000
*---------------------------------------------------------------------* 02440000
*      ADDITION  REQUEST  HANDLER                                     * 02450000
*---------------------------------------------------------------------* 02460000
         SPACE 1                                                        02470000
TOTAD    DS    0H                                                       02480000
         OI    SW1,SW1TADD       INDICATE TADD IN PROGRESS              02490000
TOADD    DS    0H                                                       02500000
         MVC   IONAME2,INNAME2   SET LAST NAME                          02510000
         MVC   IOEXT#,INEXT#     SET EXTENTION#                         02520000
         MVC   IOZIP,INZIP       SET INTERNAL ZIP CODE                  02530000
TOADD4   DS    0H                                                       02540000
         MVC   OUTDATA1,IODATA1  SET OUTPUT DATA                        02550000
         MVC   OUTCMD,IOCMD      SET OUTPUT DATA (PROCESS REQUEST CODE) 02560000
         CLI   IONAME2,C' '      IF FIRST NAME SPECIFIED ?              02570000
         BE    MOREDATA          ...ELSE, TREAT AS 'NOT ENOUGH'         02580000
         CLI   IOEXT#,C' '       IF EXTENTION# SPECIFIED ?              02590000
         BE    MOREDATA          ...ELSE, TREAT AS 'NOT ENOUGH'         02600000
         CLI   IOZIP,C' '        IF INTERNAL ZIP CODE SPECIFIED ?       02610000
         BE    MOREDATA          ...ELSE, TREAT AS 'NOT ENOUGH'         02620000
INSDAT   DS    0H                                                       02630000
         BAL   R9,ISRTDB                                                02640000
         B     ERROR02        CORRECT COUNT BY ERROR                    02650000
         TM    SW1,SW1TADD    TADD IN PROGRESS?                         02660000
         BNO   ANTW1          ELSE, DON'T BOTHER                        02670000
         XC    ECB1,ECB1      CLEAR ECB                                 02680000
         WTOR  'INSERT IS DONE, REPLY',REPLY,16,ECB1                    02690000
         WAIT  ECB=ECB1                                                 02700000
         SPACE 1                                                        02710000
ANTW1    DS    0H                                                       02720000
         MVC   OUTMSG,MADD    WRITE ANSWER TO TERMINAL                  02730000
         B     RETURN                                                   02740000
         SPACE 1                                                        02750000
MOREDATA DS    0H                                                       02760000
         MVC   OUTMSG,MMOR    REQUEST MORE DATA                         02770000
         B     RETURN                                                   02780000
         SPACE 1                                                        02790000
*---------------------------------------------------------------------* 02800000
*      UPDATE  REQUEST  HANDLER                                       * 02810000
*---------------------------------------------------------------------* 02820000
         SPACE 1                                                        02830000
TOUPD    DS    0H                                                       02840000
         MVC   OUTCMD,INCMD   SETUP OUTPUT AREA                         02850000
         MVC   OUTNAME1,INNAME1   FOR ERROR MESSAGE                     02860000
         MVC   SSAKEY,IONAME1 SET SSA KEY USING LAST NAME               02870000
         BAL   R9,GHUDB       GHU SEGMENT                               02880000
         B     ERROR01                                                  02890000
         CLI   INNAME2,C' '   IF ANY DATA SPECIFIED ?                   02900000
         BE    TOUPD1         ...ELSE,                                  02910000
         OI    SW1,SW1DATA    SET DATA TYPED IN                         02920000
         MVC   IONAME2,INNAME2  UPDATE ENTRY                            02930000
TOUPD1   DS    0H                                                       02940000
         CLI   INEXT#,C' '    IF ANY DATA SPECIFIED ?                   02950000
         BE    TOUPD2         ...ELSE,                                  02960000
         OI    SW1,SW1DATA    SET DATA TYPED IN                         02970000
         MVC   IOEXT#,INEXT#  UPDATE ENTRY                              02980000
TOUPD2   DS    0H                                                       02990000
         CLI   INZIP,C' '     IF ANY DATA SPECIFIED ?                   03000000
         BE    TOUPD3         ...ELSE,                                  03010000
         OI    SW1,SW1DATA    SET DATA TYPED IN                         03020000
         MVC   IOZIP,INZIP    UPDATE ENTRY                              03030000
TOUPD3   DS    0H                                                       03040000
         MVC   OUTDATA1,IODATA1  SET OUTPUT DATA FIELDS                 03050000
         MVC   OUTCMD,IOCMD      SET REQUEST CODE                       03060000
         TM    SW1,SW1DATA    IF ANY FIELD UPDATED ?                    03070000
         BE    TOUPDT4        ...ELSE,                                  03080000
         BAL   R9,REPLDB      UPDATE SEGMENT                            03090000
         B     ERROR03                                                  03100000
         MVC   OUTMSG,MUPD1                                             03110000
         B     RETURN                                                   03120000
TOUPDT4  DS    0H                                                       03130000
         MVC   OUTMSG,MNODATA                                           03140000
         B     RETURN                                                   03150000
         SPACE 1                                                        03160000
*---------------------------------------------------------------------* 03170000
*      DELETE  REQUEST  HANDLER                                       * 03180000
*---------------------------------------------------------------------* 03190000
         SPACE 1                                                        03200000
TODEL    DS    0H                                                       03210000
         MVC   OUTCMD,INCMD   SETUP OUTPUT AREA                         03220000
         MVC   OUTNAME1,INNAME1   FOR ERROR MESSAGE                     03230000
         MVC   SSAKEY,IONAME1  SET SSA KEY USING LAST NAME              03240000
         BAL   R9,GHUDB       GHU SEGMENT                               03250000
         B     ERROR01                                                  03260000
         MVC   OUTDATA1,IODATA1  SET OUTPUT DATA FIELDS                 03270000
         MVC   OUTCMD,IOCMD      SET REQUEST CODE                       03280000
         BAL   R9,DLETDB      DELET SEGMENT                             03290000
         B     ERROR04                                                  03300000
         MVC   OUTMSG,MDEL                                              03310000
         B     RETURN                                                   03320000
         SPACE 1                                                        03330000
*---------------------------------------------------------------------* 03340000
*      DISPLAY  REQUEST  HANDLER                                      * 03350000
*---------------------------------------------------------------------* 03360000
         SPACE 1                                                        03370000
TODIS    DS    0H                                                       03380000
         MVC   OUTCMD,INCMD   SETUP OUTPUT AREA                         03390000
         MVC   OUTNAME1,INNAME1   FOR ERROR MESSAGE                     03400000
         MVC   SSAKEY,IONAME1  SET SSA KEY USING LAST NAME              03410000
         BAL   R9,GUDB         GU SEGMENT                               03420000
         B     ERROR00                                                  03430000
         MVC   OUTDATA1,IODATA1  SET OUTPUT DATA FIELDS                 03440000
         MVC   OUTCMD,IOCMD      SET REQUEST CODE                       03450000
         MVC   OUTMSG,MDIS                                              03460000
         B     RETURN         WRITE ANSWER TO TERMINAL                  03470000
         SPACE 1                                                        03480000
*---------------------------------------------------------------------* 03490000
*      DATA BASE SEGMENT READ (GET UNIQUE) REQUEST HANDLER            * 03500000
*---------------------------------------------------------------------* 03510000
         SPACE 1                                                        03520000
GUDB     DS    0H                                                       03530000
         MVC   ECALL,GU       SAVE CALL TYPE                            03540000
         L     R8,DBPCB       POINT DBPCB                               03550000
         CALL  ASMTDLI,(GU,(R8),IODATA,SSA),VL                          03560000
         MVC   PCBM,0(R8)                                               03570000
         CLC   STATUS,BLANK   ANY ERROR                                 03580000
         BNER  R9             YES                                       03590000
         B     4(,R9)         RETURN                                    03600000
         SPACE 1                                                        03610000
*---------------------------------------------------------------------* 03620000
*      DATA BASE SEGMENT READ (GET HOLD UNIQUE) REQUEST HANDLER       * 03630000
*---------------------------------------------------------------------* 03640000
         SPACE 1                                                        03650000
GHUDB    DS    0H                                                       03660000
         MVC   ECALL,GHU      SAVE CALL TYPE                            03670000
         L     R8,DBPCB       POINT DBPCB                               03680000
         CALL  ASMTDLI,(GHU,(R8),IODATA,SSA),VL                         03690000
         MVC   PCBM,0(R8)                                               03700000
         CLC   STATUS,BLANK   ANY ERROR                                 03710000
         BNER  R9             YES                                       03720000
         B     4(,R9)         RETURN                                    03730000
         SPACE 1                                                        03740000
*---------------------------------------------------------------------* 03750000
*      DATA BASE SEGMENT READ (GET HOLD NEXT) REQUEST HANDLER         * 03760000
*---------------------------------------------------------------------* 03770000
         SPACE 1                                                        03780000
GHNDB    DS    0H                                                       03790000
         MVC   ECALL,GHN      SAVE CALL TYPE                            03800000
         L     R8,DBPCB       POINT DBPCB                               03810000
         CALL  ASMTDLI,(GHN,(R8),IODATA),VL                             03820000
         MVC   PCBM,0(R8)                                               03830000
         CLC   STATUS,BLANK   ANY ERROR                                 03840000
         BNER  R9             YES                                       03850000
         B     4(,R9)         RETURN                                    03860000
         SPACE 1                                                        03870000
*---------------------------------------------------------------------* 03880000
*      DATA BASE SEGMENT UPDATE REQUEST HANDLER                       * 03890000
*---------------------------------------------------------------------* 03900000
         SPACE 1                                                        03910000
REPLDB   DS    0H                                                       03920000
         MVC   ECALL,REPL     SAVE CALL TYPE                            03930000
         L     R8,DBPCB       POINT DBPCB                               03940000
         CALL  ASMTDLI,(REPL,(R8),IODATA),VL                            03950000
         MVC   PCBM,0(R8)                                               03960000
         CLC   STATUS,BLANK   ANY ERROR                                 03970000
         BNER  R9             YES                                       03980000
         B     4(,R9)         RETURN                                    03990000
         SPACE 1                                                        04000000
*---------------------------------------------------------------------* 04010000
*      DATA BASE SEGMENT INSERT REQUEST HANDLER                       * 04020000
*---------------------------------------------------------------------* 04030000
         SPACE 1                                                        04040000
ISRTDB   DS    0H                                                       04050000
         MVC   ECALL,ISRT     SAVE CALL TYPE                            04060000
         L     R8,DBPCB       POINT DBPCB                               04070000
         CALL  ASMTDLI,(ISRT,(R8),IODATA,SSA1),VL                       04080000
         MVC   PCBM,0(R8)                                               04090000
         CLC   STATUS,BLANK   ANY ERROR                                 04100000
         BNER  R9             YES                                       04110000
         B     4(,R9)         RETURN                                    04120000
         SPACE 1                                                        04130000
*---------------------------------------------------------------------* 04140000
*      DATA BASE SEGMENT DELETE REQUEST HANDLER                       * 04150000
*---------------------------------------------------------------------* 04160000
         SPACE 1                                                        04170000
DLETDB   DS    0H                                                       04180000
         MVC   ECALL,DLET     SAVE CALL TYPE                            04190000
         L     R8,DBPCB       POINT DBPCB                               04200000
         CALL  ASMTDLI,(DLET,(R8),IODATA),VL                            04210000
         MVC   PCBM,0(R8)                                               04220000
         CLC   STATUS,BLANK   ANY ERROR                                 04230000
         BNER  R9             YES                                       04240000
         B     4(,R9)         RETURN                                    04250000
         SPACE 1                                                        04260000
*---------------------------------------------------------------------* 04270000
*      PRINT OUTPUT                                                   * 04280000
*---------------------------------------------------------------------* 04290000
         SPACE 1                                                        04300000
RETURN   DS    0H                                                       04310000
         BAL   R9,ISRTIO      SEND MESSAGE TO GSAM OUTPUT PCB           04320000
         B     GSERR          WRITE ERROR TO OPERATOR                   04330000
         B     READIN         READ NEXT INPUT RECORD                    04340000
         SPACE 1                                                        04350000
*---------------------------------------------------------------------* 04360000
*      TERMINATION ROUTINE                                            * 04370000
*---------------------------------------------------------------------* 04380000
         SPACE 1                                                        04390000
FINISH   DS    0H                                                       04400000
         L     R13,4(,R13)    RESTORE SAVE AREA  ADDRESS                04410000
         LM    R14,R12,12(R13) RESTORE THE REGISTER                     04420000
         XR    R15,R15        RETURN-CODE ZERO                          04430000
         BR    R14            RETURN BACK                               04440000
         SPACE 1                                                        04450000
*---------------------------------------------------------------------* 04460000
*      FORMAT AND PRINT OUTPUT PAGE                                   * 04470000
*---------------------------------------------------------------------* 04480000
         SPACE 1                                                        04490000
ISRTIO   DS    0H                                                       04500000
         MVC   GSECALL,ISRT   SAVE CALL TYPE                            04510000
         L     R15,IOPSGNO    SEGMENT# ON IOPCB                         04520000
         LA    R15,1(,R15)    +1                                        04530000
         ST    R15,IOPSGNO    UPDATE SEGMENT#                           04540000
         CVD   R15,WORKD      CONVERT TO DECIMAL                        04550000
         UNPK  OUTSEGNO,WORKD+6(2) UNPACK                               04560000
         OI    OUTSEGNO+3,C'0' SET SIGN                                 04570000
         L     R8,GOPCB       POINT GSAM OUTPUT PCB                     04580000
         MVC   OUTLINE,BLANK  BLANK OUTPUT LINE                         04590000
         MVI   ANSI,C'1'      SET TO SKIP TO NEW PAGE                   04600000
         MVC   OUTLN1,BDRLN   SETUP TOP BORDER LINE                     04610000
         BAL   R10,PRTRTN     PRINT TOP BORDER LINE                     04620000
         BAL   R10,PRTRTN     PRINT 1ST LINE                            04630000
         MVC   HDRLN,HDRAST   SETUP 2ND LINE                            04640000
         BAL   R10,PRTRTN     PRINT 2ND LINE                            04650000
         MVC   HDRLN,HDRNM    SETUP 3RD LINE                            04660000
         BAL   R10,PRTRTN     PRINT 3RD LINE                            04670000
         MVC   HDRLN,HDRAST   SETUP 4TH LINE                            04680000
         BAL   R10,PRTRTN     PRINT 4TH LINE                            04690000
         BAL   R10,PRTRTN     PRINT 5TH LINE                            04700000
         BAL   R10,PRTRTN     PRINT 6TH LINE                            04710000
         MVC   D1LN,TRANTYP   SETUP 7TH LINE                            04720000
         BAL   R10,PRTRTN     PRINT 7TH LINE                            04730000
         MVC   D1CON,DATECON  SETUP                                     04740000
         MVC   D1VAR,DATEPTRN       8TH LINE                            04750000
         BAL   R10,PRTRTN     PRINT 8TH LINE                            04760000
         BAL   R10,PRTRTN     PRINT 9TH LINE                            04770000
         MVC   D2CON1,CON1    SETUP 10TH LINE                           04780000
         MVC   D2VAR(L'OUTCMD),OUTCMD                                   04790000
         BAL   R10,PRTRTN     PRINT 10TH LINE                           04800000
         MVC   D3CON,CON6     SETUP 11TH LINE                           04810000
         BAL   R10,PRTRTN     PRINT 11TH LINE                           04820000
         MVC   D2CON1,CON2    SETUP                                     04830000
         MVC   D2VAR,OUTNAME1       12TH                                04840000
         MVC   D2CON2,CON7               LINE                           04850000
         BAL   R10,PRTRTN     PRINT 12TH LINE                           04860000
         MVC   D2CON2,CON8    SETUP 13TH LINE                           04870000
         BAL   R10,PRTRTN     PRINT 13TH LINE                           04880000
         MVC   D2CON1,CON3    SETUP                                     04890000
         MVC   D2VAR,OUTNAME2       14TH                                04900000
         MVC   D2CON2,CON9               LINE                           04910000
         BAL   R10,PRTRTN     PRINT 14TH LINE                           04920000
         MVC   D2CON2,CON10   SETUP 15TH LINE                           04930000
         BAL   R10,PRTRTN     PRINT 15TH LINE                           04940000
         MVC   D2CON1,CON4    SETUP                                     04950000
         MVC   D2VAR,OUTEXT#        16TH                                04960000
         MVC   D2CON2,CON11              LINE                           04970000
         BAL   R10,PRTRTN     PRINT 16TH LINE                           04980000
         BAL   R10,PRTRTN     PRINT 17TH LINE                           04990000
         MVC   D2CON1,CON5    SETUP 18TH                                05000000
         MVC   D2VAR(L'OUTZIP),OUTZIP    LINE                           05010000
         BAL   R10,PRTRTN     PRINT 18TH LINE                           05020000
         BAL   R10,PRTRTN     PRINT 19TH LINE                           05030000
         BAL   R10,PRTRTN     PRINT 20TH LINE                           05040000
         MVC   D4VAR1,OUTMSG  SETUP                                     05050000
         MVC   D4CON,CON12          21ST                                05060000
         MVC   D4VAR2,OUTSEGNO           LINE                           05070000
         BAL   R10,PRTRTN     PRINT 21ST LINE                           05080000
         BAL   R10,PRTRTN     PRINT 22ND LINE                           05090000
         BAL   R10,PRTRTN     PRINT 23RD LINE                           05100000
         BAL   R10,PRTRTN     PRINT 24TH LINE                           05110000
         MVC   OUTLN1,BDRLN   SETUP BORDER LINE                         05120000
         BAL   R10,PRTRTN     PRINT BORDER LINE                         05130000
         B     4(,R9)         RETURN TO CALLER                          05140000
         SPACE 1                                                        05150000
*---------------------------------------------------------------------* 05160000
*        PRINT ROUTINE                                                * 05170000
*---------------------------------------------------------------------* 05180000
         SPACE 1                                                        05190000
PRTRTN   DS    0H                                                       05200000
         CALL  ASMTDLI,(ISRT,(R8),OUTLINE),VL                           05210000
         MVC   PCBM,0(R8)                                               05220000
         CLC   STATUS,BLANK   ANY ERROR                                 05230000
         BNER  R9             YES                                       05240000
         MVC   OUTLINE,BLANK  NO, BLANK OUTPUT LINE                     05250000
         MVI   LEDGE,C'|'     SETUP LEFT EDGE BORDER                    05260000
         MVI   REDGE,C'|'     SETUP RIGHT EDGE BORDER                   05270000
         BR    R10            RETURN                                    05280000
         SPACE 1                                                        05290000
         EJECT                                                          05300000
         TITLE 'IVP BMP/DLI PROGRAM  ERROR ROUTINES'                    05310000
         SPACE 1                                                        05320000
*---------------------------------------------------------------------* 05330000
*        WRITE ERROR-MESSAGES                                         * 05340000
*---------------------------------------------------------------------* 05350000
         SPACE 1                                                        05360000
NODATA   DS    0H                                                       05370000
         MVC   OUTMSG,MNODATA SET ERROR DATA                            05380000
         B     RETURN                                                   05390000
         SPACE 1                                                        05400000
NONAME   DS    0H                                                       05410000
         MVC   OUTMSG,MNONAME SET ERROR DATA                            05420000
         B     RETURN                                                   05430000
         SPACE 1                                                        05440000
INVREQ1  DS    0H                                                       05450000
         MVC   OUTMSG,MINV    SET ERROR DATA                            05460000
         B     RETURN                                                   05470000
         SPACE 1                                                        05480000
ERROR00  DS    0H                                                       05490000
         MVC   OUTMSG,MNOENT  SET ERROR MESSAGE                         05500000
         B     ERALL          ISSUE ERROR INFORMATION & RETURN REPLY    05510000
         SPACE 1                                                        05520000
ERROR01  DS    0H                                                       05530000
         MVC   OUTMSG,MNOENT  SET ERROR MESSAGE                         05540000
         B     ERALL          ISSUE ERROR INFORMATION & RETURN REPLY    05550000
         SPACE 1                                                        05560000
ERROR02  DS    0H                                                       05570000
         MVC   OUTMSG,MISRTE  SET ERROR MESSAGE                         05580000
         B     ERALL          ISSUE ERROR INFORMATION & RETURN REPLY    05590000
         SPACE 1                                                        05600000
ERROR03  DS    0H                                                       05610000
         MVC   OUTMSG,MREPLE  SET ERROR MESSAGE                         05620000
         B     ERALL          ISSUE ERROR INFORMATION & RETURN REPLY    05630000
         SPACE 1                                                        05640000
ERROR04  DS    0H                                                       05650000
         MVC   OUTMSG,MDLETE  SET ERROR MESSAGE                         05660000
         B     ERALL          ISSUE ERROR INFORMATION & RETURN REPLY    05670000
         SPACE 1                                                        05680000
ERALL    EQU   *                                                        05690000
         BAL   R14,SENDE      MESSAGE RETURN                            05700000
         B     RETURN                                                   05710000
         SPACE 1                                                        05720000
GSERR    EQU   *                                                        05730000
         BAL   R14,GSPRTE     MESSAGE RETURN                            05740000
         B     FINISH         TERMINATE PROCESSING                      05750000
         SPACE 1                                                        05760000
SENDE    DS    0H                                                       05770000
         MVC   EST,STATUS     STATUS INTO THE MESSAGE                   05780000
         CNOP  0,4                                                      05790000
         BAL   R1,WTO         WRITE TO OPERATOR                         05800000
DCMSG    DC    H'30'                                                    05810000
         DC    H'0'                                                     05820000
DCTXT    DS    0CL26                                                    05830000
         DC    C'STATUS '                                               05840000
EST      DS    CL2                                                      05850000
         DC    C', DLI CALL = '                                         05860000
ECALL    DS    CL4                                                      05870000
         SPACE 1                                                        05880000
GSPRTE   DS    0H                                                       05890000
         MVC   GSEST,STATUS     STATUS INTO THE MESSAGE                 05900000
         CNOP  0,4                                                      05910000
         BAL   R1,WTO         WRITE TO OPERATOR                         05920000
GSCMSG   DC    H'31'                                                    05930000
         DC    H'0'                                                     05940000
GSCTXT   DS    0CL27                                                    05950000
         DC    C'STATUS '                                               05960000
GSEST    DS    CL2                                                      05970000
         DC    C', GSAM CALL = '                                        05980000
GSECALL  DS    CL4                                                      05990000
         SPACE 1                                                        06000000
WTO      SVC   35                                                       06010000
         BR    R14            RETURN                                    06020000
         EJECT                                                          06030000
         TITLE 'WORK AREAS AND CONSTANTS'                               06040000
         LTORG                                                          06050000
TABDATE  DC    X'000C'             DATE TABLE: NOT LEAP YEAR            06060000
         DC    X'031C'             JANUARY 31                           06070000
         DC    X'059C'             FEBRUARY 28                          06080000
         DC    X'090C'             MARCH 31                             06090000
         DC    X'120C'             APRIL 30                             06100000
         DC    X'151C'             MAY 30                               06110000
         DC    X'181C'             JUNE 30                              06120000
         DC    X'212C'             JULY 31                              06130000
         DC    X'243C'             AUGUST 31                            06140000
         DC    X'273C'             SEPTEMBER 30                         06150000
         DC    X'304C'             OCTOBER 31                           06160000
         DC    X'334C'             NOVEMBER 30                          06170000
         DC    X'365C'             DECEMBER 31                          06180000
TABDATE1 DC    X'000C'             DATE TABLE:  LEAP YEAR               06190000
         DC    X'031C'             JANUARY 31                           06200000
         DC    X'060C'             FEBRUARY 29                          06210000
         DC    X'091C'             MARCH 31                             06220000
         DC    X'121C'             APRIL 30                             06230000
         DC    X'152C'             MAY 31                               06240000
         DC    X'182C'             JUNE 30                              06250000
         DC    X'213C'             JULY 31                              06260000
         DC    X'244C'             AUGUST 31                            06270000
         DC    X'274C'             SEPTEMBER 30                         06280000
         DC    X'305C'             OCTOBER 31                           06290000
         DC    X'335C'             NOVEMBER 30                          06300000
         DC    X'366C'             DECEMBER 31                          06310000
TLENTH   EQU   2                   TABLE ENTRY SIZE                     06320000
DATEPTRN DS    0CL8                DATE PATTERN WORK AREA               06330000
PRNTMO   DC    CL2'  '             MM                                   06340000
         DC    C'/'                                                     06350000
PRNTDAY  DC    CL2'  '             DD                                   06360000
         DC    C'/'                                                     06370000
PRNTYR   DC    CL2'  '             YY                                   06380000
PRNTDAY1 EQU   PRNTDAY+1                                                06390000
PRNTMO1  EQU   PRNTMO+1                                                 06400000
PRNTYR1  EQU   PRNTYR+1                                                 06410000
ZERODATE DC    CL8'00/00/00'                                            06420000
         DS    0D                  DOUBLE WORD BOUNDRY                  06430000
CVBWORK  DC    F'0'                CONVERSION                           06440000
CVBRIGHT DC    H'0'                WORK                                 06450000
PEDIT    DC    H'0'                AREA                                 06460000
PEDIT1   EQU   PEDIT+1                                                  06470000
PDATE    DC    F'0'                DATE SAVED: 00YYDDDS                 06480000
FW64K    DC    F'65535'            *                                    06490000
F12      DS    0F                  *                                    06500000
         DC    H'00'               *                                    06510000
H12      DC    H'12'               *                                    06520000
*        EQUATES.                                                       06530000
DISP0    EQU   0                                                        06540000
DISP2    EQU   2                                                        06550000
ONE      EQU   1                                                        06560000
FOUR     EQU   4                                                        06570000
SIXTEEN  EQU   16                                                       06580000
X03      EQU   X'03'                                                    06590000
XF0      EQU   X'F0'                                                    06600000
         SPACE 1                                                        06610000
         DS    0F                                                       06620000
KTAD     DC    C'TAD'                                                   06630000
KADD     DC    C'ADD'                                                   06640000
KEND     DC    C'END'                                                   06650000
KDIS     DC    C'DIS'                                                   06660000
KDEL     DC    C'DEL'                                                   06670000
KUPD     DC    C'UPD'                                                   06680000
BLANK    DC    256C' '                                                  06690000
         SPACE 1                                                        06700000
MDEL     DC    CL40'ENTRY WAS DELETED'                                  06710000
MADD     DC    CL40'ENTRY WAS ADDED'                                    06720000
MEND     DC    CL40'BMP/DLI PGM HAS ENDED'                              06730000
MDIS     DC    CL40'ENTRY WAS DISPLAYED'                                06740000
MUPD1    DC    CL40'ENTRY WAS UPDATED'                                  06750000
MTEST    DC    CL40'TEST REQUEST WAS ENDED'                             06760000
MMOR     DC    CL40'DATA IS NOT ENOUGH'                                 06770000
MINV     DC    CL40'PROCESS CODE IS NOT VALID'                          06780000
MUPD0    DC    CL40'PLEASE UPDATE ENTRY'                                06790000
MNODATA  DC    CL40'NO DATA WAS ENTERED'                                06800000
MNONAME  DC    CL40'LAST NAME WAS NOT SPECIFIED'                        06810000
MNOENT   DC    CL40'SPECIFIED PERSON WAS NOT FOUND'                     06820000
MISRTE   DC    CL40'ADDITION OF ENTRY HAS FAILED'                       06830000
MDLETE   DC    CL40'DELETION OF ENTRY HAS FAILED'                       06840000
MREPLE   DC    CL40'UPDATE OF ENTRY HAS FAILED'                         06850000
         SPACE 1                                                        06860000
BDRLN    DS    0CL82                                                    06870000
         DC    CL40'*---------------------------------------'           06880000
         DC    CL42'-----------------------------------------*'         06890000
HDRAST   DC    CL50'**************************************************' 06900000
HDRNM    DC    CL50'*  IMS INSTALLATION VERIFICATION PROCEDURE       *' 06910000
TRANTYP  DC    CL37'TRANSACTION TYPE : BMP/DLI (HIDAM DB)'              06920000
DATECON  DC    CL19'DATE             : '                                06930000
CON1     DC    CL24'PROCESS  CODE  (*1) :   '                           06940000
CON2     DC    CL24'LAST  NAME          :   '                           06950000
CON3     DC    CL24'FIRST  NAME         :   '                           06960000
CON4     DC    CL24'EXTENSION  NUMBER   :   '                           06970000
CON5     DC    CL24'INTERNAL  ZIP CODE  :   '                           06980000
CON6     DC    CL17'(*1) PROCESS CODE'                                  06990000
CON7     DC    CL7'ADD    '                                             07000000
CON8     DC    CL7'DELETE '                                             07010000
CON9     DC    CL7'UPDATE '                                             07020000
CON10    DC    CL7'DISPLAY'                                             07030000
CON11    DC    CL7'TADD   '                                             07040000
CON12    DC    CL12'SEGMENT# :  '                                       07050000
         SPACE 1                                                        07060000
*   OUTPUT LINE DEFINITION                                              07070000
         SPACE 1                                                        07080000
OUTLINE  DS    0CL121                                                   07090000
ANSI     DS    CL1                                                      07100000
HFILLER1 DS    CL25                                                     07110000
LEDGE    DS    CL1                                                      07120000
         SPACE 1                                                        07130000
*   DESCRIPTION OF PAGE HEADER LINE TYPE                                07140000
HFILL2   DS    CL15                                                     07150000
HDRLN    DS    CL50                                                     07160000
HFILL3   DS    CL15                                                     07170000
REDGE    DS    CL1                                                      07180000
HFILL4   DS    CL13                                                     07190000
         SPACE 1                                                        07200000
         ORG   HFILL2                                                   07210000
*   DESCRIPTION OF FIRST DATA LINE TYPE                                 07220000
DFILL1   DS    CL40                                                     07230000
D1LN     DS    0CL36                                                    07240000
D1CON    DS    CL19                                                     07250000
D1VAR    DS    CL8                                                      07260000
         SPACE 1                                                        07270000
         ORG   HFILL2                                                   07280000
*   DESCRIPTION OF SECOND DATA LINE TYPE                                07290000
DFILL2   DS    CL10                                                     07300000
D2CON1   DS    CL24                                                     07310000
D2VAR    DS    CL10                                                     07320000
DFILL2A  DS    CL23                                                     07330000
D2CON2   DS    CL7                                                      07340000
         SPACE 1                                                        07350000
         ORG   HFILL2                                                   07360000
*   DESCRIPTION OF THIRD DATA LINE TYPE                                 07370000
DFILL3   DS    CL59                                                     07380000
D3CON    DS    CL17                                                     07390000
         SPACE 1                                                        07400000
         ORG   HFILL2                                                   07410000
*   DESCRIPTION OF FOURTH DATA LINE TYPE                                07420000
DFILL4   DS    CL10                                                     07430000
D4VAR1   DS    CL40                                                     07440000
DFILL4A  DS    CL10                                                     07450000
D4CON    DS    CL12                                                     07460000
D4VAR2   DS    CL4                                                      07470000
         ORG   LEDGE                                                    07480000
*   DESCRIPTION OF BORDER LINE TYPE                                     07490000
OUTLN1   DS    CL82                                                     07500000
         ORG   ,             RESET LOCATION COUNTER                     07510000
         SPACE 2                                                        07520000
*      FUNCTION CODES USED                                              07530000
         SPACE 1                                                        07540000
GU       DC    CL4'GU  '                                                07550000
GHU      DC    CL4'GHU '                                                07560000
GN       DC    CL4'GN  '                                                07570000
GHN      DC    CL4'GHN '                                                07580000
ISRT     DC    CL4'ISRT'                                                07590000
DLET     DC    CL4'DLET'                                                07600000
REPL     DC    CL4'REPL'                                                07610000
         SPACE 2                                                        07620000
ECB1     DC    F'0'           REPLY ECB                                 07630000
REPLY    DC    4F'0'          REPLY AREA                                07640000
SW1      DC    X'00'          REPLY SWITCH                              07650000
SW1TADD  EQU   X'80'          'TADD' REQUEST IN PROGRESS                07660000
SW1DATA  EQU   X'20'          SOME DATA IS TYPED IN                     07670000
SW1DEST  EQU   X'08'          DESTINATION IS SETUP                      07680000
MODNAME  DC    CL8' '                                                   07690000
DESTNAME DC    CL8' '                                                   07700000
IOPSGNO  DC    F'0'           SEGMENT# ON IOPCB                         07710000
WORKD    DS    D              WORKAREA                                  07720000
         SPACE 1                                                        07730000
PCBALST  DC    F'0'           A(PCBLIST)                                07740000
IOPCB    DC    F'0'           A(IOPCB)                                  07750000
DBPCB    DC    F'0'           A(DBPCB)                                  07760000
GIPCB    DC    F'0'           A(GIPCB)                                  07770000
GOPCB    DC    F'0'           A(GOPCB)                                  07780000
         SPACE 1                                                        07790000
SAVEAREA DC    18F'0'                                                   07800000
PCBM     DS    CL52                                                     07810000
         SPACE 1                                                        07820000
*---------------------------------------------------------------------* 07830000
*      DATA AREA FOR TERMINAL INPUT                                   * 07840000
*---------------------------------------------------------------------* 07850000
         SPACE 1                                                        07860000
INPUT    DS    0CL80                                                    07870000
INCMD    DS    CL8             PROCESS REQUEST (COMMAND)                07880000
INDATA1  DS    0CL37                                                    07890000
INNAME1  DS    CL10            LAST NAME                                07900000
INNAME2  DS    CL10            FIRST NAME                               07910000
INEXT#   DS    CL10            EXTENTION #                              07920000
INZIP    DS    CL7             INTERNAL ZIP CODE                        07930000
INFILL   DS    CL35                                                     07940000
         SPACE 1                                                        07950000
SSA      DS    0F                                                       07960000
         DC    CL8'A1111111'                                            07970000
SSAT     DC    C'('                                                     07980000
         DC    CL8'A1111111'                                            07990000
         DC    CL2' ='                                                  08000000
SSAKEY   DS    CL10            LAST NAME                                08010000
         DC    C')'                                                     08020000
         SPACE 1                                                        08030000
SSA1     DS    0F                                                       08040000
         DC    CL8'A1111111'                                            08050000
         DC    C' '                                                     08060000
         SPACE 1                                                        08070000
*---------------------------------------------------------------------* 08080000
*      I/O AREA FOR DATA BASE HANDLING                                * 08090000
*---------------------------------------------------------------------* 08100000
         SPACE 1                                                        08110000
         DS    0F                                                       08120000
IODATA   DS    0CL48                                                    08130000
IODATA1  DS    0CL37                                                    08140000
IONAME1  DS    CL10           LAST NAME                                 08150000
IONAME2  DS    CL10           FIRST NAME                                08160000
IOEXT#   DS    CL10           EXTENTION#                                08170000
IOZIP    DS    CL7            INTERNAL ZIP CODE                         08180000
         DS    CL3                                                      08190000
IOCMD    DS    CL8                                                      08200000
         SPACE 1                                                        08210000
*---------------------------------------------------------------------* 08220000
*      DATA AREA OUTPUT                                               * 08230000
*---------------------------------------------------------------------* 08240000
         SPACE 1                                                        08250000
         DS    0F                                                       08260000
OUTPUT   DS    0CL121                                                   08270000
OUTMSG   DS    CL40                                                     08280000
OUTCMD   DS    CL8            REQUEST CODE                              08290000
OUTDATA1 DS    0CL37                                                    08300000
OUTNAME1 DS    CL10           LAST NAME                                 08310000
OUTNAME2 DS    CL10           FIRST NAME                                08320000
OUTEXT#  DS    CL10           EXTENTION#                                08330000
OUTZIP   DS    CL7            INTERNAL ZIP CODE                         08340000
OUTSEGNO DS    CL4            OUTPUT SEGMENT NUMBER                     08350000
OUTFILL  DC    CL32' '                                                  08360000
         SPACE 1                                                        08370000
PCBNAME  DSECT                                                          08380000
TNAME    DS    CL8            TRANSACTION-NAME                          08390000
TC       DS    CL2                                                      08400000
STATUS   DS    CL2            STATUS                                    08410000
DBPRO    DS    CL4            PROC OPTION                               08420000
         DS    F              RESERVED                                  08430000
DBSFB    DS    CL8            SEGMENT NAME FEEDBACK                     08440000
DBLKA    DS    F              CURRENT LENGTH OF KEY FEEDBACK AREA       08450000
DBNSS    DS    F              NO OF SENSITIVE SEGMENTS                  08460000
DBKFA    DS    CL17           KEY FEEDBACK AREA                         08470000
         DS    CL3                                                      08480000
         END                                                            08490000