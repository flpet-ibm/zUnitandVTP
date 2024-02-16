 /********************************************************************/
 /*                                                                  */
 /*                                                                  */
 /*                                                                  */
 /*     Licensed Materials - Property of IBM                         */
 /*                                                                  */
 /*     "Restricted Materials of IBM"                                */
 /*                                                                  */
 /*     5655-Y04                                                     */
 /*                                                                  */
 /*     (C) Copyright IBM Corp. 2012"                                */
 /*                                                                  */
 /*                                                                  */
 /*                                                                  */
 /*                                                                  */
 /*   STATUS = 7.2.0                                                 */
 /*                                                                  */
 /* CHANGE ACTIVITY :                                                */
 /*                                                                  */
 /*   $SEG(DFHEIBLK),COMP(COMMAND),PROD(CICS TS ):                   */
 /*                                                                  */
 /*  PN= REASON REL YYMMDD HDXXIII : REMARKS                         */
 /* $L0= Base   XXX XX     HD????  : Base                            */
 /*                                                                  */
 /********************************************************************/
 /*                                                                  */
 /*                                                                  */
 /*                                                                  */
 /*                                                                  */
   DCL DFHEIPTR PTR;                   /* INPUT PARAMETER            */
   DCL 1 EIBLK UNALIGNED BASED(DFHEIPTR),
                                       /* EXEC INTERFACE BLOCK       */
     2 EIBTIME FIXED DEC(7,0),         /* TIME IN 0HHMMSS FORMAT     */
     2 EIBDATE FIXED DEC(7,0),         /* DATE IN 0CYYDDD FORMAT,    */
                                       /* where C is the century     */
                                       /* indicator (0=1900, 1=2000) */
                                       /* YY is the year and DDD is  */
                                       /* the day number.            */
     2 EIBTRNID CHAR(4),               /* TRANSACTION IDENTIFIER     */
     2 EIBTASKN FIXED DEC(7,0),        /* TASK NUMBER                */
     2 EIBTRMID CHAR(4),               /* TERMINAL IDENTIFIER        */
     2 EIBRSVD1 FIXED BIN(15),         /* RESERVED                   */
     2 EIBCPOSN FIXED BIN(15),         /* CURSOR POSITION            */
     2 EIBCALEN FIXED BIN(15),         /* COMMAREA LENGTH            */
     2 EIBAID CHAR(1),                 /* ATTENTION IDENTIFIER       */
     2 EIBFN CHAR(2),                  /* FUNCTION CODE              */
     2 EIBRCODE CHAR(6),               /* RESPONSE CODE              */
     2 EIBDS CHAR(8),                  /* DATASET NAME               */
     2 EIBREQID CHAR(8),               /* REQUEST IDENTIFIER         */
     2 EIBRSRCE CHAR(8),               /* RESOURCE NAME              */
     2 EIBSYNC CHAR(1),                /* X'FF' SYNCPOINT REQUESTED  */
     2 EIBFREE CHAR(1),                /* X'FF' FREE REQUESTED       */
     2 EIBRECV CHAR(1),                /* X'FF' RECEIVE REQUIRED     */
     2 EIBSEND CHAR(1),                /* RESERVED                   */
     2 EIBATT CHAR(1),                 /* X'FF' ATTACH DATA RECEIVED */
     2 EIBEOC CHAR(1),                 /* X'FF' EOC RECEIVED         */
     2 EIBFMH CHAR(1),                 /* X'FF' FMHS RECEIVED        */
     2 EIBCOMPL CHAR(1),               /* X'FF' DATA COMPLETE        */
     2 EIBSIG CHAR(1),                 /* X'FF' SIGNAL RECEIVED      */
     2 EIBCONF CHAR(1),                /* X'FF' CONFIRM REQUESTED    */
     2 EIBERR CHAR(1),                 /* X'FF' ERROR RECEIVED       */
     2 EIBERRCD CHAR(4),               /* ERROR CODE RECEIVED        */
     2 EIBSYNRB CHAR(1),               /* X'FF' SYNC ROLLBACK REQ'D  */
     2 EIBNODAT CHAR(1),               /* X'FF' NO APPL DATA REC'D   */
     2 EIBRESP  FIXED BIN(31),         /* INTERNAL CONDITION NUMBER  */
     2 EIBRESP2 FIXED BIN(31),         /* MORE DETAILS ON SOME RESPS */
     2 EIBRLDBK CHAR(1);                /* ROLLED BACK                */
   DCL (DFHEPI ENTRY,
   DFHEI1  ENTRY(*,CHAR(8),*,FIXED BIN(15),*,FIXED BIN(15),
                   FIXED BIN(15),CHAR(4),CHAR(8)),
   DFHEI2  ENTRY(*,CHAR(8),PTR,FIXED BIN(15),CHAR(8),CHAR(8),PTR,
                   FIXED BIN(15)),
   DFHEI3  ENTRY(*,CHAR(8),*,FIXED BIN(15),CHAR(8),*),
   DFHEI4  ENTRY(*,CHAR(8),*,FIXED BIN(15),CHAR(8),CHAR(8),CHAR(2),
                   CHAR(2),FIXED BIN(15),PTR,*,*,FIXED BIN(15)),
   DFHEI5  ENTRY(*,LABEL,LABEL,LABEL,LABEL,LABEL,LABEL,LABEL,
                   LABEL,LABEL,LABEL,LABEL,LABEL),
   DFHEI6  ENTRY(*,CHAR(4),*,*,CHAR(8)),
   DFHEI7  ENTRY(*,PTR,FIXED BIN(15),*),
   DFHEI8  ENTRY(*,FIXED DEC(7,0),CHAR(8),PTR),
   DFHEI9  ENTRY(*,FIXED DEC(7,0),CHAR(4),*,*,*,CHAR(2),CHAR(2)),
   DFHEI10 ENTRY(*,FIXED DEC(7,0),CHAR(8),CHAR(4),*,FIXED BIN(15),
                   CHAR(4),CHAR(4),CHAR(4),CHAR(4),CHAR(8)),
   DFHEI11 ENTRY(*,FIXED BIN(15),FIXED BIN(31),CHAR(2),*,FIXED BIN(15),
                   *,FIXED BIN(15)),
   DFHEI12 ENTRY(*,*,FIXED BIN(15),*,FIXED BIN(15),*,CHAR(4),
                   FIXED BIN(15),CHAR(2),CHAR(4),CHAR(4),
                   CHAR(8)),
   DFHEI13 ENTRY(*,CHAR(8),FIXED BIN(15),CHAR(6),FIXED BIN(15),*,
                   FIXED BIN(15),*,FIXED BIN(15),FIXED BIN(15),
                   FIXED BIN(15)),
   DFHEI14 ENTRY(*,*,*,*,*,*,*,*,*,*,*,*,*),
   DFHEI15 ENTRY(*,PTR,PTR,PTR,PTR,PTR,PTR,PTR,PTR,PTR,PTR,PTR,PTR),
   DFHEI16 ENTRY(*,*,FIXED BIN(15),*,*,*,*,*,CHAR(4),CHAR(4),
                   CHAR(8)),
   DFHEI17 ENTRY(*,CHAR(8),*,CHAR(4),*,*,*,CHAR(4)),
   DFHEI18 ENTRY(*,CHAR(4),CHAR(4),CHAR(8),FIXED BIN(15),
                   FIXED BIN(15),FIXED BIN(15),CHAR(8),
                   CHAR(8),CHAR(8),CHAR(8),CHAR(8)))
                 EXT OPTIONS(ASSEMBLER INTER);