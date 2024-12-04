//       PROC MBR=TEMPNAME,PSB=,IN=,OUT=,                               00000010
//            OPT=N,SPIE=0,TEST=0,DIRCA=000,                            00000020
//            PRLD=,STIMER=,CKPTID=,PARDLI=,                            00000030
//            CPUTIME=,NBA=,OBA=,IMSID=,AGN=,                           00000040
//            SSM=,PREINIT=,RGN=512K,SOUT=A,                            00000050
//            SYS2=,ALTID=,APARM=,LOCKMAX=,                             00000060
//            ENVIRON=,JVMOPMAS=                                        00000070
//*                                                                     00000080
//G      EXEC PGM=DFSRRC00,REGION=&RGN,                                 00000090
//            PARM=(BMP,&MBR,&PSB,&IN,&OUT,                             00000100
//            &OPT&SPIE&TEST&DIRCA,&PRLD,                               00000110
//            &STIMER,&CKPTID,&PARDLI,&CPUTIME,                         00000120
//            &NBA,&OBA,&IMSID,&AGN,&SSM,                               00000130
//            &PREINIT,&ALTID,                                          00000140
//            '&APARM',&LOCKMAX,                                        00000150
//            &ENVIRON,&JVMOPMAS)                                       00000160
//STEPLIB  DD DSN=DFSF10.USER.SDFSRESL,DISP=SHR                         00000170
//         DD DSN=DFSF10.SDFSRESL,DISP=SHR                              00000170
//         DD DSN=DFSF10.&SYS2.PGMLIB,DISP=SHR                          00000180
//PROCLIB  DD DSN=DFSF10.&SYS2.PROCLIB,DISP=SHR                         00000190
//SYSUDUMP DD SYSOUT=&SOUT,                                             00000200
//         DCB=(LRECL=121,RECFM=VBA,BLKSIZE=3129),                      00000210
//         SPACE=(125,(2500,100),RLSE,,ROUND)                           00000220