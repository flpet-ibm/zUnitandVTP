//IBMUSERR JOB 241901,'',NOTIFY=&SYSUID,CLASS=A,MSGCLASS=H
//*
//*
//BIND    EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD  DSN=IBMUSER.UBUILD.LOAD,DISP=SHR
//         DD  DSN=DB2V13.SDSNLOAD,DISP=SHR
//*         DD  DSN=IBMUSER.DLAYDBG.LOAD,DISP=SHR
//*          DD DISP=SHR,DSN=FELF00.SEQAMOD
//DBRMLIB  DD  DSN=IBMUSER.UBUILD.DBRM,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//CEEOPTS DD  *
  TEST
/*
//CEEDUMP DD SYSOUT=*
//SYSOUT  DD SYSOUT=*
//SYSTSIN DD *
DSN SYSTEM(DBD1)
BIND PACKAGE (LGDB2MAI)                                    -
     ISO(CS)                                              -
     CURRENTDATA(NO)                                      -
     MEMBER(LGDB2MAI)                                     -
     DEGREE(1)                                            -
     DYNAMICRULES(BIND)                                   -
     ACTION (REPLACE)                                     -
     EXPLAIN(NO)                                          -
     OWNER(IBMUSER)                                       -
     QUALIFIER(GENASA1)                                   -
     ENABLE(BATCH,CICS)                                   -
     REL(DEALLOCATE)                                      -
     VALIDATE(BIND)


BIND PLAN (LGDB2MAI)                                       -
     PKLIST(NULLID.*, *.LGDB2MAI.*)                        -
     CURRENTDATA(NO)                                      -
     ISO(CS)                                              -
     ACTION (REP)                                         -
     OWNER(IBMUSER)                                       -
     QUALIFIER(GENASA1)                                   -
     REL(DEALLOCATE)                                      -
     ACQUIRE(USE)                                         -
     RETAIN                                               -
     NOREOPT(VARS)                                        -
     VALIDATE(BIND)


RUN PROGRAM(LGDB2MAI) PLAN (LGDB2MAI)
/*