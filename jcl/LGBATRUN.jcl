//IBMUSERR JOB 241901,'BIND PROGRAMS',NOTIFY=&SYSUID,CLASS=A,MSGCLASS=H
//*
//*
//BIND    EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD  DSN=IBMUSER.UBUILD.LOAD,DISP=SHR
//         DD  DSN=DB2V13.SDSNLOAD,DISP=SHR
//*         DD  DSN=IBMUSER.DLAYDBG.LOAD,DISP=SHR
//*          DD DISP=SHR,DSN=FELF00.SEQAMOD
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//CEEOPTS DD  *
  NOTEST
/*
//CEEDUMP DD SYSOUT=*
//SYSOUT  DD SYSOUT=*
//SYSTSIN DD *
DSN SYSTEM(DBD1)
RUN PROGRAM(LGDB2BAT) PLAN (LGBATCH)
/*