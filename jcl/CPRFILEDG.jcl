//IBMUSER1 JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(10),REGION=144M,COND=(16,LT)
//*
//RUN     EXEC PGM=CPRFILED
//*******************************************
//*    PARM=('/TEST(,,,DBM%IBMUSER)')
//*******************************************
//*   PARM=('/TEST(,,,DBM%IBMUSER)',
//*    'ENVAR("AQE_STARTUP_KEY=CC")')
//*******************************************
//STEPLIB   DD DSN=IBMUSER.UBUILD.LOAD,DISP=SHR
//         DD DISP=SHR,DSN=CEE.SCEERUN
//         DD DISP=SHR,DSN=ISM400.SEQAAUTH
//         DD DISP=SHR,DSN=ISM400.SFELLOAD
//         DD DISP=SHR,DSN=ISM400.SEQALPA
//         DD DISP=SHR,DSN=ISM400.SEQAMOD
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//FILEIN   DD DSN=IBMUSER.COBOL.CPRFILE.FILEIN,DISP=SHR
//*
