//IBMUSERP JOB ,MSGCLASS=H,CLASS=A,NOTIFY=&SYSUID,REGION=144M,
//   TIME=(10)
//*
//* Action: Run Test Case...
//*
//RUNNER EXEC PROC=BZUPPLA3,
//        PRM='TRACE=Y,UNINIT=00',
//       BZULOD=IBMUSER.UBUILV15.LOAD,
//*        BZULOD=IBMUSER.LOAD,
//        BZULOD2=IBMUSER.LOAD,
//        BZUPLAY=IBMUSER.IMS.PB
//BZUMETA  DD DISP=SHR,DSN=IBMUSER.BZU.BZUMETA
//CEEOPTS  DD  *
  TRAP(OFF),STORAGE(00,NONE,00),
  STACK(4K,4080,ANYWHERE,KEEP,4K,4080)
  NOTEST
//SYSOUT DD SYSOUT=*
//*
//  EXEC JSONFTP,BZUMETA=IBMUSER.BZU.BZUMETA,
//  TESTCASE=IMSTM
//*
//