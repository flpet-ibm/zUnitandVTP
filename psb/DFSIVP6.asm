**************************** INSTALL/IVP ****************************** 00010000
*                                                                     * 00020000
*                   M O D U L E    P R O L O G U E                    * 00030000
*                                                                     * 00040000
*********************************************************************** 00050000
*                                                                     * 00060000
*          NAME:  DFSIVP6                                             * 00070000
*                                                                     * 00080000
*   DESCRIPTION:  PSB FOR BATCH/BMP - HIDAM/OSAM                      * 00090000
*                                                                     * 00100000
**************************************************************@SCPYRT** 00110000
*                                                                     * 00120000
*  Licensed Materials - Property of IBM                               * 00130000
*                                                                     * 00140000
*  5635-A06                                                           * 00150000
*                                                                     * 00160000
*      Copyright IBM Corp. 1974,1998 All Rights Reserved              * 00170000
*                                                                     * 00180000
*  US Government Users Restricted Rights - Use, duplication or        * 00190000
*  disclosure restricted by GSA ADP Schedule contract with            * 00200000
*  IBM Corp.                                                          * 00210000
*                                                                     * 00220000
**************************************************************@ECPYRT** 00230000
*                                                                     * 00240000
*        STATUS:  IMS/ESA 3.1                                         * 00250000
*                                                                     * 00260000
*   MODULE TYPE:  ASSEMBLER                                           * 00270000
*                                                                     * 00280000
*       CHANGES:  CHANGED TO HIDAM FOR 3.1                            * 00290000
*                                                                     * 00300000
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7* 00310000
         PCB   TYPE=DB,DBDNAME=IVPDB1,PROCOPT=A,KEYLEN=10,SB=COND       00320000
         SENSEG NAME=A1111111,PARENT=0,PROCOPT=A                        00330000
         PCB   TYPE=GSAM,DBDNAME=IVPDB5,PROCOPT=G                       00340000
         PCB   TYPE=GSAM,DBDNAME=IVPDB5,PROCOPT=L                       00350000
         PSBGEN LANG=ASSEM,PSBNAME=DFSIVP6,CMPAT=YES,OLIC=YES           00360000
         END                                                            00370000