 /******************************************************************
  *                                                                *
  * LICENSED MATERIALS - PROPERTY OF IBM                           *
  *                                                                *
  * "RESTRICTED MATERIALS OF IBM"                                  *
  *                                                                *
  * CB12                                                           *
  *                                                                *
  * (C) COPYRIGHT IBM CORP. 2011, 2013 ALL RIGHTS RESERVED         *
  *                                                                *
  * US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION,      *
  * OR DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE                   *
  * CONTRACT WITH IBM CORPORATION                                  *
  *                                                                *
  *                                                                *
  *               COPYBOOK for COMMAREA structure                  *
  *                                                                *
  *   This commarea can be used for all functions                  *
  *                                                                *
  *                                                                *
  *                                                                *
  ******************************************************************/
  DCL 01 LGCMAREP,
       03 REQUESTID            CHAR(6),
       03 RETURNCODE           PIC '99',
       03 CUSTOMERNUM          PIC '9999999999',
          03 FIRSTNAME         CHAR(10),
          03 LASTNAME          CHAR(20),
          03 DOB                CHAR(10),
          03 HOUSENAME         CHAR(20),
          03 HOUSENUM          CHAR(4),
          03 POSTCODE           CHAR(8),
          03 NUMPOLICIES       PIC '999',
          03 PHONEMOBILE       CHAR(20),
          03 PHONEHOME         CHAR(20),
          03 EMAILADDRESS      CHAR(100),
          03 POLICYDATA        CHAR(10000),
          03 POLICYDATA2       CHAR(10000),
          03 POLICYDATA3       CHAR(12267);
