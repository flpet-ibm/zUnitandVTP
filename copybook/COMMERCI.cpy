      ******************************************************************
      * DCLGEN TABLE(GENASA1.COMMERCIAL)                               *
      *        LIBRARY(IBMUSER.UBUILD.COBOL(COMMERCI))                 *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE COMMERCIAL TABLE
           ( POLICYNUMBER                   INTEGER NOT NULL,
             REQUESTDATE                    TIMESTAMP,
             STARTDATE                      DATE,
             RENEWALDATE                    DATE,
             ADDRESS                        CHAR(255),
             ZIPCODE                        CHAR(8),
             LATITUDEN                      CHAR(11),
             LONGITUDEW                     CHAR(11),
             CUSTOMER                       CHAR(255),
             PROPERTYTYPE                   CHAR(255),
             FIREPERIL                      SMALLINT,
             FIREPREMIUM                    INTEGER,
             CRIMEPERIL                     SMALLINT,
             CRIMEPREMIUM                   INTEGER,
             FLOODPERIL                     SMALLINT,
             FLOODPREMIUM                   INTEGER,
             WEATHERPERIL                   SMALLINT,
             WEATHERPREMIUM                 INTEGER,
             STATUS                         SMALLINT,
             REJECTIONREASON                CHAR(255)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GENASA1.COMMERCIAL                 *
      ******************************************************************
       01  DCLCOMMERCIAL.
           10 POLICYNUMBER         PIC S9(9) USAGE COMP-5.
           10 REQUESTDATE          PIC X(26).
           10 STARTDATE            PIC X(10).
           10 RENEWALDATE          PIC X(10).
           10 ADDRESS              PIC X(255).
           10 ZIPCODE              PIC X(8).
           10 LATITUDEN            PIC X(11).
           10 LONGITUDEW           PIC X(11).
           10 CUSTOMER             PIC X(255).
           10 PROPERTYTYPE         PIC X(255).
           10 FIREPERIL            PIC S9(4) USAGE COMP-5.
           10 FIREPREMIUM          PIC S9(9) USAGE COMP-5.
           10 CRIMEPERIL           PIC S9(4) USAGE COMP-5.
           10 CRIMEPREMIUM         PIC S9(9) USAGE COMP-5.
           10 FLOODPERIL           PIC S9(4) USAGE COMP-5.
           10 FLOODPREMIUM         PIC S9(9) USAGE COMP-5.
           10 WEATHERPERIL         PIC S9(4) USAGE COMP-5.
           10 WEATHERPREMIUM       PIC S9(9) USAGE COMP-5.
           10 STATUS               PIC S9(4) USAGE COMP-5.
           10 REJECTIONREASON      PIC X(255).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 20      *
      ******************************************************************