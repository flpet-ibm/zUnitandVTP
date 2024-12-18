       CBL SQL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AADCLGEN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER. MAINFRAME WITH DEBUGGING MODE.
       SOURCE-COMPUTER. MAINFRAME.


       DATA DIVISION.
      *****************************************************************
      *** Working storage                                           ***
      *****************************************************************
       WORKING-STORAGE SECTION.


      * Get the CONTROL table
           EXEC SQL INCLUDE CLAIM     END-EXEC.
           EXEC SQL INCLUDE COMMERCI  END-EXEC.
           EXEC SQL INCLUDE CUSTOMER  END-EXEC.
           EXEC SQL INCLUDE CUSTOMSE  END-EXEC.
           EXEC SQL INCLUDE ENDOWMEN  END-EXEC.
           EXEC SQL INCLUDE MOTOR     END-EXEC.
           EXEC SQL INCLUDE POLICY    END-EXEC.



      *****************************************************************
      *** Main Processing                                           ***
      *****************************************************************
       PROCEDURE DIVISION.
       MAIN SECTION.
       MAIN1.

           GOBACK.
