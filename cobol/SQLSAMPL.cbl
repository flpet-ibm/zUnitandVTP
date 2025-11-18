       ID DIVISION.
       PROGRAM-ID. sqlsampl.
      ***
      *
      *    (C) 2019 IBM FLEMMING PETERSEN
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-SQL-STMT PIC x(100).
       01  WS-SQL-POS  PIC s9(4) binary.
       01  WS-SQL-CHAR PIC X.
       01  WS-SQL-LEN  pic s9(4) binary.

       PROCEDURE DIVISION.
      *
       MAIN SECTION.
       MAIN1.
           move 100 to ws-sql-len
           move 'select * into ? where number = :?' to
              ws-sql-stmt.

           display 'SQL Statement before:'.
           display ws-sql-stmt

           PERFORM VARYING WS-SQL-POS FROM 1 BY 1
                   UNTIL WS-SQL-POS > WS-SQL-LEN

      *      MOVE SUBSTRING(WS-SQL-STMT,WS-SQL-POS:1) TO WS-SQL-CHAR
             MOVE WS-SQL-STMT(WS-SQL-POS:1) TO WS-SQL-CHAR
                   IF WS-SQL-CHAR = '?'
                MOVE '?' TO WS-SQL-STMT(WS-SQL-POS:1)
             ELSE
                MOVE WS-SQL-CHAR TO WS-SQL-STMT(WS-SQL-POS:1)
             END-IF
             COMPUTE WS-SQL-POS = WS-SQL-POS + 1
           END-PERFORM
           display 'SQL Statement after:'.
           display ws-sql-stmt

           stop run.

       end program sqlsampl.
