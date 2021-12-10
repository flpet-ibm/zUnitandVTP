           05 MYPREFIX-PARM-FILG0570.
             10 MYPREFIX-FILG-TIMESTAMP PIC X(26).
              10 MYPREFIX-TID-STAMP REDEFINES MYPREFIX-FILG-TIMESTAMP.
                15 MYPREFIX-DAT-AAAA-MM-DD
                                        PIC X(10).
                15 MYPREFIX-FILLER      OCCURS 1 TIMES
                                        PIC X.
                15 MYPREFIX-TID-HH-MM-SS PIC X(8).
                15 MYPREFIX-FILLER      OCCURS 1 TIMES
                                        PIC X.
                15 MYPREFIX-MIKROSEK    PIC X(6).
              10 MYPREFIX-FILLER        OCCURS 1 TIMES
                                        PIC X.
