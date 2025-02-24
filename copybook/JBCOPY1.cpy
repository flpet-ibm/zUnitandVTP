       03  P016-QZRE016.

          05  P016-QRRG001.
                 07  P016-QRRG101.
             09  P016-ANTAL-PROF-GR-DATA         PIC S9(2) COMP-3
                 VALUE ZERO.
             09  P016-PROF-GR-AENDRET
                            OCCURS 150 TIMES     PIC X(1).

             09  P016-SARG010G.
               11  P016-SARG010-PROF-GR-LISTE
                            OCCURS 150 TIMES.
                            13 P016-SARG010-PROF-GR PIC X.
