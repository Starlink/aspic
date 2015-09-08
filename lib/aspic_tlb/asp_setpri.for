      SUBROUTINE ASP_SETPRI (BP,STATUS)

*+  Set the current base priority, using the system service routine
*   'SYS$SETPRI'.

      INTEGER BP,STATUS,SYS$SETPRI

      STATUS = SYS$SETPRI (,,%VAL(BP),)

      END
