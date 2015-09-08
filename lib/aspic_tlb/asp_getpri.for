      SUBROUTINE ASP_GETPRI (BP,STATUS)

*+  Obtain the current base priority, using the system service routine
*   'SYS$SETPRI'.

      INTEGER BP,STATUS,SYS$SETPRI

      STATUS = SYS$SETPRI (,,%VAL(31),BP)
      STATUS = SYS$SETPRI (,,%VAL(BP),)

      END
