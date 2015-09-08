      SUBROUTINE ASP_GETWSL (WSL,STATUS)

*+  Obtain the current working-set limit, using the system service routine
*   'SYS$ADJWSL'.

      INTEGER WSL,STATUS,SYS$ADJWSL

      STATUS = SYS$ADJWSL (,WSL)

      END
