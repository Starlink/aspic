      SUBROUTINE ASP_SETWSL (WSL,STATUS)

*+  Set the current working-set limit, using the system service routine
*   'SYS$ADJWSL'.

      INTEGER WSL,STATUS,SYS$ADJWSL,WS

      STATUS = SYS$ADJWSL (,WS)
      STATUS = SYS$ADJWSL (%VAL(WSL-WS),)

      END
