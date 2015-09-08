      SUBROUTINE ASP_DCLEXH (HAND)

*+  Declare exit handler routine to be executed in the event of image
*   exit. Any error is ignored.

      INTEGER DESBLK(4),STATUS,SYS$DCLEXH,EXSTAT

      DESBLK(1) = 0
      DESBLK(2) = %LOC(HAND)
      DESBLK(3) = 1
      DESBLK(4) = %LOC(EXSTAT)

      STATUS = SYS$DCLEXH (DESBLK)

      END
