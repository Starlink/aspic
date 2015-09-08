C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      FUNCTION TRUN
C
C
C ----------------------------------------------------
C
C
C
      FUNCTION TRUN(NUM,A)
C
C
C
      NUMA = NUM
      IF (NUMA.LT.1) NUMA = 1
      TRUN = A
      TOP = 10.0**(NUMA) - 0.1
      IF (TRUN.GT.TOP) TRUN = TOP
      BOT = -1.0*(10.0**(NUMA-1)) + 0.1
      IF (TRUN.LT.BOT) TRUN = BOT
C
C
C
      END




