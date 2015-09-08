      PROGRAM CLRQUAD

C+
C  program CLRQUAD
C	part of IAM suite (or stand alone)
C	clears given quadrant of ARGS by calling subroutine CLRQUADS
C
C  Given (program parameters)
C    REPLY (CH)    quadrant cleared (from quad)
C    WORK (I*2A)    work array used in clrquads to clear args quadrant
C
C  D. Tudhope/ROE/Sept 1982
C-

      INTEGER NAXWK(2),NPWK,ISTATUS
      INTEGER IXR,IYR,IDUM

      ISTATUS=0
C*  get origin of quadrant to be cleared
      CALL QUAD(IXR,IYR,IDUM)
C*  obtain a work array (I*2) to be used to clear screen in clrquads
      NAXWK(1)=256
      NAXWK(2)=256
      CALL OUTPICI2('WORK',' ',2,NAXWK,NPWK,ISTATUS)
      IF (ISTATUS.NE.0) THEN
         CALL WRERR('MISREAD')
      ELSE
        CALL CLRQUADS(%VAL(NPWK),IXR,IYR)
      ENDIF
      END
