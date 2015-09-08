      SUBROUTINE ASP_RDDSR (PARAM,DESCR,MAXSIZ,OUTPUT,SIZE,STATUS)

*+  ASP_RDDSR
*
*   Read an array of integers from a frame descriptor. Note that there
*   is an upper limit of 16384 on the number of values which can be
*   read - even if MAXSIZ is greater than 16384.
*
*   Given:
*    PARAM   C   program parameter name corresponding to BDF
*    DESCR   C   descriptor name corresponding to required information
*    MAXSIZ  I   size of output array
*
*   Returned:
*    OUTPUT  RA  output array
*    SIZE    I   no of values corresponding to DESCR (may be > MAXSIZ)
*    STATUS  I   return status (Starlink + status of 9 indicates SIZE
*                exceeded 16384)
*
*   Called:
*    RDDSCR, CTOR: STARLINK
*
*   WFL RGO 22 Oct 1981
*-

      INCLUDE 'INTERIM(ERRPAR)'
      INTEGER MAXSIZ,SIZE,STATUS,I
      REAL OUTPUT(MAXSIZ)
      CHARACTER PARAM*(*),DESCR*(*),CVALS(16384)*15

      CALL RDDSCR (PARAM,DESCR,16384,CVALS,SIZE,STATUS)
      IF (STATUS.EQ.ERR_NORMAL) THEN
          DO I=1,MIN (16384,SIZE,MAXSIZ)
              CALL CTOR (CVALS(I),OUTPUT(I),STATUS)
          ENDDO
          IF (SIZE.GT.16384) STATUS = 9

      ENDIF
    
      END
