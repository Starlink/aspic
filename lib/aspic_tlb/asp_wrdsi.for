      SUBROUTINE ASP_WRDSI (PARAM,DESCR,OUTPUT,SIZE,STATUS)

*+  ASP_WRDSI
*
*   Write an array of integers to a frame descriptor.
*
*   Given:
*    PARAM   C   program parameter name corresponding to BDF
*    DESCR   C   descriptor name corresponding to required information
*
*   Returned:
*    OUTPUT  IA  output array
*    SIZE    I   no of values corresponding to DESCR
*    STATUS  I   return status (Starlink)
*
*   Called:
*    ITOC, WRDSCR: STARLINK
*
*   WFL RGO 22 Oct 1981
*-

      INTEGER SIZE,STATUS,I,NLEFT,J
      LOGICAL FIRST
      INTEGER OUTPUT(SIZE)
      CHARACTER PARAM*(*),DESCR*(*),CVALS(256)*15

      FIRST = .TRUE.
      DO I=1,SIZE,256
          NLEFT = MIN (256,SIZE-I+1)
          DO J=1,NLEFT
              CALL ITOC (OUTPUT(I+J-1),CVALS(J),STATUS)
          ENDDO
          IF (FIRST) THEN
              CALL WRDSCR (PARAM,DESCR,CVALS,NLEFT,STATUS)
              FIRST = .FALSE.
          ELSE
              CALL ADDSCR (PARAM,DESCR,CVALS,NLEFT,STATUS)
          ENDIF
          IF (STATUS.NE.ERR_NORMAL) THEN
              GOTO 999
          ENDIF
      ENDDO

999   END
