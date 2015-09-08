      SUBROUTINE STOKES1(NXT,NYT,ARRT,NXA,NYA,ARRA,
     :                   NXQ,NYQ,ARRQ,NXU,NYU,ARRU)
*+
*   STOKES1
*
*   Calculate equatorial Stokes parameters given total
*   polarization and angle.
*
*   Given      (arguments)
*   NXT     I   X-dimension of polarization frame
*   NYT     I   Y-dimension of polarization frame
*   ARRT    RA  polarization frame
*   NXA     I   X-dimension of angle frame
*   NYA     I   Y-dimension of angle frame
*   ARRA    RA  angle frame
*   NXQ     I   X-dimension of Q frame
*   NYQ     I   Y-dimension of Q frame
*   NXU     I   X-dimension of U frame
*   NYU     I   Y-dimension of U frame
*
*   Returned   (arguments)
*   ARRQ    RA  Q frame
*   ARRU    RA  U frame
*
*   B.D.Kelly/ROE/1.3.1982
*-

      INTEGER NXT,NYT,NXA,NYA,NXQ,NYQ,NXU,NYU
      REAL ARRT(NXT,NYT),ARRA(NXA,NYA),ARRQ(NXQ,NYQ),ARRU(NXU,NYU)
*
*   Calculate values for common areas of the given arrays.
*
      DO J=1,MIN(NYT,NYA,NYQ,NYU)
         DO I=1,MIN(NXT,NXA,NXQ,NXU)
            ARRQ(I,J)=ARRT(I,J)*COS(2.0*ARRA(I,J))
            ARRU(I,J)=ARRT(I,J)*SIN(2.0*ARRA(I,J))
         ENDDO
      ENDDO
*
*   Zero any remaining unitialized elements in output arrays.
*
      IF(NYQ.GT.MIN(NYT,NYA,NYQ,NYU)) THEN
         DO J=MIN(NYT,NYA,NYQ,NYU)+1,NYQ
            DO I=1,NXQ
               ARRQ(I,J)=0.0
            ENDDO
         ENDDO
      ENDIF
 
      IF(NXQ.GT.MIN(NXT,NXA,NXQ,NXU)) THEN
         DO J=1,NYQ
            DO I=MIN(NXT,NXA,NXQ,NXU)+1,NXQ
               ARRQ(I,J)=0.0
            ENDDO
         ENDDO
      ENDIF
 
      IF(NYU.GT.MIN(NYT,NYA,NYQ,NYU)) THEN
         DO J=MIN(NYT,NYA,NYQ,NYU)+1,NYU
            DO I=1,NXU
               ARRU(I,J)=0.0
            ENDDO
         ENDDO
      ENDIF
 
      IF(NXU.GT.MIN(NXT,NXA,NXQ,NXU)) THEN
         DO J=1,NYU
            DO I=MIN(NXT,NXA,NXQ,NXU)+1,NXU
               ARRU(I,J)=0.0
            ENDDO
         ENDDO
      ENDIF
 
      END
