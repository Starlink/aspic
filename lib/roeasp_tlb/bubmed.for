      SUBROUTINE BUBMED(ARRAY,NX,NY,IST,IFN,JST,JFN,RMED)
*+
*   BUBMED
*
*   Determines the median of an area of a 2-D array using a
*   BUBBLE sort. The maximum number of pixels allowed in the
*   area is 25
*
*   Given      (arguments)
*   ARRAY   RA  2-D data array
*   NX      I   X-extent of data array
*   NY      I   Y-extent of data array
*   IST     I   X-start coordinate
*   IFN     I   X-finish coordinate
*   JST     I   Y-start coordinate
*   JFN     I   Y-finish coordinate
*
*   Returned   (arguments)
*   RMED    R   median of the rectangle between (IST,JST) and (IFN,JFN)
*
*   Subroutines called :
*   BUBBLE             : E2DLIB
*
*   B.D.Kelly/ROE/2.4.1982
*-

      INTEGER NX,NY,IST,IFN,JST,JFN
      REAL ARRAY(NX,NY),RMED
      REAL SAMPLE(25)

      ILEN=IFN-IST+1
      JLEN=JFN-JST+1
      NTOT=ILEN*JLEN

      IF(NTOT.LE.25) THEN
         DO J=JST,JFN
            DO I=IST,IFN
               SAMPLE((I-IST+1)+(J-JST)*ILEN)=ARRAY(I,J)
            ENDDO
         ENDDO
         CALL BUBBLE(SAMPLE,NTOT,NTOT)
         RMED=SAMPLE(NTOT/2+1)
      ELSE
         RMED=0.0
      ENDIF

      END
