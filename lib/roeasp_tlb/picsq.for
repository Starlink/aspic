      SUBROUTINE PICSQ(IXEXT,IYEXT,ILARGE,ARRAY)
*+
*   PICSQ
*
*   Pads out a non-square image to fill a square array.
*
*   Given         (arguments)
*   IXEXT     I    X-extent of image
*   IYEXT     I    Y-extent of image
*   ILARGE    I    dimension of square array
*   ARRAY     RA   square array containing non-square image.
*
*   Returned      (arguments)
*   ARRAY     RA   array containing padded image
*
*   Subroutines called :
*   HIST1              : E2DLIB
*
*   D.R.K.Brownrigg/ROE/1981
*   B.D.Kelly/ROE/2.12.1981
*-

      INTEGER IXEXT,IYEXT,ILARGE
      REAL ARRAY(ILARGE,ILARGE)

      VMAX=1.0E19
      VMIN=-VMAX
      CALL WRUSER('PICTURE NOT SQUARE',ERROR)
      CALL WRUSER('IMAGE PARAMETERS ARE',ERROR)
      CALL HISDATA(ILARGE,ILARGE,ARRAY,1,IXEXT,1,IYEXT)
      CALL READR('REPLY','TYPE FILL-IN VALUE TO SQUARE PICTURE',
     :          0.0,VMIN,VMAX,AVAL,IST)
      IF(IXEXT.LT.IYEXT) THEN
         DO J=1,IYEXT
            DO I=IXEXT+1,IYEXT
               ARRAY(I,J)=AVAL
            ENDDO
         ENDDO
      ENDIF
      IF(IYEXT.LT.IXEXT) THEN
         DO J=IYEXT+1,IXEXT
            DO I=1,IXEXT
               ARRAY(I,J)=AVAL
            ENDDO
         ENDDO
      ENDIF

      END
