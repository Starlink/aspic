      SUBROUTINE COORDS2(NDIMS,NUM,COORDS,NPTS)
*+
*    COORDS2
*
*    purpose : to allow a user to specify up to NUM positions in the
*    current image using the cursor.
*	
*    Arguments :
*
*    Given    Type  Usage
*    NDIMS     I    no. of dimensions (assumed = 2)
*    NUM       I    maximum no. of positions allowed.
*
*    Returned  Type Usage
*    COORDS    RA   coordinates of positions specified.
*    NPTS      I    number of positions specified
*
*    Subroutines called :
*    XCUR               : E2DLIB
*
*    B.D KELLY/ROE/1981
*    D.W.T.Baines/ROE/April 1983
*-
      INTEGER NDIMS,NUM
      REAL COORDS(NDIMS,NUM)
      CHARACTER*78 OBUFF
      LOGICAL ABORT
*
*    set whole of array COORDS to zeros
*
      DO J=1,NUM
        DO I=1,NDIMS
          COORDS(I,J)=0.0
        ENDDO
      ENDDO
      NPTS=0
      X = 255.
      Y = 255.
      WRITE(OBUFF,'('' Up to 500 positions can be specified'',
     :      '' using the cursor'')')
      CALL WRUSER(OBUFF,ISTAT)
      ABORT = .FALSE.
      DO WHILE((.NOT. ABORT) .AND. (NPTS .LT. NUM))
         WRITE(OBUFF,'('' Use cursor to input position number'',
     :         I5)') NPTS+1
        CALL WRUSER(OBUFF,ISTAT)
        CALL XCUR(X,Y,ABORT)
        IF(.NOT. ABORT) THEN
           NPTS=NPTS+1
           COORDS(1,NPTS)=X
           COORDS(2,NPTS)=Y
           WRITE(OBUFF,'('' X = '',F8.1,'' , Y = '',F8.1)') X,Y
           CALL WRUSER(OBUFF,ISTAT)
        ENDIF
      ENDDO
      WRITE(OBUFF,'(I5,'' positions measured.'')') NPTS
      CALL WRUSER(OBUFF,ISTAT)
      END
