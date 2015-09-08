      PROGRAM ASAVE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   ASAVE *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               ASAVE [XLIMITS=I1,I2] [YLIMITS=J1,J2]
C
C
C          FUNCTION:-
C               Saves the currently displayed Args image by copying it into
C               a file on disk.  By default the entire 16 bit image is read
C               from  the  Args  memory  store  into  an  integer*2   file.
C               Optionally,  any  rectangular region of the Args screen may
C               be saved by specifying the X and Y limits of the region.
C
C
C          USE:-
C               Useful for storing an ARGS image  for  later  display.  The
C               image  can  be  redisplayed  most  simply  with the program
C               AFLASH, which simply copies the array back  into  the  Args
C               without  any scaling.  Alternatively, the program ADISP may
C               be used if scaling  limits  of  0  to  255  are  specified.
C               However, ADISP will not redisplay any overlays properly.
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               Name of starlink 2D disk file
C                                             to   be   created  by  ASAVE,
C                                             containing the Args image.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         XLIMITS         0,511               Minimum and  maximum  columns
C                                             on the Args to be saved.
C
C         YLIMITS         0,511               Minimum and maximum  rows  on
C                                             the Args to be saved.
C
C
C
C         W D Pence                AAO                            13-JAN-83
C
C
C--------------------------------------------------------------------------
C
C     COPIES THE ARGS IMAGE AND SAVES IT AS A DISK 2D FILE.
C
      INTEGER IDIMN(2),IX(2),IY(2)
      CALL SRINIT(0,.FALSE.,ISTAT)
      IF (ISTAT .NE. 0)THEN
        CALL WRUSER('FAILED TO GET ARGS',ISTAT)
        STOP
      END IF
C
C     GET BOUNDARYS OF RECTANGULAR AREA TO BE SAVED
C
      GO TO 12
10    CALL CNPAR('XLIMITS',ISTAT)
      CALL CNPAR('YLIMITS',ISTAT)
12    CALL RDKEYI('XLIMITS',.FALSE.,2,IX,NXVALS,ISTAT)
      IF (NXVALS .LT. 2 )THEN
        CALL WRUSER('MUST INPUT MIN AND MAX LIMITS',ISTAT)
        GO TO 10
      END IF
      IF (IX(1) .GE. IX(2))THEN
        CALL WRUSER('FIRST LIMIT MUST BE LESS THAN THE SECOND',
     1       ISTAT)
        GO TO 10
      END IF
      IF (IX(1) .LT. 0 .OR. IX(2) .GT. 511)THEN
        CALL WRUSER('LIMITS MUST BE IN THE RANGE 0 TO 511',ISTAT)
        GO TO 10
      END IF
      CALL RDKEYI('YLIMITS',.FALSE.,2,IY,NYVALS,ISTAT)
      IF (NYVALS .LT. 2)THEN
        CALL WRUSER('MUST INPUT MIN AND MAX LIMITS',ISTAT)
        GO TO 10
      END IF
      IF (IY(1) .GE. IY(2))THEN
        CALL WRUSER('FIRST LIMIT MUST BE LESS THAN THE SECOND',
     1       ISTAT)
        GO TO 10
      END IF
      IF (IY(1) .LT. 0 .OR. IY(2) .GT. 511)THEN
        CALL WRUSER('LIMITS MUST BE IN THE RANGE 0 TO 511',ISTAT)
        GO TO 10
      END IF
C
      IDIMN(1)=IX(2)-IX(1)+1
      IDIMN(2)=IY(2)-IY(1)+1
      NDIMS=2
C
C     CREATE AN OUTPUT ARRAY
C
      CALL WRUSER('Name for new image file = ?',JSTAT)
      CALL WRIMAG('IMAGE',102,IDIMN,NDIMS,IPIN,JSTAT)
C
C     LOAD THE ARGS IMAGE INTO THE ARRAY
C
      CALL SRFPR(%VAL(IPIN),IDIMN(1),IDIMN(1),IDIMN(2),IX(1),IY(1))
C
      END
C***********************************************************************
      SUBROUTINE SRFPR(IB2,JD1,NX,NY,X,Y)
C
C READS RECTANGULAR PIXEL ARRAY FROM THE ARGS. EACH PIXEL VALUE
C IS CONTAINED IN ONE INTEGER*2 ARRAY ELEMENT.
C 1ST SUBSCRIPT INCREASES WITH INCREASING X.
C 2ND SUBSCRIPT CORRESPONDS TO Y
C ARGUMENTS:
C    IB2   (EXIT) 2-D ARRAY CONTAINING PIXEL VALUES.
C    JD1   (ENTRY) 1ST DIMENSION OF ACTUAL ARRAY CORRESPDG TO IB2
C    NX,NY  (ENTRY) NUMBER OF PIXELS IN X- AND Y- DIRECTIONS
C    X,Y    (ENTRY) BOTTOM LEFT OF RECTANGLE
C
      PARAMETER BITS=16
      INTEGER JD1
      INTEGER*2 IB2(JD1,NY)
      INTEGER NX,NY, X,Y
      INTEGER PPE,HLINES
      INTEGER*2 CHECK
      REAL PPB
C   CHECK ARGUMENTS
      CHECK=IB2(1,1)
      CHECK=IB2(JD1,NY)
      CHECK=IB2(NX,NY)
C
C               NUMBER OF (P)IXELS (P)ER ARRAY (E)LEMENT
C               & PER (B)YTE (LATTER IS REAL BECAUSE IT COULD BE <1).
C
      PPE=16/BITS
      PPB=8.0/FLOAT(BITS)
      HLINES=10000
C
C     ADJUST HLINES TO THE SPACE ALLOWED BY ARGS_FPRI2
C
      HLINES=MIN(HLINES,(32767*PPE)/NX, (INT(32767.0*PPB)/NX))
C
C     CAN READ CHUNKS OF POSSIBLY MORE THAN 1 ROW.
C
      DO  JY=1,NY,HLINES
C       (NY2 IS # OF ROWS TO BE PUT THIS ITERATION)
        NY2=MIN(HLINES,NY-JY+1)
        CALL ARGS_FPRI2(IB2(1,JY),NY2*NX,X,Y+JY-1,
     1          NX,NY2)
      END DO
      END
C***********************************************************************
      SUBROUTINE ARGS_FPRI2(BUFF,N,X1,Y1,NX,NY)
C
C READS PACKED PIXEL ARRAY FROM ARGS.
C ARGUMENTS
C   BUFF   (ARRAY) ON OUTPUT CONTAINS PACKED DATA (INTEGER*2)
C   N      (ENTRY) NUMBER OF INTEGER*2 ELEMENTS IN BUFF
C   X1,Y1  (ENTRY) BOTTOM LEFT OF PIXEL IMAGE.
C   NX,NY  (ENTRY) NUMBER OF PIXELS TO BE OCCUPIED IN X & Y RESP.
C
      INTEGER*2 BUFF(N),IBUFF(32771)
      INTEGER X1,Y1,NX,NY
      INTEGER NW,AW,PPAW,NAME
      INTEGER*2 CHECK,IRDINS(10)
C
      INTEGER*2 IJUNK
      LOGICAL LPRT,LARGSD,LARGSS,INITD
      COMMON /CARGS1/IJUNK(520),L1,L2,L3,LPRT,LARGSD,LARGSS,
     1 CHAN,INITD,L4
      DATA NW/1/
      DATA NAME/3HFPR/
C
C   CHECK ARRAY EXTREMITIES
      CHECK=BUFF(1)
      CHECK=BUFF(N)
C   CHECK NUMBER OF WORDS
      IF( N*NW.GT.32767 ) CALL ARGS_ITF(12,.TRUE.,N,N,Z)
C   (P)IXELS (P)ER (A)RGS (W)ORD
      PPAW=1
C   NEED EXACT NUMBER OF ARGS DATA WORDS OR FEWER.
      AW=MIN( N*NW, (NX*NY+PPAW-1)/PPAW)
C
C   MOVE PEN, OUTPUT ARGS ORDER & THEN READ THE IMAGE.
C
      CALL ARGS_S1(3HXMA,X1)
      CALL ARGS_S1(3HYMA,Y1)
C
C     OPCODE FOR READING ARGS = '6B04'
C
      IRDINS(5)='6B04'X
      IRDINS(6)=X1
      IRDINS(7)=Y1
      IRDINS(8)=NX
      IRDINS(9)=NY
      IRDINS(10)=AW
C
C     ARGS_IDCALLRD SENDS 'IRDINS' TO THE ARGS AND RECEIVES
C        THE REPLY IN IBUFF.  THERE ARE 4 HEADER WORDS IN THE
C        BEGINNING OF EACH BUFFER WHICH MUST BE IGNORRED.
C
      CALL ARGS_IDCALLRD(1,CHAN,IRDINS,10,IBUFF,AW+4,NOK)
C
      DO I=1,AW
        BUFF(I)=IBUFF(I+4)
      END DO
      END
