	SUBROUTINE LAGIN (X,Y,XINT,YINT,N,M,NPTS,NINT)
C+
C       LAGIN.
C 
C       Subroutine to perform Lagrangian interpolation.
C
C  Given;
C   X     (RA) X Coords for original array.
C   Y     (RA) Y   "     "     "       "  .
C   XINT  (RA) X Coords. for interpolated array.
C   N     (I)  Size of arrays XINT and YINT.
C   M     (I)   "   "    "    X and Y.
C   NPTS  (I)  No. of pts. in original arrays.
C   NINT  (I)  No. of pts. to be interpolated.
C
C  Returned;
C   YINT  (RA) Set of Y values interpolated.
C
C  A C Davenhall./St Andrews/                              Spring 79.
C  A C Davenhall./ROE/          {Modified}                 4/8/82.
C-
         DIMENSION X(M),Y(M)
	DIMENSION XINT(N),YINT(N)
         DATA IZ,IONE,ITWO,I3,I4,ONE,ZERO/0,1,2,3,4,1.0E0,0.0E0/
         NPTS1=NPTS-IONE
         DO 1 I=1,NINT
C
         IF (XINT(I).LT.X(1)) GOTO 11
C
         IF (XINT(I).LT.X(2)) GOTO 4
         DO 2 J=3,NPTS1
	 IF (XINT(I).LT.X(J)) GOTO 5
 2	CONTINUE
	 YINT(I)=RLINT(X(NPTS1),X(NPTS),Y(NPTS1),Y(NPTS),XINT(I))
	 GOTO 10
4	YINT(I)=RLINT(X(1),X(2),Y(1),Y(2),XINT(I))
	 GOTO 10
C
11	YINT(I)=Y(1)
	 GOTO 10
C
5	IBASE=J-ITWO
         IBASE2=IBASE+I3
	 YINTT=ZERO
	 DO 7 K=IBASE,IBASE2
	 SUM=ONE
	 DO 8 KK=IBASE,IBASE2
	 IF (KK.EQ.K) GOTO 9
	 SUM=SUM*(XINT(I)-X(KK))/(X(K)-X(KK))
 9	CONTINUE
 8	CONTINUE
 7	YINTT=YINTT+(SUM*Y(K))
	 YINT(I)=YINTT
 10	CONTINUE
 1	CONTINUE
	 RETURN
	 END
