      SUBROUTINE DRACON(X,Y,N1,N2,N3,N4)                                        
C+
C     LOCAL VARIABLES
C
C     DRAWS LINES BETWEEN SELECTED PAIRS OF POINTS X AND Y
C
C	GIVEN:	(ARGUMENTS)
C	X, Y, N1, N2, N3, N4
C
C	CALLS FINGS: MOVTO2, LINTO2
C
C	B.D KELLY/ROE/1981
C-
      REAL X(5),Y(5)
      CALL MOVTO2(X(N1),Y(N1))
      CALL LINTO2(X(N2),Y(N2))
      CALL MOVTO2(X(N3),Y(N3))
      CALL LINTO2(X(N4),Y(N4))
      RETURN
      END
C***************************************************************
C***************************************************************
