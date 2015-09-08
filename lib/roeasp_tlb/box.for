      SUBROUTINE BOX (XMIN,XMAX,YMIN,YMAX)                                      
C+
C   BOX
C
C     plots box in FINGS
C
C     JAC/UOE/03SEP81
C-
      REAL XMIN,XMAX,YMIN,YMAX
 
      CALL MOVTO2(XMIN,YMIN)
      CALL LINTO2(XMAX,YMIN)
      CALL LINTO2(XMAX,YMAX)
      CALL LINTO2(XMIN,YMAX)
      CALL LINTO2(XMIN,YMIN)
 
      END
 
 
C****************************************************************
C****************************************************************
