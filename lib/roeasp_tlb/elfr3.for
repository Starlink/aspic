      SUBROUTINE ELFR3(I,J,X0,Y0,F1,F2,F3,F4,XEL,YEL,XER,YER,YEB,YET,
     :                 XE1,XE2,XE3,XE4,FRAC)
C+
C   SUBROUTINE ELFR3(I,J,X0,Y0,F1,F2,F3,F4,XEL,YEL,XER,YER,YEB,YET,
C                    XE1,XE2,XE3,XE4,FRAC)
C
C   Calculates the fraction of pixel I,J that lies within the ellipse
C
C   Given       (arguments)
C   I       I   X coordinate of pixel centre
C   J       I   Y coordinate of pixel centre
C   X0      D   X coordinate of centre of ellipse
C   Y0      D   Y coordinate of centre of ellipse
C   F1      D   Parameter of equation of ellipse
C   F2      D   Parameter of equation of ellipse
C   F3      D   Parameter of equation of ellipse
C   F4      D   Parameter of equation of ellipse
C   XEL     D   X coordinate of leftmost point on ellipse
C   YEL     D   Y coordinate of leftmost point on ellipse
C   XER     D   X coordinate of rightmost point on ellipse
C   YER     D   Y coordinate of rightmost point on ellipse
C   YEB     D   Y coordinate of lowest point on ellipse
C   YET     D   Y coordinate of highest point on ellipse
C   XE1     D   Left intercept of ellipse with line y=j+0.5
C   XE2     D   Right intercept of ellipse with line y=j+0.5
C   XE3     D   Right intercept of ellipse with line y=j-0.5
C   XE4     D   Left intercept of ellipse with line y=j-0.5
C
C   Returned    (arguments)
C   FRAC    D   Fraction of pixel lying within the ellipse
C
C   Subroutines called:
C   LINT,TINT,UINT    :E2DLIB
C  
C   D. R. K. Brownrigg/ROE/10.12.1981
C-
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL INSIDE
      FJPH=FLOAT(J)+0.5
      FJMH=FLOAT(J)-0.5
      FIPH=FLOAT(I)+0.5
      FIMH=FLOAT(I)-0.5
      XE1E4=DMAX1(XE1,XE4)
      XE2E3=DMIN1(XE2,XE3)
      IF(XE1E4.NE.0.0.AND.XE1E4.LE.FIMH.AND.XE2E3.NE.0.0
     +               .AND.FIPH.LE.XE2E3) THEN
          INSIDE=.TRUE.
      ELSE
          INSIDE=.FALSE.
      ENDIF
      IF(YEL.LE.FJMH) THEN
          IF(YER.LE.FJMH) THEN
              IF(YET.LE.FJPH) THEN
C ALPHADA
                  IF(XE3.LE.FIMH.OR.FIPH.LE.XE4) THEN
                      FRAC=0.0
                  ELSE
                      XL=DMAX1(FIMH,XE4)
                      XR=DMIN1(FIPH,XE3)
                      CALL UINT(X0,Y0,F1,F2,F3,F4,XL,XR,VALUE)
                      FRAC=VALUE-(XR-XL)*FJMH
                  ENDIF
              ELSE
C ALPHADB
                  IF(FIPH.LE.XE4.OR.XE3.LE.FIMH) THEN
                      FRAC=0.0
                  ELSE IF(INSIDE) THEN
                      FRAC=1.0
                  ELSE
                      XL=DMAX1(XE4,FIMH)
                      XR=DMIN1(XE3,FIPH)
                      CALL UINT(X0,Y0,F1,F2,F3,F4,XL,XR,VALUE1)
                      ADD1=VALUE1-(XR-XL)*FJMH
                      IF(FIPH.LE.XE1.OR.XE2.LE.FIMH) THEN
                          SUB=0.0
                      ELSE
                          XLT=DMAX1(XE1,FIMH)
                          XRT=DMIN1(XE2,FIPH)
                          CALL UINT(X0,Y0,F1,F2,F3,F4,XLT,XRT,VALUE2)
                          SUB=VALUE2-(XRT-XLT)*FJPH
                      ENDIF
                      FRAC=ADD1-SUB
                  ENDIF
              ENDIF
          ELSE
              IF(YER.LT.FJPH) THEN
                  IF(YET.LE.FJPH) THEN
C ALPHAP2A
                      IF(FIPH.LE.XE4.OR.XER.LE.FIMH) THEN
                          FRAC=0.0
                      ELSE
                          XL=DMAX1(XE4,FIMH)
                          XR=DMIN1(XER,FIPH)
                          CALL TINT(X0,F1,F2,F3,F4,XL,XR,VALUE1)
                          IF(XE3.LE.FIMH) THEN
                              SUB=0.0
                          ELSE
                              XRT=DMIN1(XE3,FIPH)
                              CALL LINT(X0,Y0,F1,F2,F3,F4,XL,XRT,VALUE2)
                              SUB=(XRT-XL)*FJMH-VALUE2
                          ENDIF
                          FRAC=2.0*VALUE1-SUB
                      ENDIF
                  ELSE
C ALPHAP2B
                      IF(XER.LE.FIMH.OR.FIPH.LE.XE4) THEN
                          FRAC=0.0
                      ELSE IF (INSIDE) THEN
                          FRAC=1.0
                      ELSE
                          XL=DMAX1(XE4,FIMH)
                          XR=DMIN1(XER,FIPH)
                          CALL TINT(X0,F1,F2,F3,F4,XL,XR,VALUE1)
                          IF(XE2.LE.FIMH.OR.FIPH.LE.XE1) THEN
                              SUB1=0.0
                          ELSE
                              XLT=DMAX1(XL,XE1)
                              XRT=DMIN1(XR,XE2)
                              CALL UINT(X0,Y0,F1,F2,F3,F4,XLT,XRT,
     :                                  VALUE2)
                              SUB1=VALUE2-(XRT-XLT)*FJPH
                          ENDIF
                          IF(XE3.LE.FIMH) THEN
                              SUB2=0.0
                          ELSE
                              XRT=DMIN1(XE3,FIPH)
                              CALL LINT(X0,Y0,F1,F2,F3,F4,XL,XRT,VALUE3)
                              SUB2=(XRT-XL)*FJMH-VALUE3
                          ENDIF
                          FRAC=2.0*VALUE1-SUB1-SUB2
                      ENDIF
                  ENDIF
              ELSE
C ALPHAP1
                  IF(XE2.LE.FIMH.OR.FIPH.LE.XE4) THEN
                      FRAC=0.0
                  ELSE IF(INSIDE) THEN
                      FRAC=1.0
                  ELSE
                      XL=DMAX1(FIMH,XE4)
                      XR=DMIN1(FIPH,XE2)
                      CALL TINT(X0,F1,F2,F3,F4,XL,XR,VALUE1)
                      IF(FIPH.LE.XE1) THEN
                          SUB1=0.0
                      ELSE
                          XLT=DMAX1(FIMH,XE1)
                          CALL UINT(X0,Y0,F1,F2,F3,F4,XLT,XR,VALUE2)
                          SUB1=VALUE2-(XR-XLT)*FJPH
                      ENDIF
                      IF(XE3.LE.FIMH) THEN
                          SUB2=0.0
                      ELSE
                          XRT=DMIN1(FIPH,XE3)
                          CALL LINT(X0,Y0,F1,F2,F3,F4,XL,XRT,VALUE3)
                          SUB2=(XRT-XL)*FJMH-VALUE3
                      ENDIF
                      FRAC=2.0*VALUE1-SUB1-SUB2
                  ENDIF
              ENDIF
          ENDIF
      ELSE
          IF(YEL.LT.FJPH) THEN
              IF(YER.LE.FJMH) THEN
                  IF(YET.LE.FJPH) THEN
C ALPHAM3A
                      IF(FIPH.LE.XEL.OR.XE3.LE.FIMH) THEN
                          FRAC=0.0
                      ELSE
                          XL=DMAX1(XEL,FIMH)
                          XR=DMIN1(XE3,FIPH)
                          CALL TINT(X0,F1,F2,F3,F4,XL,XR,VALUE1)
                          IF(FIPH.LE.XE4) THEN
                              SUB=0.0
                          ELSE
                              XLT=DMAX1(FIMH,XE4)
                              CALL LINT(X0,Y0,F1,F2,F3,F4,XLT,XR,VALUE2)
                              SUB=(XR-XLT)*FJMH-VALUE2
                          ENDIF
                          FRAC=2.0*VALUE1-SUB
                      ENDIF
                  ELSE
C ALPHAM3B
                      IF(XE3.LE.FIMH.OR.FIPH.LE.XEL) THEN
                          FRAC=0.0
                      ELSE IF(INSIDE) THEN
                          FRAC=1.0
                      ELSE
                          XL=DMAX1(XEL,FIMH)
                          XR=DMIN1(XE3,FIPH)
                          CALL TINT(X0,F1,F2,F3,F4,XL,XR,VALUE1)
                          IF(XE2.LE.FIMH.OR.FIPH.LE.XE1) THEN
                              SUB1=0.0
                          ELSE
                              XLT=DMAX1(XL,XE1)
                              XRT=DMIN1(XR,XE2)
                              CALL UINT(X0,Y0,F1,F2,F3,F4,XLT,XRT,
     :                                  VALUE2)
                              SUB1=VALUE2-(XRT-XLT)*FJPH
                          ENDIF
                          IF(FIPH.LE.XE4) THEN
                              SUB2=0.0
                          ELSE
                              XLT=DMAX1(XL,XE4)
                              CALL LINT(X0,Y0,F1,F2,F3,F4,XLT,XR,VALUE3)
                              SUB2=(XR-XLT)*FJMH-VALUE3
                          ENDIF
                          FRAC=2.0*VALUE1-SUB1-SUB2
                      ENDIF
                  ENDIF
              ELSE
                  IF(YER.LT.FJPH) THEN
                      IF(YET.LE.FJPH) THEN
                          IF(FJMH.LE.YEB) THEN
C ALPHAIA
                              IF(XER.LE.FIMH.OR.FIPH.LE.XEL) THEN
                                  FRAC=0.0
                              ELSE
                                  XL=DMAX1(FIMH,XEL)
                                  XR=DMIN1(FIPH,XER)
                                  CALL TINT(X0,F1,F2,F3,F4,XL,XR,VALUE)
                                  FRAC=2.0*VALUE
                              ENDIF
                          ELSE
C ALPHAIC
                              IF(XER.LE.FIMH.OR.FIPH.LE.XEL) THEN
                                  FRAC=0.0
                              ELSE
                                  XL=DMAX1(XEL,FIMH)
                                  XR=DMIN1(XER,FIPH)
                                  CALL TINT(X0,F1,F2,F3,F4,XL,XR,VALUE1)
                                  IF(XE3.LE.FIMH.OR.FIPH.LE.XE4) THEN
                                      SUB=0.0
                                  ELSE
                                      XLT=DMAX1(XL,XE4)
                                      XRT=DMIN1(XR,XE3)
                                      CALL LINT(X0,Y0,F1,F2,F3,F4,XLT,
     :                                          XRT,VALUE2)
                                      SUB=(XRT-XLT)*FJMH-VALUE2
                                  ENDIF
                                  FRAC=2.0*VALUE1-SUB
                              ENDIF
                          ENDIF
                      ELSE
                          IF(FJMH.LE.YEB) THEN
C ALPHAIB
                              IF(XER.LE.FIMH.OR.FIPH.LE.XEL) THEN
                                  FRAC=0.0
                              ELSE
                                  XL=DMAX1(XEL,FIMH)
                                  XR=DMIN1(XER,FIPH)
                                  CALL TINT(X0,F1,F2,F3,F4,XL,XR,VALUE1)
                                  IF(XE2.LE.FIMH.OR.FIPH.LE.XE1) THEN
                                      SUB=0.0
                                  ELSE
                                      XLT=DMAX1(XL,XE1)
                                      XRT=DMIN1(XR,XE2)
                                      CALL UINT(X0,Y0,F1,F2,F3,F4,XLT, 
     :                                          XRT,VALUE2)
                                      SUB=VALUE2-(XRT-XLT)*FJPH
                                  ENDIF
                                  FRAC=2.0*VALUE1-SUB
                              ENDIF
                          ELSE
C ALPHAID
                              IF(XER.LE.FIMH.OR.FIPH.LE.XEL) THEN
                                  FRAC=0.0
                              ELSE IF(INSIDE) THEN
                                  FRAC=1.0
                              ELSE
                                  XL=DMAX1(XEL,FIMH)
                                  XR=DMIN1(XER,FIPH)
                                  CALL TINT(X0,F1,F2,F3,F4,XL,XR,VALUE1)
                                  ADD1=2.0*VALUE1
                                  IF(XE3.LE.FIMH.OR.FIPH.LE.XE4) THEN
                                      SUB1=0.0
                                  ELSE
                                      XLT=DMAX1(XL,XE4)
                                      XRT=DMIN1(XR,XE3)
                                      CALL LINT(X0,Y0,F1,F2,F3,F4,XLT,
     :                                          XRT,VALUE2)
                                      SUB1=(XRT-XLT)*FJMH-VALUE2
                                  ENDIF
                                  IF(XE2.LE.FIMH.OR.FIPH.LE.XE1) THEN
                                      SUB2=0.0
                                  ELSE
                                      XLT=DMAX1(XL,XE1)
                                      XRT=DMIN1(XR,XE2)
                                      CALL UINT(X0,Y0,F1,F2,F3,F4,XLT,
     :                                          XRT,VALUE3)
                                      SUB2=VALUE3-(XRT-XLT)*FJPH
                                  ENDIF
                                  FRAC=ADD1-SUB1-SUB2
                              ENDIF
                          ENDIF
                      ENDIF
                  ELSE
                      IF(FJMH.LE.YEB) THEN
C ALPHAP3A
                          IF(XE2.LE.FIMH.OR.FIPH.LE.XEL) THEN
                              FRAC=0.0
                          ELSE
                              XL=DMAX1(XEL,FIMH)
                              XR=DMIN1(XE2,FIPH)
                              CALL TINT(X0,F1,F2,F3,F4,XL,XR,VALUE1)
                              IF(FIPH.LE.XE1) THEN
                                  SUB=0.0
                              ELSE
                                  XLT=DMAX1(XL,XE1)
                                  CALL UINT(X0,Y0,F1,F2,F3,F4,XLT,XR,
     :                                      VALUE2)
                                  SUB=VALUE2-(XR-XLT)*FJPH
                              ENDIF
                              FRAC=2.0*VALUE1-SUB
                          ENDIF
                      ELSE
C ALPHAP3B
                          IF(XE2.LE.FIMH.OR.FIPH.LE.XEL) THEN
                              FRAC=0.0
                          ELSE IF(INSIDE) THEN
                              FRAC=1.0
                          ELSE
                              XL=DMAX1(XEL,FIMH)
                              XR=DMIN1(XE2,FIPH)
                              CALL TINT(X0,F1,F2,F3,F4,XL,XR,VALUE1)
                              IF(FIPH.LE.XE1) THEN
                                  SUB1=0.0
                              ELSE
                                  XLT=DMAX1(XL,XE1)
                                  CALL UINT(X0,Y0,F1,F2,F3,F4,XLT,XR,
     :                                      VALUE2)
                                  SUB1=VALUE2-(XR-XLT)*FJPH
                              ENDIF
                              IF(XE3.LE.FIMH.OR.FIPH.LE.XE4) THEN
                                  SUB2=0.0
                              ELSE
                                  XLT=DMAX1(XL,XE4)
                                  XRT=DMIN1(XR,XE3)
                                  CALL LINT(X0,Y0,F1,F2,F3,F4,XLT, 
     :                                      XRT,VALUE3)
                                  SUB2=(XRT-XLT)*FJMH-VALUE3
                              ENDIF
                              FRAC=2.0*VALUE1-SUB1-SUB2
                          ENDIF
                      ENDIF
                  ENDIF
              ENDIF
          ELSE
              IF(YER.LE.FJMH) THEN
C ALPHAM1
                  IF(XE3.LE.FIMH.OR.FIPH.LE.XE1) THEN
                      FRAC=0.0
                  ELSE IF(INSIDE) THEN
                      FRAC=1.0
                  ELSE
                      XL=DMAX1(XE1,FIMH)
                      XR=DMIN1(XE3,FIPH)
                      CALL TINT(X0,F1,F2,F3,F4,XL,XR,VALUE1)
                      IF(XE2.LE.FIMH) THEN
                          SUB1=0.0
                      ELSE
                          XRT=DMIN1(XE2,FIPH)
                          CALL UINT(X0,Y0,F1,F2,F3,F4,XL,XRT,VALUE2)
                          SUB1=VALUE2-(XRT-XL)*FJPH
                      ENDIF
                      IF(FIPH.LE.XE4) THEN
                          SUB2=0.0
                      ELSE
                          XLT=DMAX1(XL,XE4)
                          CALL LINT(X0,Y0,F1,F2,F3,F4,XLT,XR,VALUE3)
                          SUB2=(XR-XLT)*FJMH-VALUE3
                      ENDIF
                      FRAC=2.0*VALUE1-SUB1-SUB2
                  ENDIF
              ELSE
                  IF(YER.LT.FJPH) THEN
                      IF(FJMH.LE.YEB) THEN
C ALPHAM2A
                          IF(XER.LE.FIMH.OR.FIPH.LE.XE1) THEN
                              FRAC=0.0
                          ELSE
                              XL=DMAX1(XE1,FIMH)
                              XR=DMIN1(XER,FIPH)
                              CALL TINT(X0,F1,F2,F3,F4,XL,XR,VALUE1)
                              IF(XE2.LE.FIMH) THEN
                                  SUB=0.0
                              ELSE
                                  XRT=DMIN1(FIPH,XE2)
                                  CALL UINT(X0,Y0,F1,F2,F3,F4,XL,XRT,
     :                                      VALUE2)
                                  SUB=VALUE2-(XRT-XL)*FJPH
                              ENDIF
                              FRAC=2.0*VALUE1-SUB
                          ENDIF
                      ELSE
C ALPHAM2B
                          IF(XER.LE.FIMH.OR.FIPH.LE.XE1) THEN
                              FRAC=0.0
                          ELSE IF(INSIDE) THEN
                              FRAC=1.0
                          ELSE
                              XL=DMAX1(XE1,FIMH)
                              XR=DMIN1(XER,FIPH)
                              CALL TINT(X0,F1,F2,F3,F4,XL,XR,VALUE1)
                              IF(XE2.LE.FIMH) THEN
                                  SUB1=0.0
                              ELSE
                                  XRT=DMIN1(XR,XE2)
                                  CALL UINT(X0,Y0,F1,F2,F3,F4,XL,XRT, 
     :                                      VALUE2)
                                  SUB1=VALUE2-(XRT-XL)*FJPH
                              ENDIF
                              IF(FIPH.LE.XE4.OR.XE3.LE.FIMH) THEN
                                  SUB2=0.0
                              ELSE
                                  XLT=DMAX1(XL,XE4)
                                  XRT=DMIN1(XR,XE3)
                                  CALL LINT(X0,Y0,F1,F2,F3,F4,XLT,
     :                                      XRT,VALUE3)
                                  SUB2=(XRT-XLT)*FJMH-VALUE3
                              ENDIF
                              FRAC=2.0*VALUE1-SUB1-SUB2
                          ENDIF
                      ENDIF
                  ELSE
                      IF(FJMH.LE.YEB) THEN
C ALPHAUA
                          IF(XE2.LE.FIMH.OR.FIPH.LE.XE1) THEN
                              FRAC=0.0
                          ELSE
                              XL=DMAX1(FIMH,XE1)
                              XR=DMIN1(FIPH,XE2)
                              CALL LINT(X0,Y0,F1,F2,F3,F4,XL,XR,VALUE)
                              FRAC=(XR-XL)*FJPH-VALUE
                          ENDIF
                      ELSE
C ALPHAUB
                          IF(XE2.LE.FIMH.OR.FIPH.LE.XE1) THEN
                              FRAC=0.0
                          ELSE IF(INSIDE) THEN
                              FRAC=1.0
                          ELSE
                              XL=DMAX1(XE1,FIMH)
                              XR=DMIN1(XE2,FIPH)
                              CALL LINT(X0,Y0,F1,F2,F3,F4,XL,XR,VALUE1)
                              ADD1=(XR-XL)*FJPH-VALUE1
                              IF(XE3.LE.FIMH.OR.FIPH.LE.XE4) THEN
                                  SUB=0.0
                              ELSE
                                  XLT=DMAX1(XL,XE4)
                                  XRT=DMIN1(XR,XE3)
                                  CALL LINT(X0,Y0,F1,F2,F3,F4,XLT, 
     :                                      XRT,VALUE2)
                                  SUB=(XRT-XLT)*FJMH-VALUE2
                              ENDIF
                              FRAC=ADD1-SUB
                          ENDIF
                      ENDIF
                  ENDIF
              ENDIF
          ENDIF
      ENDIF
      RETURN
      END
