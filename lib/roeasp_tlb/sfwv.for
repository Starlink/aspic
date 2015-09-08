      SUBROUTINE SFWV (WKID,VXT,VYT)
*+
*
*
*     ---------
*       SFWV
*     ---------
*
*
*	set full workstation viewport
*
*  Given (arguments)
*	WKID (I)		workstation id
*
*  returned (arguments)
*	VXT,VYT (R)		extent of workstation window
*
*  uses GKS to draw on full display area
*  sets wkstn viewport and wkstn window
*
*  D. Tudhope. ROE.  March 1983.
*  B. Mcnally  ROE   November 1983
*-

      INTEGER WKID
      REAL D,DSX,DSY,VXT,VYT,XA,YA
      INTEGER IWKSQ

      CALL GKS_IWKID(WKID,I1,IWKSQ)
      CALL GKS_IMXDS(IWKSQ,DSX,DSY,I1,I2)

*  Normalise the workstation vuport parameters to produce
*  the required workstation window to fill the display
*  surface

      XA=ABS(DSX)
      YA=ABS(DSY)
      D=MAX(XA,YA)
      VXT=DSX/D
      VYT=DSY/D

      CALL GKS_SWKW(WKID,0.0,0.0,VXT,VYT)
      CALL GKS_SWKVW(WKID,DSX,DSY)
      END
