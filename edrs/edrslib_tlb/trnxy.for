      subroutine trnxy(xin,yin,nxy,tr,xout,yout)
 
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO APPLY A 6 PARAMETER TRANSFORMATION TO A SET OF X,Y POSITIONS
*
*METHOD
*       CALCULATE THE TRANSFORMED POSITIONS, THEN COPY TO OUTPUT ARRAY
*       SO THAT OUTPUT ARRAY MAY BE THE SAME AS INPUT
*
*ARGUMENTS
*       XIN,YIN (IN)
*       REAL(*)
*               THE INPUT X,Y COORDINATES
*       NXY (IN)
*       INTEGER
*               THE NUMBER OF COORDINATE PAIRS TO BE TRANSFORMED
*       TR (IN)
*       REAL(6)
*               THE TRANSFORMATION COEFFICIENTS TO BE USED
*       XOUT,YOUT (OUT)
*       REAL(*)
*               THE OUTPUT X,Y COORDINATES
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
*
*
* ARRAYS ARE DIMENSIONED (*) SO THAT A CALL WITH NXY=0 DOES NOT CAUSE
* AN ADJUSTABLE ARRAY DIMENSION ERROR, BUT SIMPLY DOES NOTHING
*
      real xin(*),yin(*),tr(6),xout(*),yout(*)
 
*
* CALCULATE THE TRANSFORMED COORDINATES FOR EACH POSITION
*
 
      do 1 i=1,nxy
         x=tr(1)+tr(2)*xin(i)+tr(3)*yin(i)
         y=tr(4)+tr(5)*xin(i)+tr(6)*yin(i)
 
*
* COPY TRANSFORMED POSITION TO OUTPUT (THIS ALLOWS OUTPUT TO BE THE
* SAME ARRAYS AS INPUT)
*
         xout(i)=x
         yout(i)=y
1     continue
 
 
      end
 
 
 
