      subroutine blabels (nplane,col,lab1,lab2,lab3,lab4)
*+
*   BLABELS
*
*   puts button labels to ARGS screen
*
*   Given:
*	NPLANE	I	overlay plane (8-15)
*	COL	C	R,G,B,Y,C,M else black
*	LAB1	C	label for button 1 etc;
*	LAB2	C	labels will be truncated to 12 chars
*	LAB3	C
*	LAB4	C
*
*   JAC/UOE/8Jan81
*-
      integer nplane
      character col*1,lab1*(*),lab2*(*),lab3*(*),lab4*(*)

      call argset(0)

      call args_ovwrt(nplane)
      call args_ovclr(nplane)
      call args_ovcol(nplane,col)
      call args_ovgen('W')

      call blabel1(lab1,5.)
      call blabel1(lab2,133.)
      call blabel1(lab3,261.)
      call blabel1(lab4,389.)

      call devend

      call args_picwrt

      end
