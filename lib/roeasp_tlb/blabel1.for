      subroutine blabel1 (label,lhs)
*+
*   BLABEL1
*
*   Draws one button label for BLABELS
*
*   Given:
*	LABEL	C	label
*	LHS	R	position of lhs of label box
*
*   JAC/UOE/8Jan81
*-
      character label*(*),tlabel*14
      real lhs,rhs
      integer length

      rhs=lhs+117.

      call box (lhs,rhs,5.,25.)

*   truncate label if required.....
      length=len(label)

      if (length.gt.12) then
         tlabel=label(1:12)//'*.'
      else
         tlabel=label//'*.'
      endif

*   write it.....
      call movby2 (5.,5.)
      call chahol (%ref(tlabel))

      end
