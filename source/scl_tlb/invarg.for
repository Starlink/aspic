!+
!   Procedure:-  INVARG.
!
!
!                       +----------+
!                       |          |
!                       |  INVARG. |
!                       |          |
!                       +----------+
!
!
!  Description:-
!   Command procedure to invert the colour table currently held in the
!   Args.
!
!   The current colour table is read from the Args, inverted and sent
!   back to the Args. The stack is then returned to its previous
!   state.
!
!   A C Davenhall./ROE/                                       15/3/83.
!
!-  ***
LUTSTORE $
LET INVCOL_INCOL  = $
LET INVCOL_OUTCOL = $
LET INVCOL_REPLY  = "INVERTED"
INVCOL
LUTREAD $
POP
$ WRITE SYS$OUTPUT "  "
$ WRITE SYS$OUTPUT "Colour table inverted."
$ WRITE SYS$OUTPUT "  "
