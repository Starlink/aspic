      subroutine args_curop (lamps,col)

*+  Prepare ARGS cursor. 'lamps' is a character string and lamp 'i' will
*   be switched on iff 'lamps' contains the character 'i'. The cursor will
*   be of colour 'col' (see 'args_curc').

      integer l1,l2,l3,l4
      character lamps*(*),col*(*)

*   reset VSR
      call args_vsrrst

*   select cursor colour
      call args_curc (col)

*   enable system crosshair cursor and position at ARGS screen centre
      call args_curs ('+')
      call args_curp (0,256,256)

*   light appropriate lamps
      l1 = index (lamps,'1')
      l2 = index (lamps,'2')
      l3 = index (lamps,'3')
      l4 = index (lamps,'4')
      call args_lamps (l1,l2,l3,l4)

      end
