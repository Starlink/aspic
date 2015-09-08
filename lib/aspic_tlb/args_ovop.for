      subroutine args_ovop (n,col)
*+  Perform all initialisation necessary to allow use of bit plane 'n'
*   for overlays. Overlays will be in colour 'col' (see 'args_decol').

      integer n,maskn
      character col

*   reset VSR
      call args_vsrrst

*   disable overlays temporalily
      call args_ovcg ('0')

*   enable plane 'n', clear it and set ZDI
      call args_cls (n)

*   enable overlays for bit plane 'n'
      call args_ovcg ('W')
      call args_ovc (ishft ('0001'X,iand (n,'0007'X)),col)

      end
