      subroutine args_ovcl (n,dis)

*+  Enable all bit-planes for writing, set ZDI to 'FFFF'X. Disable overlays
*   iff 'dis' is true.

      integer n
      logical dis

*   enable writing to all planes and set ZDI
      call args_flush (2)
      call args_s1 ('ZWE1','FFFF'X)
      call args_s1 ('ZDI1','FFFF'X)
      call srsend

*   disable overlays
      if (dis) then
          call args_ovcg ('0')
          call args_ovc (ishft ('0001'X,iand (n,'0007'X)),'0')
      endif

      end
