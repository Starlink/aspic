      subroutine args_cls (n)

*+  Clear bit-plane 'n'. Because 'CLS1' does not seem to work, this is done
*   in a rather convoluted fashion. After execution only bit plane 'n' is
*   enabled for writing and ZDI is set to 2**'n'.

      integer n,maskn

      maskn = iand (n,'000F'X)
      call args_flush (9)
      call args_s1 ('ZWE1',ishft ('0001'X,maskn))
      call args_s1 ('ZDI1','0000'X)
      call args_s1 ('SSB',6)
      call args_s1 ('XMA',0)
      call args_s1 ('YMA',0)
      call args_s1 ('XMA',511)
      call args_s1 ('YDA',511)
      call args_s1 ('RSB',6)
      call args_s1 ('ZDI1',ishft ('0001'X,maskn))

*
*   The following line was inserted as the SSB and/or the RSB
*   commands given above seemed to set the size in the ARGS
*   graphics status word to 1 . Therefore we now set it back
*   to zero.
*
      call args_s1 ('SSZ',0)


      call srsend

      end
