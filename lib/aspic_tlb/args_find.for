      subroutine args_find (device,status)

*+  Translate logical name 'ARGS_DEVICE' to obtain device name

      integer status
      character device*(*)

      call args_trlog ('ARGS_DEVICE',device,status)
      end
