        SUBROUTINE ASP_VLTGET(VLUT)

        INTEGER*2 VLUT(512),BUFF(516),RDINS(6)

        INCLUDE 'LIBDIR:ARGS(CARGS1)'

        CALL ARGS_FLUSH(-1)

        RDINS(5)='19FF'X
        RDINS(6)='0000'X

        CALL ARGS_IDCALLRD(1,CHAN,RDINS,6,BUFF,516,NOK)

        DO I=1,512
           VLUT(I)=BUFF(I+4)
        END DO

        RETURN
        END




