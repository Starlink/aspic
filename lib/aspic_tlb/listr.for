        SUBROUTINE LISTR(ARRY,N1,N2,NORD)
C
        REAL ARRY(N1,N2)
        CHARACTER*80 OUTPUT
        WRITE(OUTPUT,925) NORD
925     FORMAT('ORDER IS ',I1)
        CALL WRUSER(OUTPUT,STATUS)
        DO I = 1,NORD
        WRITE(OUTPUT,900) I,ARRY(1,I),I,ARRY(2,I)
900     FORMAT('   X(',I1,') ', G18.8,'   Y(',I1,') ', G18.8)
        CALL WRUSER(OUTPUT,STATUS)
        ENDDO
        RETURN
        END
