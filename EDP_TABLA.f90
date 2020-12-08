MODULE EDP_TABLA
    IMPLICIT NONE
    CHARACTER(*), PARAMETER :: F_VAL = '(F15.7, A)', F_CUADRO = '(F15.7)'
CONTAINS
!---Subrutinas para escribir el archivo---!
    !Para no complicarme supongo que la unidad ya fue abierta y es la 1.
    SUBROUTINE T_CABECERA(DX, N)
        REAL(8), INTENT(IN) :: DX
        INTEGER, INTENT(IN) :: N
        !
        REAL(8) :: XAUX
        INTEGER :: I
        
        WRITE(1, '(A1)', ADVANCE = 'NO') '%'
        WRITE(1, '(A14)', ADVANCE = 'NO') 't\x'
        XAUX = 0.
        DO I = 1, N
            WRITE(1, F_VAL, ADVANCE = 'NO') XAUX, ' '
            XAUX = XAUX + DX
        END DO
        WRITE(1,'()')
    END SUBROUTINE
    
    !Para no complicarme supongo que la unidad ya fue abierta y es la 1.
    SUBROUTINE T_PASOINICIAL(U, T)
        REAL(8), INTENT(IN) :: U(:), T
        !
        INTEGER :: I, N
        N = SIZE(U)
        
        WRITE(1,F_CUADRO, ADVANCE = 'NO') T
        DO I = 1, N
                WRITE(1, F_VAL, ADVANCE = 'NO') U(I), ' '
        END DO
        WRITE(1,'()')
    END SUBROUTINE
    
    !Para no complicarme supongo que la unidad ya fue abierta y es la 1.
    SUBROUTINE T_PASO(U, T)
        REAL(8), INTENT(IN) :: U(:), T
        !
        INTEGER :: I, N
        N = SIZE(U)
        
        WRITE(1,F_CUADRO, ADVANCE = 'NO') T
        DO I = 1, N
            WRITE(1, F_VAL, ADVANCE = 'NO') U(I), ' '
        END DO
        WRITE(1,'()')
    END SUBROUTINE
END MODULE
