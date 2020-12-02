PROGRAM EDP
    !Modulo
    USE VYM_IO
    USE VYM_CALCULOS
    USE EDP_SETUP
    
    IMPLICIT NONE
    REAL(8), DIMENSION(:), ALLOCATABLE :: UINI, UFIN
    REAL(8) :: DX, DT, R
    REAL(8) :: XFINAL, TFINAL
    !
    PRINT *, 'Calculando los valores de DX, DT y R.'
    CALL CALCULOS_DE_R(DX, DT, R)
    CALL MOSTRAR_CALC_R(DX, DT, R)
    
    IF (R > 0.5) GOTO 20
    PRINT *, 'Creando vector inicial U'
    CALL INICIALIZAR_EDP(UINI, DX, XFINAL, TFINAL)
    PRINT *, 'Vector creado'
    
    PRINT *, 'Vector:'
    CALL VEC_MOSTRAR(UINI)
    PRINT *, 'XFINAL = ', XFINAL, 'TFINAL = ', TFINAL
    
    PRINT *, 'Empezando método explícito.'
    CALL MET_EXPLICITO(UINI, UFIN, DX, DT, TFINAL)
    PRINT *, 'Fin método explícito.'
    GOTO 10
 20 PRINT *, 'R inestable, volver a intentar.'
 10 PRINT *, 'Fin.'
CONTAINS
    SUBROUTINE CALCULOS_DE_R(DX, DT, R)
        REAL(8), INTENT(OUT) :: DX, DT, R
        !
        REAL(8), PARAMETER :: ERROR = 1E-10
        !Obtengo los valores dato de las dos que correspondan
        CALL INICIALIZAR_VALORES_R(DX, DT, R)
        !La que hay que calcular está inicializada en 0, y sólo esa en 0, 
        !Ya sé que es ineficiente pero no ocupa lugar asi.
        IF ((ABS(DX) - ERROR) < 0.) DX = CALC_DX(DT, R)
        IF ((ABS(DT) - ERROR) < 0.) DT = CALC_DT(DX, R)
        IF ((ABS(R)  - ERROR) < 0.)  R = CALC_R(DX, DT)
    END SUBROUTINE
    
    SUBROUTINE MOSTRAR_CALC_R(DX, DT, R)
        REAL(8), INTENT(IN) :: DX, DT, R
        
        WRITE(*,'(3(A, F15.10, /))') 'DX = ', DX, 'DT = ', DT, 'R  = ', R
    END SUBROUTINE
    !---Método Explícito---!
    SUBROUTINE MET_EXPLICITO(UINI, U, DX, DT, TFINAL)
        REAL(8), DIMENSION(:), INTENT(IN) :: UINI
        REAL(8), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: U
        REAL(8), INTENT(IN) :: DX, DT, TFINAL
        !
        REAL(8), DIMENSION(:), ALLOCATABLE :: UANT
        REAL(8) :: T, XAUX
        INTEGER :: I, N
        CHARACTER(*), PARAMETER :: ARCHIVO = 'EDP Explicito.txt'
        
        OPEN(1, FILE = ARCHIVO, ACTION = 'WRITE')

        N = SIZE(UINI)
        ALLOCATE(UANT(N), U(N))
        U = UINI
        
        !Escribo en el primer renglon las referencias de la tabla
        WRITE(1, '(A1)', ADVANCE = 'NO') '%'
        WRITE(1, '(A14)', ADVANCE = 'NO') 't\x'
        XAUX = 0.
        DO I = 1, N
            WRITE(1, '(F25.15, A)', ADVANCE = 'NO') XAUX, ' '
            XAUX = XAUX + DX
        END DO
        WRITE(1,'()')
        !
        
        !Escribo el paso inicial
        T = 0.
        WRITE(1,'(F15.10)', ADVANCE = 'NO') T
        DO I = 1, N
                WRITE(1, '(F25.15, A)', ADVANCE = 'NO') U(I), ' '
        END DO
        WRITE(1,'()')
        
        !El ciclo en sí.
        DO WHILE(T <= TFINAL)
            T = T + DT
            UANT = U
            WRITE(1,'(F15.10)', ADVANCE = 'NO') T
            DO I = 2, N-1
                IF (R == 0.5) THEN !Sé que estoy haciendo un if en cada iteración y no me importa.
                    U(I) = (UANT(I+1) + UANT(I-1)) / 2.
                    PRINT *, 'R == 0.5'
                ELSE
                    U(I) = R*(UANT(I+1) + UANT(I-1)) + (1. - 2.*R) * UANT(I)
                    PRINT *, 'R /= 0.5'
                END IF
            END DO
            DO I = 1, N
                WRITE(1, '(F25.15, A)', ADVANCE = 'NO') U(I), ' '
            END DO
            WRITE(1,'()')
        END DO
        
        CLOSE(1)
        DEALLOCATE(UANT)
    END SUBROUTINE
    
    !---Método Implícito---!
    SUBROUTINE MET_IMPLICITO(UINI, U, DX, DT, TFINAL, TOL)
        REAL(8), DIMENSION(:), INTENT(IN) :: UINI
        REAL(8), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: U
        REAL(8), INTENT(IN) :: DX, DT, TFINAL, TOL
        !
        REAL(8), DIMENSION(:), ALLOCATABLE :: UANT, UERROR
        REAL(8) :: ERROR, X, T
        INTEGER :: N, ITER
        INTEGER, PARAMETER :: MAXITER = 1000
        
        N = SIZE(UINI)
        ALLOCATE(U(N), UANT(N), UERROR(N))
        U = UINI
        
        DO WHILE(T <= TFINAL)
            UANT = U
            ERROR = 2*TOL 
            ITER = 0; ERROR = 2.*TOL !Valor imposible
            DO WHILE(ITER < MAXITER .AND. ERROR >= TOL)
                UERROR = U
                CALL PASONORMAL(U, UANT, R)
                ERROR = VEC_NORMAM(U-UERROR) !porque UANT es el del paso de tiempo anterior
                ITER = ITER + 1
            END DO
            
        END DO
    END SUBROUTINE
    
    SUBROUTINE PASONORMAL(U, UANT, R)
        REAL(8), INTENT(IN) :: UANT(:), R
        REAL(8), INTENT(INOUT) :: U(:)
        !
        REAL(8) :: C
        INTEGER :: I, N
        
        N = SIZE(U)
        DO I = 2, N-1
            IF (R == 1.) THEN !Sé que estoy haciendo un if en cada iteración y no me importa.
                C = R*UANT(I-1) + (2.*(-2.)*R)*UANT(I) + R*UANT(I+1)
                U(I) = (C + R*(U(I-1) + U(I+1))) / (2.+2.*R)
            ELSE
                U(I) = (U(I-1) + U(I+1) + UANT(I-1) + UANT(I+1)) / 4.
            END IF
        END DO
    END SUBROUTINE
END PROGRAM
