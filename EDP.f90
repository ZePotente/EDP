PROGRAM EDP
    !Modulo
    USE VYM_IO
    USE VYM_CALCULOS
    USE EDP_SETUP
    USE EDP_TABLA
    
    IMPLICIT NONE
    REAL(8), DIMENSION(:), ALLOCATABLE :: UINI, UFIN
    REAL(8) :: DX, DT, R, TOL
    REAL(8) :: XFINAL, TFINAL
    !
    PRINT *, 'Calculando los valores de DX, DT y R.'
    CALL CALCULOS_DE_R(DX, DT, R)
    CALL MOSTRAR_CALC_R(DX, DT, R)
    
    PRINT *, 'Creando vector inicial U'
    CALL INICIALIZAR_EDP(UINI, DX, XFINAL, TFINAL)
    PRINT *, 'Vector creado'
    
    PRINT *, 'Vector:'
    CALL VEC_MOSTRAR(UINI)
    PRINT *, 'XFINAL = ', XFINAL, 'TFINAL = ', TFINAL
    
    GOTO 20
    IF (R > 0.5) GOTO 20
    PRINT *, 'Empezando método explícito.'
    CALL MET_EXPLICITO(UINI, UFIN, DX, DT, TFINAL, R)
    PRINT *, 'Fin método explícito.'
    GOTO 10
 20 PRINT *, 'R mayor a 0.5, se intenta con método implícito.'
    TOL = 0.00001
    PRINT *, 'Se establece una tolerancia TOL = ', TOL
    PRINT *, 'Empezando método implícito.'
    CALL MET_IMPLICITO(UINI, UFIN, DX, DT, TFINAL, R, TOL)
    PRINT *, 'Fin método implícito.'
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
    SUBROUTINE MET_EXPLICITO(UINI, U, DX, DT, TFINAL, R)
        REAL(8), DIMENSION(:), INTENT(IN) :: UINI
        REAL(8), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: U
        REAL(8), INTENT(IN) :: DX, DT, TFINAL, R
        !
        REAL(8), DIMENSION(:), ALLOCATABLE :: UANT
        REAL(8) :: T
        INTEGER :: I, N
        CHARACTER(*), PARAMETER :: ARCHIVO = 'EDP Explicito.txt'
        
        OPEN(1, FILE = ARCHIVO, ACTION = 'WRITE')
        
        N = SIZE(UINI)
        ALLOCATE(UANT(N), U(N))
        U = UINI
        
        T = 0.
        !Escribo en el primer renglón las referencias de la tabla
        CALL T_CABECERA(DX, N)        
        !Escribo el paso inicial
        CALL T_PASOINICIAL(U, T)
        
        !Para ver si se está yendo al correcto:
        PRINT *, 'R = ', R
        PRINT *, '¿R == 0.5?', R == 0.5
        !El ciclo en sí.
        DO WHILE(T <= TFINAL)
            T = T + DT
            UANT = U
            DO I = 2, N-1
                IF (R == 0.5) THEN !Sé que estoy haciendo un if en cada iteración y no me importa.
                    U(I) = (UANT(I+1) + UANT(I-1)) / 2.
                ELSE
                    U(I) = R*(UANT(I+1) + UANT(I-1)) + (1. - 2.*R) * UANT(I)
                END IF
            END DO
            CALL T_PASO(U, T)
        END DO
        
        CLOSE(1)
        DEALLOCATE(UANT)
    END SUBROUTINE
    
    !---Método Implícito---!
    SUBROUTINE MET_IMPLICITO(UINI, U, DX, DT, TFINAL, R, TOL)
        REAL(8), DIMENSION(:), INTENT(IN) :: UINI
        REAL(8), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: U
        REAL(8), INTENT(IN) :: DX, DT, TFINAL, R, TOL
        !
        REAL(8), DIMENSION(:), ALLOCATABLE :: UANT, UERROR
        REAL(8) :: ERROR, T
        INTEGER :: N, ITER
        INTEGER, PARAMETER :: MAXITER = 1000
        CHARACTER(*), PARAMETER :: ARCHIVO = 'EDP Implicito.txt'
        
        OPEN(1, FILE = ARCHIVO, ACTION = 'WRITE')
        N = SIZE(UINI)
        ALLOCATE(U(N), UANT(N), UERROR(N))
        U = UINI
        
        T = 0.
        !Escribo en el primer renglón las referencias de la tabla
        CALL T_CABECERA(DX, N)        
        !Escribo el paso inicial
        CALL T_PASOINICIAL(U, T)
        
        !Para ver si se está yendo al correcto:
        PRINT *, 'R = ', R
        PRINT *, '¿R == 1?', R == 1.
        !
        DO WHILE(T <= TFINAL)
            T = T + DT
            UANT = U
            ERROR = 2*TOL 
            ITER = 0; ERROR = 2.*TOL !Valor imposible
            DO WHILE(ITER < MAXITER .AND. ERROR >= TOL)
                UERROR = U
                CALL PASONORMAL(U, UANT, R)
                ERROR = VEC_NORMAM(U-UERROR) !porque UANT es el del paso de tiempo anterior
                ITER = ITER + 1
            END DO
            CALL T_PASO(U, T)
        END DO
        CLOSE(1)
    END SUBROUTINE
    
    !Paso del método indirecto.
    SUBROUTINE PASONORMAL(U, UANT, R)
        REAL(8), INTENT(IN) :: UANT(:), R
        REAL(8), INTENT(INOUT) :: U(:)
        !
        REAL(8) :: C
        INTEGER :: I, N
        
        N = SIZE(U)
        DO I = 2, N-1
            IF (R /= 1.) THEN !Sé que estoy haciendo un if en cada iteración y no me importa.
                C = R*UANT(I-1) + (2. - 2.*R)*UANT(I) + R*UANT(I+1)
                U(I) = (C + R*(U(I-1) + U(I+1))) / (2. + 2.*R)
            ELSE
                U(I) = (U(I-1) + U(I+1) + UANT(I-1) + UANT(I+1)) / 4.
            END IF
        END DO
    END SUBROUTINE
END PROGRAM
