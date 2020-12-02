MODULE EDP_SETUP
    IMPLICIT NONE
    !Este módulo inicializa el vector inicial con sus condiciones iniciales y de frontera.
    !También establece los valores iniciales que correspondan de DX, DT y R
    !Además establece los valores de C, K y RHO, y los usa para calcular DX, DT o R.
    
    !K: Conductividad térmica [J/m.s.K]     (Joule / metro segundo Kelvin) puede ser tambien cal o cm.
    !C: Calor específico [J/kg.K]           puede ser tambien cal o cm.
    !RHO: Densidad del material [kg/m]
    !K/(C*RHO) = ALFA = [m²/s] por lo general, pero tienen que coincidir las unidades
    REAL(8), PARAMETER :: C = 1., K = 1., RHO = 1., PI = 3.14159265359
CONTAINS
    
    !Inicializa los valores del vector U y las dimensiones físicas y temporales iniciales.
    !O sea U con cond iniciales y frontera, y XFINAL y TFINAL
    SUBROUTINE INICIALIZAR_EDP(UINI, DX, XFINAL, TFINAL)
        REAL(8), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: UINI
        REAL(8), INTENT(IN) :: DX
        REAL(8), INTENT(OUT) :: XFINAL, TFINAL
        !
        REAL(8) :: X
        INTEGER :: I, N
        
        XFINAL = 1.
        TFINAL = 4.
        
        N = NINT(XFINAL/DX) + 1 !Redondeo a int y sumo 1.
        ALLOCATE(UINI(N))
        !Condiciones iniciales
        X = 0.
        DO I = 2, N-1
            X = X + DX
            UINI(I) = SIN(PI * X)
        END DO
        
        !Condiciones de frontera
        UINI(1) = 0.; UINI(N) = 0.;
    END SUBROUTINE
    
    !Se les da valores a dos de las tres variables, la otra queda en 0.
    !Como no tiene sentido que alguna sea 0, esa va a ser la que hay que calcular
    !Basándose en las otras dos.
    SUBROUTINE INICIALIZAR_VALORES_R(DX, DT, R)
        REAL(8), INTENT(OUT) :: DX, DT, R
        !
        DX = 0.1
        DT = 0.
        R = 1.0
!        R = 1./6.
    END SUBROUTINE
    
    !---Cálculos relacionados a R---!
    !Usan C, K y RHO del módulo.
    
    FUNCTION CALC_DX(DT, R)
        REAL(8) :: CALC_DX
        REAL(8), INTENT(IN) :: DT, R
        !REAL(8), INTENT(IN), OPTIONAL :: R
        
        CALC_DX = SQRT((K*DT)/(C*RHO*R))
    END FUNCTION
    
    FUNCTION CALC_DT(DX, R)
        REAL(8) :: CALC_DT
        REAL(8), INTENT(IN) :: DX, R
        !REAL(8), INTENT(IN), OPTIONAL :: R
        
            CALC_DT = R * (C * RHO * DX*DX)/K
    END FUNCTION

    FUNCTION CALC_R(DX, DT)
        REAL(8) :: CALC_R
        REAL(8), INTENT(IN) :: DX, DT
        
        CALC_R = K*DT/(C*RHO*DX*DX)
    END FUNCTION
END MODULE
