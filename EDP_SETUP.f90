MODULE EDP_SETUP
    IMPLICIT NONE
    !Este módulo inicializa el vector inicial con sus condiciones iniciales y de frontera.
    !También establece los valores iniciales que correspondan de DX, DT y R
    !Además establece los valores de C, K y RHO, y los usa para calcular DX, DT o R.
    
    !K: Conductividad térmica [J/m.s.K]     (Joule / metro segundo Kelvin) puede ser tambien cal o cm.
    !C: Calor específico [J/kg.K]           puede ser tambien cal o cm.
    !RHO: Densidad del material [kg/m]
    !K/(C*RHO) = ALFA = [m²/s] por lo general, pero tienen que coincidir las unidades
    REAL(8), PARAMETER :: C = 900., K = 247., RHO = 2700., PI = 3.14159265359
CONTAINS
    
    !Inicializa los valores del vector U, las dimensiones físicas y el tiempo final.
    !O sea U con cond iniciales y frontera, y XFINAL y TFINAL
    SUBROUTINE INICIALIZAR_EDP(UINI, DX, XFINAL, TFINAL)
        REAL(8), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: UINI
        REAL(8), INTENT(IN) :: DX
        REAL(8), INTENT(OUT) :: XFINAL, TFINAL
        !
        REAL(8) :: X
        INTEGER :: I, N
        
        XFINAL = 0.3
        TFINAL = 15.
        
        N = NINT(XFINAL/DX) + 1 !Redondeo a int y sumo 1.
        ALLOCATE(UINI(N))
        !Condiciones iniciales
!        X = 0.
!        DO I = 2, N-1
!            X = X + DX
!            UINI(I) = SIN(PI * X)
!        END DO
        UINI(:) = 25.
        !Condiciones de frontera
        UINI(1) = 200.;! UINI(N) = 0.;
    END SUBROUTINE
    
    !Se les da valores a dos de las tres variables, la otra queda en 0.
    !Como no tiene sentido que alguna sea 0, esa va a ser la que hay que calcular
    !Basándose en las otras dos. 
    !Esto en vez de armar un menú porque pensé que iba a ser más rápido de implementar.
    SUBROUTINE INICIALIZAR_VALORES_R(DX, DT, R)
        REAL(8), INTENT(OUT) :: DX, DT, R
        !
!        DX = 0D0
        DX = 0.01
!        DT = 0D0
        DT = 0.5
        R = 0D0
!        R =  1.
!        R = 0.5
!        R =  1./6.
    END SUBROUTINE
    
    !---Cálculos relacionados a R---!
    !Usan C, K y RHO del módulo.
    
    FUNCTION CALC_DX(DT, R)
        REAL(8) :: CALC_DX
        REAL(8), INTENT(IN) :: DT, R
        
        CALC_DX = SQRT(CALC_ALFA()*DT/R)
        !CALC_DX = SQRT((K*DT)/(C*RHO*R))
    END FUNCTION
    
    FUNCTION CALC_DT(DX, R)
        REAL(8) :: CALC_DT
        REAL(8), INTENT(IN) :: DX, R
        
        CALC_DT = (R * DX*DX)/CALC_ALFA()
        !CALC_DT = R * (C * RHO * DX*DX)/K
    END FUNCTION

    FUNCTION CALC_R(DX, DT)
        REAL(8) :: CALC_R
        REAL(8), INTENT(IN) :: DX, DT
        
        CALC_R = CALC_ALFA()*DT/(DX*DX)
    END FUNCTION
    
    !Está aclarado arriba, pero: alfa = K/(C*RHO)
    !du/dt = alfa. (d²u/dx²)
    FUNCTION CALC_ALFA()
        REAL(8) :: CALC_ALFA
!         CALC_ALFA = 1D0   
        CALC_ALFA = K/(C*RHO)
        PRINT *, 'Valor de alfa: ', CALC_ALFA
    END FUNCTION
END MODULE
