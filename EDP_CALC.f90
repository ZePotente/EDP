MODULE EDP_CALC
    IMPLICIT NONE
    !K: Conductividad térmica
    !C: Capacidad calórica
    !RHO: Densidad del material
    REAL(8), PARAMETER :: C = 0., K = 0., RHO = 0.
CONTAINS
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
