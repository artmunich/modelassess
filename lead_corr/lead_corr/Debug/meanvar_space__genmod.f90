        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 25 08:57:43 2014
        MODULE MEANVAR_SPACE__genmod
          INTERFACE 
            SUBROUTINE MEANVAR_SPACE(NLON,NLAT,LAT,X,AX,SX,VX)
              INTEGER(KIND=4) :: NLAT
              INTEGER(KIND=4) :: NLON
              REAL(KIND=4) :: LAT(NLAT)
              REAL(KIND=4) :: X(NLON,NLAT)
              REAL(KIND=4) :: AX
              REAL(KIND=4) :: SX
              REAL(KIND=4) :: VX
            END SUBROUTINE MEANVAR_SPACE
          END INTERFACE 
        END MODULE MEANVAR_SPACE__genmod
