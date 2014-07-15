        !COMPILER-GENERATED INTERFACE MODULE: Mon Jul 14 22:22:34 2014
        MODULE RMSE_SPACE__genmod
          INTERFACE 
            SUBROUTINE RMSE_SPACE(NLON,NLAT,X,LAT,RMSE)
              INTEGER(KIND=4) :: NLAT
              INTEGER(KIND=4) :: NLON
              REAL(KIND=4) :: X(NLON,NLAT)
              REAL(KIND=4) :: LAT(NLON)
              REAL(KIND=4) :: RMSE
            END SUBROUTINE RMSE_SPACE
          END INTERFACE 
        END MODULE RMSE_SPACE__genmod
