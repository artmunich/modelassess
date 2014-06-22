        !COMPILER-GENERATED INTERFACE MODULE: Sun Jun 22 07:00:32 2014
        MODULE ERRORVAR__genmod
          INTERFACE 
            SUBROUTINE ERRORVAR(M,N,NT,H1,H2,HNEW)
              INTEGER(KIND=4) :: NT
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=4) :: H1(M,N,NT)
              REAL(KIND=4) :: H2(M,N,NT)
              REAL(KIND=4) :: HNEW(M,N,NT)
            END SUBROUTINE ERRORVAR
          END INTERFACE 
        END MODULE ERRORVAR__genmod
