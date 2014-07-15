        !COMPILER-GENERATED INTERFACE MODULE: Tue Jul 08 09:29:58 2014
        MODULE GRID__genmod
          INTERFACE 
            SUBROUTINE GRID(M,N,MUL,H1,H2)
              INTEGER(KIND=4) :: MUL
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=4) :: H1(M,N)
              REAL(KIND=4) :: H2(M/MUL,((N-1)/MUL)+1)
            END SUBROUTINE GRID
          END INTERFACE 
        END MODULE GRID__genmod
