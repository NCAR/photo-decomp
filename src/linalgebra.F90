
   module linalgebra_type

   use musica_constants,    only : ik => musica_ik, dk => musica_dk
   use abs_linalgebra_type, only : abs_linalgebra_t

   implicit none

   public :: linalgebra_t

   type, extends(abs_linalgebra_t) :: linalgebra_t
     contains
     procedure :: SGBCO
     procedure :: SGBFA
     procedure :: SGBSL
     procedure :: SGECO
     procedure :: SGEFA
     procedure :: SGESL
     procedure :: tridiag
   end type linalgebra_t

   integer(ik), PARAMETER :: iZERO  = 0_ik
   integer(ik), PARAMETER :: iONE  = 1_ik
   REAL(dk), PARAMETER ::    rZERO = 0.0_dk
   REAL(dk), PARAMETER ::    rONE  = 1.0_dk

   contains

   SUBROUTINE SGBCO( this, ABD, N, ML, MU, IPVT, RCOND, Z )
!------------------------------------------------------------
!         FACTORS A REAL BAND MATRIX BY GAUSSIAN ELIMINATION 
!         AND ESTIMATES THE CONDITION OF THE MATRIX.
!
!         REVISION DATE:  8/1/82
!         AUTHOR:  MOLER, C. B., (U. OF NEW MEXICO)
!
!     IF  RCOND  IS NOT NEEDED, SGBFA IS SLIGHTLY FASTER.
!     TO SOLVE  A*X = B , FOLLOW SBGCO BY SGBSL.
!------------------------------------------------------------

   use abs_linalgebra_type, only : SASUM, SDOT, SASUM, SAXPY, SSCAL

   class(linalgebra_t), intent(inout) :: this

   INTEGER(ik), intent(in)  :: N, ML, MU
   INTEGER(ik), intent(out) :: IPVT(:)
   REAL(dk), intent(out)    :: RCOND
   REAL(dk), intent(inout)  :: ABD(:,:)
   REAL(dk), intent(out)    :: Z(:)

   REAL(dk)    :: EK, T, WK, WKM
   REAL(dk)    :: ANORM, S, SM, YNORM
   INTEGER(ik) :: IS, INFO, J, JU, K, KB, KP1, L, LA, LM, LZ, M, MM


!                       ** COMPUTE 1-NORM OF A
   ANORM = rZERO
   L = ML + iONE
   IS = L + MU
   DO J = iONE, N
     ANORM = MAX(ANORM, SASUM(L,ABD(IS:,J), 1))
     IF (IS .GT. ML + 1) IS = IS - iONE
     IF (J .LE. MU) L = L + iONE
     IF (J .GE. N - ML) L = L - iONE
   ENDDO

   CALL this%SGBFA(ABD, N, ML, MU, IPVT, INFO)

!     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
!     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  TRANS(A)*Y = E .
!     TRANS(A)  IS THE TRANSPOSE OF A .  THE COMPONENTS OF  E  ARE
!     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF W  WHERE
!     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID
!     OVERFLOW.

!                     ** SOLVE TRANS(U)*W = E
	EK = rONE
	Z(:N) = rZERO

	M = ML + MU + iONE
	JU = iZERO
	DO K = iONE, N
	   IF (Z(K) /= rZERO) EK = SIGN(EK, -Z(K))
	   IF (ABS(EK-Z(K)) > ABS(ABD(M,K))) THEN
	      S = ABS(ABD(M,K))/ABS(EK-Z(K))
	      CALL SSCAL(N, S, Z, iONE)
	      EK = S*EK
	   ENDIF
	   WK = EK - Z(K)
	   WKM = -EK - Z(K)
	   S = ABS(WK)
	   SM = ABS(WKM)
	   IF (ABD(M,K) /= rZERO) THEN
	      WK  = WK /ABD(M,K)
	      WKM = WKM/ABD(M,K)
	   ELSE
	      WK  = rONE
	      WKM = rONE
	   ENDIF
	   KP1 = K + iONE
	   JU = MIN(MAX(JU, MU+IPVT(K)), N)
	   MM = M
	   IF (KP1 <= JU) THEN
	      DO J = KP1, JU
	         MM = MM - iONE
	         SM = SM + ABS(Z(J)+WKM*ABD(MM,J))
	         Z(J) = Z(J) + WK*ABD(MM,J)
	         S = S + ABS(Z(J))
              ENDDO
	      IF (S < SM) THEN
	         T = WKM - WK
	         WK = WKM
	         MM = M
	         DO J = KP1, JU
	            MM = MM - 1
	            Z(J) = Z(J) + T*ABD(MM,J)
                 ENDDO
	      ENDIF
	   ENDIF
	   Z(K) = WK
        ENDDO

	S = rONE / SASUM(N, Z, iONE)
	CALL SSCAL(N, S, Z, iONE)

!                         ** SOLVE TRANS(L)*Y = W
	DO KB = iONE, N
	   K = N + iONE - KB
	   LM = MIN(ML, N-K)
	   IF (K < N) THEN
             Z(K) = Z(K) + SDOT(LM, ABD(M+iONE:,K), iONE, Z(K+iONE:), iONE)
	   ENDIF
	   IF (ABS(Z(K)) > rONE) THEN
	      S = rONE / ABS(Z(K))
	      CALL SSCAL(N, S, Z, iONE)
	   ENDIF
	   L = IPVT(K)
	   T = Z(L)
	   Z(L) = Z(K)
	   Z(K) = T
	ENDDO

	S = rONE / SASUM(N, Z, iONE)
	CALL SSCAL(N, S, Z, iONE)

	YNORM = rONE
!                         ** SOLVE L*V = Y
	DO K = iONE, N
	   L = IPVT(K)
	   T = Z(L)
	   Z(L) = Z(K)
	   Z(K) = T
	   LM = MIN(ML, N-K)
	   IF (K < N) THEN
             CALL SAXPY(LM, T, ABD(M+iONE:,K), iONE, Z(K+iONE:), iONE)
	   ENDIF
	   IF (ABS(Z(K)) > rONE) THEN
	      S = rONE / ABS(Z(K))
	      CALL SSCAL(N, S, Z, iONE)
	      YNORM = S*YNORM
	   ENDIF
	ENDDO

	S = rONE/SASUM(N, Z, iONE)
	CALL SSCAL(N, S, Z, iONE)
	YNORM = S*YNORM
!                           ** SOLVE  U*Z = W
	DO KB = iONE, N
	   K = N + iONE - KB
	   IF (ABS(Z(K)) > ABS(ABD(M,K))) THEN
	      S = ABS(ABD(M,K)) / ABS(Z(K))
	      CALL SSCAL(N, S, Z, iONE)
	      YNORM = S*YNORM
	   ENDIF
	   IF (ABD(M,K) /= rZERO) THEN
              Z(K) = Z(K)/ABD(M,K)
           ELSE
	      Z(K) = rONE
           ENDIF
	   LM = MIN(K, M) - iONE
	   LA = M - LM
	   LZ = K - LM
	   T = -Z(K)
	   CALL SAXPY(LM, T, ABD(LA:,K), iONE, Z(LZ:), iONE)
	ENDDO
!                              ** MAKE ZNORM = 1.0
	S = rONE / SASUM(N, Z, iONE)
	CALL SSCAL(N, S, Z, iONE)
	YNORM = S*YNORM

	IF (ANORM /= rZERO) THEN
           RCOND = YNORM/ANORM
        ELSE
	   RCOND = rZERO
        ENDIF

   END SUBROUTINE SGBCO

   SUBROUTINE SGBFA( this, ABD, N, ML, MU, IPVT, INFO )

   use abs_linalgebra_type, only : SAXPY, SSCAL, ISAMAX

!         FACTORS A REAL BAND MATRIX BY ELIMINATION.

!         REVISION DATE:  8/1/82
!         AUTHOR:  MOLER, C. B., (U. OF NEW MEXICO)

!     SGBFA IS USUALLY CALLED BY SBGCO, BUT IT CAN BE CALLED
!     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.

!     INPUT:  SAME AS 'SGBCO'

!     ON RETURN:

!        ABD,IPVT    SAME AS 'SGBCO'

!        INFO    INTEGER
!                = 0  NORMAL VALUE.
!                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
!                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES
!                     INDICATE THAT SGBSL WILL DIVIDE BY ZERO IF
!                     CALLED.  USE  RCOND  IN SBGCO FOR A RELIABLE
!                     INDICATION OF SINGULARITY.

!     (SEE 'SGBCO' FOR DESCRIPTION OF BAND STORAGE MODE)

!     ROUTINES CALLED:  FROM BLAS:    SAXPY, SSCAL, ISAMAX
!                       FROM FORTRAN: MAX, MIN

   class(linalgebra_t), intent(inout) :: this
   INTEGER(ik), intent(in)   ::  N, ML, MU
   INTEGER(ik), intent(out)  ::  INFO
   INTEGER(ik), intent(out)  ::  IPVT(:)
   REAL(dk), intent(inout)   ::  ABD(:,:)

   REAL(dk)     :: T
   INTEGER(ik)  :: I,I0,J,JU,JZ,J0,J1,K,KP1,L,LM,M,MM,NM1


	M = ML + MU + iONE
	INFO = iZERO
!                        ** ZERO INITIAL FILL-IN COLUMNS
	J0 = MU + 2_ik
	J1 = MIN(N, M) - iONE
	DO JZ = J0, J1
	   I0 = M + iONE - JZ
	   DO I = I0, ML
	      ABD(I,JZ) = rZERO
           ENDDO
        ENDDO
	JZ = J1
	JU = iZERO

!                       ** GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
	NM1 = N - iONE
        DO K = iONE, NM1
           KP1 = K + iONE
!                                  ** ZERO NEXT FILL-IN COLUMN
	   JZ = JZ + iONE
	   IF (JZ <= N) THEN
	      DO I = 1, ML
	         ABD(I,JZ) = rZERO
              ENDDO
	   ENDIF
!                                  ** FIND L = PIVOT INDEX
	   LM = MIN(ML, N-K)
	   L = ISAMAX(LM+iONE, ABD(M:,K), iONE) + M - iONE
	   IPVT(K) = L + K - M

	   IF (ABD(L,K) == rZERO) THEN
!          ** ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
	      INFO = K
	   ELSE
!                                ** INTERCHANGE IF NECESSARY
	      IF (L /= M) THEN
	         T = ABD(L,K)
	         ABD(L,K) = ABD(M,K)
	         ABD(M,K) = T
	      ENDIF
!                                   ** COMPUTE MULTIPLIERS
	      T = -rONE / ABD(M,K)
	      CALL SSCAL(LM, T, ABD(M+iONE:,K), iONE)

!                               ** ROW ELIMINATION WITH COLUMN INDEXING

	      JU = MIN(MAX(JU, MU+IPVT(K)), N)
	      MM = M
	      DO J = KP1, JU
	         L = L - iONE
	         MM = MM - iONE
	         T = ABD(L,J)
	         IF (L /= MM) THEN
	            ABD(L,J) = ABD(MM,J)
	            ABD(MM,J) = T
	         ENDIF
	         CALL SAXPY(LM, T, ABD(M+iONE:,K), iONE, ABD(MM+iONE:,J), iONE)
              ENDDO
           ENDIF
        ENDDO

	IPVT(N) = N
	IF (ABD(M,N) == rZERO) INFO = N

   END SUBROUTINE SGBFA

   SUBROUTINE SGBSL( this, ABD, N, ML, MU, IPVT, B, JOB )

   use abs_linalgebra_type, only : SAXPY, SDOT

!         SOLVES THE REAL BAND SYSTEM
!            A * X = B  OR  TRANSPOSE(A) * X = B
!         USING THE FACTORS COMPUTED BY SBGCO OR SGBFA.

!         REVISION DATE:  8/1/82
!         AUTHOR:  MOLER, C. B., (U. OF NEW MEXICO)

!     INPUT:

!        ABD     REAL(LDA, N)
!                THE OUTPUT FROM SBGCO OR SGBFA.

!        N       INTEGER
!                THE ORDER OF THE ORIGINAL MATRIX.

!        ML      INTEGER
!                NUMBER OF DIAGONALS BELOW THE MAIN DIAGONAL.

!        MU      INTEGER
!                NUMBER OF DIAGONALS ABOVE THE MAIN DIAGONAL.

!        IPVT    INTEGER(N)
!                THE PIVOT VECTOR FROM SBGCO OR SGBFA.

!        B       REAL(N)
!                THE RIGHT HAND SIDE VECTOR.

!        JOB     INTEGER
!                = 0         TO SOLVE  A*X = B ,
!                = NONZERO   TO SOLVE  TRANS(A)*X = B , WHERE
!                            TRANS(A)  IS THE TRANSPOSE.

!     ON RETURN

!        B       THE SOLUTION VECTOR  X .

!     ERROR CONDITION

!        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
!        ZERO ON THE DIAGONAL.  TECHNICALLY, THIS INDICATES SINGULARITY,
!        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
!        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE
!        CALLED CORRECTLY AND IF SBGCO HAS SET RCOND .GT. 0.0
!        OR SGBFA HAS SET INFO .EQ. 0 .

!     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX
!     WITH  P  COLUMNS
!           CALL SGBCO(ABD,LDA,N,ML,MU,IPVT,RCOND,Z)
!           IF (RCOND IS TOO SMALL) GO TO ...
!           DO 10 J = 1, P
!              CALL SGBSL(ABD,LDA,N,ML,MU,IPVT,C(1,J),0)
!        10 CONTINUE

!     ROUTINES CALLED:  FROM BLAS:    SAXPY, SDOT
!                       FROM FORTRAN: MIN

   class(linalgebra_t), intent(inout) :: this

   INTEGER(ik), intent(in) ::  N, ML, MU, JOB
   INTEGER(ik), intent(in) ::  IPVT(:)
   REAL(dk), intent(in)    ::  ABD(:,:)
   REAL(dk), intent(inout) ::  B(:)

	REAL(dk)     :: T
	INTEGER(ik)  :: K,KB,L,LA,LB,LM,M,NM1


        M = MU + ML + iONE
        NM1 = N - iONE
        IF (JOB == 0) THEN
!                               ** JOB = 0 , SOLVE  A * X = B
!                               ** FIRST SOLVE L*Y = B
	   IF (ML /= iZERO) THEN
	      DO K = iONE, NM1
	         LM = MIN(ML, N-K)
	         L = IPVT(K)
	         T = B(L)
	         IF (L /= K) THEN
	            B(L) = B(K)
	            B(K) = T
	         ENDIF
	         CALL SAXPY( LM, T, ABD(M+iONE:,K), iONE, B(K+iONE:), iONE )
	      ENDDO
	   ENDIF
!                           ** NOW SOLVE  U*X = Y
	   DO KB = iONE, N
	      K = N + iONE - KB
	      B(K) = B(K) / ABD(M,K)
	      LM = MIN(K, M) - iONE
	      LA = M - LM
	      LB = K - LM
	      T = -B(K)
	      CALL SAXPY(LM, T, ABD(LA:,K), iONE, B(LB:), iONE)
	   ENDDO
        ELSE
!                          ** JOB = NONZERO, SOLVE  TRANS(A) * X = B
!                                  ** FIRST SOLVE  TRANS(U)*Y = B
	   DO K = iONE, N
	      LM = MIN(K, M) - iONE
	      LA = M - LM
	      LB = K - LM
	      T = SDOT(LM, ABD(LA:,K), iONE, B(LB:), iONE)
	      B(K) = (B(K) - T)/ABD(M,K)
	   ENDDO
!                                  ** NOW SOLVE TRANS(L)*X = Y
	   IF (ML /= iZERO) THEN
	      DO KB = iONE, NM1
	         K = N - KB
	         LM = MIN(ML, N-K)
	         B(K) = B(K) + SDOT(LM, ABD(M+iONE:,K), iONE, B(K+iONE:), iONE)
	         L = IPVT(K)
	         IF (L /= K) THEN
	            T = B(L)
	            B(L) = B(K)
	            B(K) = T
	         ENDIF
	      ENDDO
	   ENDIF
        ENDIF

   END SUBROUTINE SGBSL

   SUBROUTINE SGESL( this, A, N, IPVT, B, JOB )

   use abs_linalgebra_type, only : SAXPY, SDOT

!         SOLVES THE REAL SYSTEM
!            A * X = B  OR  TRANS(A) * X = B
!         USING THE FACTORS COMPUTED BY SGECO OR SGEFA.

!         REVISION DATE:  8/1/82
!         AUTHOR:  MOLER, C. B., (U. OF NEW MEXICO)

!     ON ENTRY

!        A       REAL(LDA, N)
!                THE OUTPUT FROM SGECO OR SGEFA.

!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .

!        IPVT    INTEGER(N)
!                THE PIVOT VECTOR FROM SGECO OR SGEFA.

!        B       REAL(N)
!                THE RIGHT HAND SIDE VECTOR.

!        JOB     INTEGER
!                = 0         TO SOLVE  A*X = B ,
!                = NONZERO   TO SOLVE  TRANS(A)*X = B  WHERE
!                            TRANS(A)  IS THE TRANSPOSE.

!     ON RETURN

!        B       THE SOLUTION VECTOR  X .

!     ERROR CONDITION

!        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
!        ZERO ON THE DIAGONAL.  TECHNICALLY, THIS INDICATES SINGULARITY,
!        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
!        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE
!        CALLED CORRECTLY AND IF SGECO HAS SET RCOND .GT. 0.0
!        OR SGEFA HAS SET INFO .EQ. 0 .

!     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX
!     WITH  P  COLUMNS
!           CALL SGECO(A,LDA,N,IPVT,RCOND,Z)
!           IF (RCOND IS TOO SMALL) GO TO ...
!           DO 10 J = 1, P
!              CALL SGESL(A,LDA,N,IPVT,C(1,J),0)
!        10 CONTINUE

   class(linalgebra_t), intent(inout) :: this

        INTEGER(ik), intent(in) :: N, JOB
        INTEGER(ik), intent(in) :: IPVT(:)
	REAL(dk), intent(in)    :: A(:,:)
	REAL(dk), intent(inout) :: B(:)

	REAL(dk)     :: T
	INTEGER(ik)  :: K,KB,L,NM1


	NM1 = N - iONE
	IF (JOB == iZERO) THEN
!                                 ** JOB = 0 , SOLVE  A * X = B
!                                     ** FIRST SOLVE  L*Y = B
	   DO K = iONE, NM1
	      L = IPVT(K)
	      T = B(L)
	      IF (L /= K) THEN
	         B(L) = B(K)
	         B(K) = T
	      ENDIF
	      CALL SAXPY( N-K, T, A(K+iONE:,K), iONE, B(K+iONE:), iONE )
           ENDDO
!                                    ** NOW SOLVE  U*X = Y
	   DO KB = iONE, N
	      K = N + iONE - KB
	      B(K) = B(K) / A(K,K)
	      T = -B(K)
	      CALL SAXPY( K-iONE, T, A(iONE:,K), iONE, B, iONE )
           ENDDO

	ELSE
!                         ** JOB = NONZERO, SOLVE  TRANS(A) * X = B
!                                    ** FIRST SOLVE  TRANS(U)*Y = B
	   DO K = iONE, N
	      T = SDOT( K-iONE, A(iONE:,K), iONE, B, iONE )
	      B(K) = (B(K) - T) / A(K,K)
           ENDDO
!                                    ** NOW SOLVE  TRANS(L)*X = Y
	   DO KB = iONE, NM1
	      K = N - KB
	      B(K) = B(K) + SDOT( N-K, A(K+iONE:,K), iONE, B(K+iONE:), iONE )
	      L = IPVT(K)
	      IF (L /= K) THEN
	         T = B(L)
	         B(L) = B(K)
	         B(K) = T
	      ENDIF
           ENDDO

	ENDIF

   END SUBROUTINE SGESL

   SUBROUTINE SGECO( this, A, N, IPVT, RCOND, Z )

   use abs_linalgebra_type, only : SAXPY, SSCAL, SDOT, SASUM

!         FACTORS A REAL MATRIX BY GAUSSIAN ELIMINATION
!         AND ESTIMATES THE CONDITION OF THE MATRIX.

!         REVISION DATE:  8/1/82
!         AUTHOR:  MOLER, C. B., (U. OF NEW MEXICO)

!         IF  RCOND  IS NOT NEEDED, SGEFA IS SLIGHTLY FASTER.
!         TO SOLVE  A*X = B , FOLLOW SGECO BY SGESL.

!     ON ENTRY

!        A       REAL(LDA, N)
!                THE MATRIX TO BE FACTORED.

!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .

!     ON RETURN

!        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
!                WHICH WERE USED TO OBTAIN IT.
!                THE FACTORIZATION CAN BE WRITTEN  A = L*U , WHERE
!                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
!                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.

!        IPVT    INTEGER(N)
!                AN INTEGER VECTOR OF PIVOT INDICES.

!        RCOND   REAL
!                AN ESTIMATE OF THE RECIPROCAL CONDITION OF  A .
!                FOR THE SYSTEM  A*X = B , RELATIVE PERTURBATIONS
!                IN  A  AND  B  OF SIZE  EPSILON  MAY CAUSE
!                RELATIVE PERTURBATIONS IN  X  OF SIZE  EPSILON/RCOND .
!                IF  RCOND  IS SO SMALL THAT THE LOGICAL EXPRESSION
!                           1.0 + RCOND .EQ. 1.0
!                IS TRUE, THEN  A  MAY BE SINGULAR TO WORKING
!                PRECISION.  IN PARTICULAR,  RCOND  IS ZERO  IF
!                EXACT SINGULARITY IS DETECTED OR THE ESTIMATE
!                UNDERFLOWS.

!        Z       REAL(N)
!                A WORK VECTOR WHOSE CONTENTS ARE USUALLY UNIMPORTANT.
!                IF  A  IS CLOSE TO A SINGULAR MATRIX, THEN  Z  IS
!                AN APPROXIMATE NULL VECTOR IN THE SENSE THAT
!                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .

!     ROUTINES CALLED:  FROM LINPACK: SGEFA
!                       FROM BLAS:    SAXPY, SDOT, SSCAL, SASUM
!                       FROM FORTRAN: ABS, AMAX1, SIGN

   class(linalgebra_t), intent(inout) :: this

   INTEGER(ik), intent(in)    :: N
   INTEGER(ik), intent(out)   :: IPVT(:)
   REAL(dk), intent(out)      :: RCOND
   REAL(dk), intent(inout)    :: A(:,:)
   REAL(dk), intent(out)      :: Z(:)

   REAL(dk) ::  EK,T,WK,WKM
   REAL(dk) ::  ANORM,S,SM,YNORM
   INTEGER(ik)  :: INFO,J,K,KB,KP1,L

!                        ** COMPUTE 1-NORM OF A
	ANORM = rZERO
	DO J = iONE, N
	   ANORM = MAX( ANORM, SASUM(N,A(iONE:,J),iONE) )
	ENDDO
!                                      ** FACTOR
      CALL this%SGEFA(A,N,IPVT,INFO)

!     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
!     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  TRANS(A)*Y = E .
!     TRANS(A)  IS THE TRANSPOSE OF A .  THE COMPONENTS OF  E  ARE
!     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF W  WHERE
!     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID
!     OVERFLOW.

!                        ** SOLVE TRANS(U)*W = E
	EK = rONE
	DO J = iONE, N
	   Z(J) = rZERO
	ENDDO

	DO K = iONE, N
	   IF (Z(K) .NE. rZERO) EK = SIGN(EK, -Z(K))
	   IF (ABS(EK-Z(K)) .GT. ABS(A(K,K))) THEN
	      S = ABS(A(K,K)) / ABS(EK-Z(K))
	      CALL SSCAL(N, S, Z, iONE)
	      EK = S*EK
	   ENDIF
	   WK = EK - Z(K)
	   WKM = -EK - Z(K)
	   S = ABS(WK)
	   SM = ABS(WKM)
	   IF (A(K,K) .NE. rZERO) THEN
	      WK  = WK  / A(K,K)
	      WKM = WKM / A(K,K)
	   ELSE
	      WK  = rONE
	      WKM = rONE
	   ENDIF
	   KP1 = K + iONE
	   IF (KP1 .LE. N) THEN
	      DO J = KP1, N
	         SM = SM + ABS(Z(J)+WKM*A(K,J))
	         Z(J) = Z(J) + WK*A(K,J)
	         S = S + ABS(Z(J))
	      ENDDO
	      IF (S .LT. SM) THEN
	         T = WKM - WK
	         WK = WKM
	         DO J = KP1, N
	            Z(J) = Z(J) + T*A(K,J)
	         ENDDO
	      ENDIF
	   ENDIF
	   Z(K) = WK
        ENDDO

	S = rONE / SASUM(N, Z, iONE)
	CALL SSCAL(N, S, Z, iONE)
!                                ** SOLVE TRANS(L)*Y = W
	DO KB = iONE, N
	   K = N + iONE - KB
	   IF (K .LT. N) THEN
             Z(K) = Z(K) + SDOT(N-K, A(K+iONE:,K), iONE, Z(K+iONE:), iONE)
	   ENDIF
	   IF (ABS(Z(K)) .GT. rONE) THEN
	      S = rONE/ABS(Z(K))
	      CALL SSCAL(N, S, Z, iONE)
	   ENDIF
	   L = IPVT(K)
	   T = Z(L)
	   Z(L) = Z(K)
	   Z(K) = T
	ENDDO

	S = rONE / SASUM(N, Z, iONE)
	CALL SSCAL(N, S, Z, iONE)
!                                 ** SOLVE L*V = Y
	YNORM = rONE
	DO K = iONE, N
	   L = IPVT(K)
	   T = Z(L)
	   Z(L) = Z(K)
	   Z(K) = T
	   IF (K .LT. N) CALL SAXPY(N-K, T, A(K+iONE:,K), iONE, Z(K+iONE:), iONE)
	   IF (ABS(Z(K)) .GT. rONE) THEN
	      S = rONE/ABS(Z(K))
	      CALL SSCAL(N, S, Z, iONE)
	      YNORM = S*YNORM
	   ENDIF
	ENDDO

	S = rONE / SASUM(N, Z, iONE)
	CALL SSCAL(N, S, Z, iONE)
!                                  ** SOLVE  U*Z = V
	YNORM = S*YNORM
	DO KB = iONE, N
	   K = N + iONE - KB
	   IF (ABS(Z(K)) .GT. ABS(A(K,K))) THEN
	      S = ABS(A(K,K))/ABS(Z(K))
	      CALL SSCAL(N, S, Z, iONE)
	      YNORM = S*YNORM
	   ENDIF
	   IF (A(K,K) .NE. rZERO) Z(K) = Z(K)/A(K,K)
	   IF (A(K,K) .EQ. rZERO) Z(K) = 1.0E0
	   T = -Z(K)
	   CALL SAXPY(K-iONE, T, A(iONE:,K), iONE, Z, iONE)
	ENDDO
!                                   ** MAKE ZNORM = 1.0
	S = rONE / SASUM(N, Z, iONE)
	CALL SSCAL(N, S, Z, iONE)
	YNORM = S*YNORM

	IF (ANORM .NE. rZERO) RCOND = YNORM/ANORM
	IF (ANORM .EQ. rZERO) RCOND = rZERO
	
   END SUBROUTINE SGECO

   SUBROUTINE SGEFA( this, A, N, IPVT, INFO )

   use abs_linalgebra_type, only : SAXPY, SSCAL, ISAMAX

!         FACTORS A REAL MATRIX BY GAUSSIAN ELIMINATION.

!         REVISION DATE:  8/1/82
!         AUTHOR:  MOLER, C. B., (U. OF NEW MEXICO)

!     SGEFA IS USUALLY CALLED BY SGECO, BUT IT CAN BE CALLED
!     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
!     (TIME FOR SGECO) = (1 + 9/N)*(TIME FOR SGEFA) .

!     INPUT:  SAME AS 'SGECO'

!     ON RETURN:

!        A,IPVT  SAME AS 'SGECO'

!        INFO    INTEGER
!                = 0  NORMAL VALUE.
!                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
!                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES
!                     INDICATE THAT SGESL OR SGEDI WILL DIVIDE BY ZERO
!                     IF CALLED.  USE  RCOND  IN SGECO FOR A RELIABLE
!                     INDICATION OF SINGULARITY.

!     ROUTINES CALLED:  FROM BLAS:    SAXPY, SSCAL, ISAMAX

   class(linalgebra_t), intent(inout) :: this

   INTEGER(ik), intent(in)  ::  N
   INTEGER(ik), intent(out) ::  INFO
   INTEGER(ik), intent(out) ::  IPVT(:)
   REAL(dk), intent(inout)  ::  A(:,:)

	REAL(dk)     :: T
	INTEGER(ik)  :: J,K,KP1,L,NM1


!                      ** GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
	INFO = iZERO
	NM1 = N - iONE
	DO K = iONE, NM1
	   KP1 = K + iONE
!                                            ** FIND L = PIVOT INDEX
	   L = ISAMAX( N-K+iONE, A(K:,K), iONE) + K-iONE
	   IPVT(K) = L

	   IF (A(L,K) .EQ. rZERO) THEN
!                                     ** ZERO PIVOT IMPLIES THIS COLUMN 
!                                     ** ALREADY TRIANGULARIZED
	      INFO = K
	   ELSE
!                                     ** INTERCHANGE IF NECESSARY
	      IF (L .NE. K) THEN
	         T = A(L,K)
	         A(L,K) = A(K,K)
	         A(K,K) = T
	      ENDIF
!                                     ** COMPUTE MULTIPLIERS
	      T = -rONE / A(K,K)
	      CALL SSCAL( N-K, T, A(K+iONE:,K), iONE )

!                              ** ROW ELIMINATION WITH COLUMN INDEXING
	      DO J = KP1, N
	         T = A(L,J)
	         IF (L .NE. K) THEN
	            A(L,J) = A(K,J)
	            A(K,J) = T
	         ENDIF
	         CALL SAXPY( N-K, T, A(K+iONE:,K), iONE, A(K+iONE:,J), iONE )
	      ENDDO

	   ENDIF
	ENDDO

	IPVT(N) = N
	IF (A(N,N) == rZERO) INFO = N

   END SUBROUTINE SGEFA

   FUNCTION tridiag(this, a, b, c, r) result(u)

   use musica_constants, only : dk => musica_dk
!_______________________________________________________________________
! solves tridiagonal system.  From Numerical Recipies, p. 40
!_______________________________________________________________________

! input:
      REAL(dk), intent(in) :: a(:), b(:), c(:), r(:)
      class(linalgebra_t), intent(in) :: this

! output:
      REAL(dk) :: u(size(b))

! local:
      INTEGER(ik) :: j
      INTEGER(ik) :: n

      REAL(dk) :: bet
      REAL(dk) :: gam(2*size(b))
!_______________________________________________________________________

      IF (b(1) == rZERO) THEN
        write(*,*) 'tridiag: pivot 1 is zero; halting'
        STOP 'tridiag: zero pivot'
      ENDIF
      n = size(b)
      bet   = b(1)
      u(1) = r(1)/bet
      DO j = 2, n   
         gam(j) = c(j - 1)/bet
         bet = b(j) - a(j)*gam(j)
         IF (bet == rZERO) THEN
           write(*,'('' tridiag: pivot '',i4,''is zero; halting'')') j
           STOP 'tridiag: zero pivot'
         ENDIF
         u(j) = (r(j) - a(j)*u(j - 1))/bet
      ENDDO

      DO j = n - 1, 1, -1  
         u(j) = u(j) - gam(j + 1)*u(j + 1)
      ENDDO

      END FUNCTION tridiag

   end module linalgebra_type
