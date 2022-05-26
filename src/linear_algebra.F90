! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
module tuvx_linear_algebra

   use musica_constants, only : ik => musica_ik, dk => musica_dk

   implicit none

   private
   public :: abs_linalgebra_t, sscal, saxpy, sasum, sdot, isamax

   integer(ik), parameter :: iZERO = 0_ik
   integer(ik), parameter :: iONE  = 1_ik
   real(dk), parameter    :: rZERO = 0.0_dk

   type, abstract :: abs_linalgebra_t
     contains
     procedure(SGBCO), deferred :: SGBCO
     procedure(SGBFA), deferred :: SGBFA
     procedure(SGBSL), deferred :: SGBSL
     procedure(SGECO), deferred :: SGECO
     procedure(SGEFA), deferred :: SGEFA
     procedure(SGESL), deferred :: SGESL
     procedure(tridiag), deferred :: tridiag
   end type abs_linalgebra_t

   interface
      SUBROUTINE SGBCO( this, ABD, N, ML, MU, IPVT, RCOND, Z )
        use musica_constants, only : ik => musica_ik, dk => musica_dk
        import abs_linalgebra_t
        class(abs_linalgebra_t), intent(inout) :: this
        INTEGER(ik), intent(in)  :: N, ML, MU
        INTEGER(ik), intent(out) :: IPVT(:)
	REAL(dk), intent(out)    :: RCOND
	REAL(dk), intent(inout)  :: ABD(:,:)
	REAL(dk), intent(out)    :: Z(:)
      END SUBROUTINE SGBCO

      SUBROUTINE SGBFA( this, ABD, N, ML, MU, IPVT, INFO )
        use musica_constants, only : ik => musica_ik, dk => musica_dk
        import abs_linalgebra_t
        class(abs_linalgebra_t), intent(inout) :: this
        INTEGER(ik), intent(in)  :: N, ML, MU
        INTEGER(ik), intent(out) ::  INFO
        INTEGER(ik), intent(out) :: IPVT(:)
	REAL(dk), intent(inout)  :: ABD(:,:)
      END SUBROUTINE SGBFA

      SUBROUTINE SGBSL( this, ABD, N, ML, MU, IPVT, B, JOB )
        use musica_constants, only : ik => musica_ik, dk => musica_dk
        import abs_linalgebra_t
        class(abs_linalgebra_t), intent(inout) :: this
        INTEGER(ik), intent(in) ::  N, ML, MU, JOB
        INTEGER(ik), intent(in) ::  IPVT(:)
        REAL(dk), intent(in)    ::  ABD(:,:)
        REAL(dk), intent(inout) ::  B(:)
      END SUBROUTINE SGBSL

      SUBROUTINE SGECO( this, A, N,IPVT, RCOND, Z )
        use musica_constants, only : ik => musica_ik, dk => musica_dk
        import abs_linalgebra_t
        class(abs_linalgebra_t), intent(inout) :: this
        INTEGER(ik), intent(in)  :: N
        INTEGER(ik), intent(out) :: IPVT(:)
	REAL(dk), intent(out)    :: RCOND
        REAL(dk), intent(inout)  :: A(:,:)
	REAL(dk), intent(out)    :: Z(:)
      END SUBROUTINE SGECO

      SUBROUTINE SGEFA( this, A, N, IPVT, INFO )
        use musica_constants, only : ik => musica_ik, dk => musica_dk
        import abs_linalgebra_t
        class(abs_linalgebra_t), intent(inout) :: this
        INTEGER(ik), intent(in)  :: N
        INTEGER(ik), intent(out) :: INFO
        INTEGER(ik), intent(out) :: IPVT(:)
        REAL(dk), intent(inout)  :: A(:,:)
      END SUBROUTINE SGEFA

      SUBROUTINE SGESL( this, A, N, IPVT, B, JOB )
        use musica_constants, only : ik => musica_ik, dk => musica_dk
        import abs_linalgebra_t
        class(abs_linalgebra_t), intent(inout) :: this
        INTEGER(ik), intent(in) :: N, JOB
        INTEGER(ik), intent(in) :: IPVT(:)
        REAL(dk), intent(in)    :: A(:,:)
        REAL(dk), intent(inout) :: B(:)
      END SUBROUTINE SGESL

      FUNCTION tridiag(this, a, b, c, r) result(u)
        use musica_constants, only : dk => musica_dk
        import abs_linalgebra_t
        class(abs_linalgebra_t), intent(in) :: this
        REAL(dk), intent(in) :: a(:), b(:), c(:), r(:)
        REAL(dk) :: u(size(b))
      END FUNCTION tridiag
   end interface

   contains

   INTEGER(ik) FUNCTION ISAMAX( N, SX, INCX )

!  --INPUT--  N  NUMBER OF ELEMENTS IN VECTOR OF INTEREST
!            SX  SING-PREC ARRAY, LENGTH 1+(N-1)*INCX, CONTAINING VECTOR
!          INCX  SPACING OF VECTOR ELEMENTS IN 'SX'

! --OUTPUT-- ISAMAX   FIRST I, I = 1 TO N, TO MAXIMIZE
!                         ABS(SX(1+(I-1)*INCX))

        INTEGER(ik), intent(in) :: N, INCX
	REAL(dk), intent(in)    :: SX(:)

        INTEGER(ik) :: I,II
	REAL(dk) :: SMAX, XMAG

        IF( N <= iZERO ) THEN
          ISAMAX = iZERO
        ELSEIF( N == iONE ) THEN
          ISAMAX = iONE
        ELSE
	   SMAX = rZERO
	   II = iONE
	   DO I = iONE, iONE+(N-1)*INCX, INCX
	      XMAG = ABS(SX(I))
	      IF( SMAX.LT.XMAG ) THEN
	         SMAX = XMAG
	         ISAMAX = II
	      ENDIF
	      II = II + iONE
           ENDDO
        ENDIF

   END FUNCTION  ISAMAX

   REAL(dk) FUNCTION SASUM( N, SX, INCX )

!  --INPUT--  N  NUMBER OF ELEMENTS IN VECTOR TO BE SUMMED
!            SX  SING-PREC ARRAY, LENGTH 1+(N-1)*INCX, CONTAINING VECTOR
!          INCX  SPACING OF VECTOR ELEMENTS IN 'SX'

! --OUTPUT-- SASUM   SUM FROM 0 TO N-1 OF  ABS(SX(1+I*INCX))

        INTEGER(ik), intent(in) :: N, INCX
	REAL(dk), intent(in)    :: SX(:)

        INTEGER(ik) :: I, M

        SASUM = rZERO
        IF( N > iZERO ) THEN
          IF( INCX.NE. iONE ) THEN
!                                          ** NON-UNIT INCREMENTS
	    DO I = iONE, iONE+(N-1)*INCX, INCX
	       SASUM = SASUM + ABS(SX(I))
            ENDDO
          ELSE
!                                          ** UNIT INCREMENTS
	    M = MOD(N,6_ik)
	    IF( M.NE. iZERO ) THEN
!                             ** CLEAN-UP LOOP SO REMAINING VECTOR 
!                             ** LENGTH IS A MULTIPLE OF 6.
	      DO I = iONE, M
	        SASUM = SASUM + ABS(SX(I))
              ENDDO
            ENDIF
!                              ** UNROLL LOOP FOR SPEED
	    DO I = M+iONE, N, 6_ik
	     SASUM = SASUM + ABS(SX(I))   + ABS(SX(I+1_ik)) + ABS(SX(I+2_ik)) &
                           + ABS(SX(I+3_ik)) + ABS(SX(I+4_ik)) + ABS(SX(I+5_ik))
            ENDDO
          ENDIF
        ENDIF

   END FUNCTION SASUM

   REAL(dk) FUNCTION SDOT( N, SX, INCX, SY, INCY )

!          S.P. DOT PRODUCT OF VECTORS  'X'  AND  'Y'

!  --INPUT--
!        N  NUMBER OF ELEMENTS IN INPUT VECTORS 'X' AND 'Y'
!       SX  SING-PREC ARRAY CONTAINING VECTOR 'X'
!     INCX  SPACING OF ELEMENTS OF VECTOR 'X' IN 'SX'
!       SY  SING-PREC ARRAY CONTAINING VECTOR 'Y'
!     INCY  SPACING OF ELEMENTS OF VECTOR 'Y' IN 'SY'

! --OUTPUT--
!     SDOT   SUM FOR I = 0 TO N-1 OF  SX(LX+I*INCX) * SY(LY+I*INCY),
!            WHERE  LX = 1          IF INCX .GE. 0, 
!                      = (-INCX)*N  IF INCX .LT. 0,
!            AND LY IS DEFINED IN A SIMILAR WAY USING INCY.

        INTEGER(ik), intent(in) :: N, INCX, INCY
	REAL(dk), intent(in)    :: SX(:), SY(:)

        INTEGER(ik) :: I, M, IX, IY

        SDOT = rZERO
        IF( N > iZERO ) THEN

          IF ( INCX.EQ.INCY .AND. INCX.GT. iONE )  THEN

	    DO I = iONE, iONE+(N-1)*INCX, INCX
	       SDOT = SDOT + SX(I) * SY(I)
            ENDDO

          ELSE IF ( INCX.EQ.INCY .AND. INCX.EQ.iONE )  THEN

!                                        ** EQUAL, UNIT INCREMENTS
	    M = MOD(N,5_ik)
	    IF( M .NE. iZERO ) THEN
!                            ** CLEAN-UP LOOP SO REMAINING VECTOR LENGTH
!                            ** IS A MULTIPLE OF 4.
	      DO I = iONE, M
	         SDOT = SDOT + SX(I) * SY(I)
              ENDDO
	    ENDIF
!                              ** UNROLL LOOP FOR SPEED
	    DO I = M+iONE, N, 5_ik
	      SDOT = SDOT + SX(I)*SY(I) + SX(I+1_ik)*SY(I+1_ik) &
                          + SX(I+2_ik)*SY(I+2_ik) + SX(I+3_ik)*SY(I+3_ik) &
                          + SX(I+4_ik)*SY(I+4_ik)
            ENDDO

          ELSE
!               ** NONEQUAL OR NONPOSITIVE INCREMENTS.
	    IX = iONE
	    IY = iONE
	    IF( INCX.LT. iZERO )  IX = iONE + (N-iONE)*(-INCX)
	    IF( INCY.LT. iZERO )  IY = iONE + (N-iONE)*(-INCY)
	    DO I = iONE, N
	      SDOT = SDOT + SX(IX) * SY(IY)
	      IX = IX + INCX
	      IY = IY + INCY
            ENDDO

          ENDIF
        ENDIF

   END FUNCTION SDOT

   SUBROUTINE SSCAL( N, SA, SX, INCX )

!         CALCULATE  X = A*X  (X = VECTOR, A = SCALAR)

!  --INPUT--  N  NUMBER OF ELEMENTS IN VECTOR
!            SA  SINGLE PRECISION SCALE FACTOR
!            SX  SING-PREC ARRAY, LENGTH 1+(N-1)*INCX, CONTAINING VECTOR
!          INCX  SPACING OF VECTOR ELEMENTS IN 'SX'
! --OUTPUT-- SX  REPLACE  SX(1+I*INCX)  WITH  SA * SX(1+I*INCX) 
!                FOR I = 0 TO N-1

        INTEGER(ik), intent(in) :: N, INCX
	REAL(dk), intent(in)    :: SA
	REAL(dk), intent(inout) :: SX(:)

        INTEGER(ik) :: I, M

        IF( N > iZERO ) THEN
          IF( INCX.NE. iONE ) THEN
	    DO I = iONE, iONE+(N-1)*INCX, INCX
	       SX(I) = SA * SX(I)
            ENDDO
          ELSE
	    M = MOD(N,5_ik)
	    IF( M.NE. iZERO ) THEN
!                           ** CLEAN-UP LOOP SO REMAINING VECTOR LENGTH
!                           ** IS A MULTIPLE OF 5.
	      DO I = iONE, M
	         SX(I) = SA * SX(I)
              ENDDO
	    ENDIF
!                             ** UNROLL LOOP FOR SPEED
	    DO I = M+iONE, N, 5_ik
	      SX(I)   = SA * SX(I)
	      SX(I+iONE) = SA * SX(I+iONE)
	      SX(I+2_ik) = SA * SX(I+2_ik)
	      SX(I+3_ik) = SA * SX(I+3_ik)
	      SX(I+4_ik) = SA * SX(I+4_ik)
            ENDDO
          ENDIF
        ENDIF

        END SUBROUTINE SSCAL

   SUBROUTINE SAXPY( N, SA, SX, INCX, SY, INCY )

!          Y = A*X + Y  (X, Y = VECTORS, A = SCALAR)
!  --INPUT--
!        N  NUMBER OF ELEMENTS IN INPUT VECTORS 'X' AND 'Y'
!       SA  SINGLE PRECISION SCALAR MULTIPLIER 'A'
!       SX  SING-PREC ARRAY CONTAINING VECTOR 'X'
!     INCX  SPACING OF ELEMENTS OF VECTOR 'X' IN 'SX'
!       SY  SING-PREC ARRAY CONTAINING VECTOR 'Y'
!     INCY  SPACING OF ELEMENTS OF VECTOR 'Y' IN 'SY'
! --OUTPUT--
!       SY   FOR I = 0 TO N-1, OVERWRITE  SY(LY+I*INCY) WITH 
!                 SA*SX(LX+I*INCX) + SY(LY+I*INCY), 
!            WHERE LX = 1          IF INCX .GE. 0,
!                     = (-INCX)*N  IF INCX .LT. 0
!            AND LY IS DEFINED IN A SIMILAR WAY USING INCY.

        INTEGER(ik), intent(in) :: N, INCX, INCY
	REAL(dk), intent(in)    :: SA
	REAL(dk), intent(in)    :: SX(:)
	REAL(dk), intent(inout) :: SY(:)

        INTEGER(ik) :: I, M, IX, IY

        IF( N > iZERO .and. SA /= rZERO ) THEN
          IF ( INCX.EQ.INCY .AND. INCX.GT. iONE )  THEN
	    DO I = iONE, iONE+(N-1)*INCX, INCX
	       SY(I) = SY(I) + SA * SX(I)
            ENDDO
          ELSE IF ( INCX.EQ.INCY .AND. INCX.EQ. iONE )  THEN
!                                        ** EQUAL, UNIT INCREMENTS
	    M = MOD(N,4_ik)
	    IF( M .NE. iZERO ) THEN
!                            ** CLEAN-UP LOOP SO REMAINING VECTOR LENGTH
!                            ** IS A MULTIPLE OF 4.
	      DO I = iONE, M
	        SY(I) = SY(I) + SA * SX(I)
              ENDDO
	    ENDIF
!                              ** UNROLL LOOP FOR SPEED
	    DO I = M+iONE, N, 4_ik
	      SY(I)   = SY(I)   + SA * SX(I)
	      SY(I+iONE) = SY(I+iONE) + SA * SX(I+iONE)
	      SY(I+2_ik) = SY(I+2_ik) + SA * SX(I+2_ik)
	      SY(I+3_ik) = SY(I+3_ik) + SA * SX(I+3_ik)
            ENDDO
          ELSE
!               ** NONEQUAL OR NONPOSITIVE INCREMENTS.
	    IX = iONE
	    IY = iONE
	    IF( INCX.LT. iZERO )  IX = iONE + (N-iONE)*(-INCX)
	    IF( INCY.LT. iZERO )  IY = iONE + (N-iONE)*(-INCY)
	    DO I = iONE, N
	      SY(IY) = SY(IY) + SA*SX(IX)
	      IX = IX + INCX
	      IY = IY + INCY
            ENDDO
          ENDIF
        ENDIF

        END SUBROUTINE SAXPY

end module tuvx_linear_algebra
