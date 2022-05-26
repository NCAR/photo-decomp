! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
module tuvx_constants

      use musica_constants, only : ik => musica_ik, dk => musica_dk

      IMPLICIT NONE

!* delta for adding points at beginning or end of data grids
      REAL(dk), PARAMETER :: deltax = 1.e-5_dk

!* some constants...

!* pi:
      REAL(dk), PARAMETER :: pi = 3.1415926535898_dk

!* radius of the earth, km:
      REAL(dk), PARAMETER :: radius = 6.371E+3_dk

!* Planck constant x speed of light, J m

      REAL(dk), PARAMETER :: hc = 6.626068e-34_dk * 2.99792458e8_dk

!* largest number of the machine:
      REAL(dk), PARAMETER :: largest = 1.E+36_dk

!* small numbers (positive and negative)
      REAL(dk), PARAMETER :: pzero =  10._dk/largest
      REAL(dk), PARAMETER :: nzero = -10._dk/largest

!* machine precision
      REAL(dk), PARAMETER :: precis = 1.e-7_dk

!* More physical constants:
!*_________________________________________________________________
!* Na = 6.022142E23  mol-1       = Avogadro constant
!* kb = 1.38065E-23  J K-1       = Boltzmann constant 
!* R  = 8.31447      J mol-1 K-1 = molar gas constant
!* h  = 6.626068E-34 J s         = Planck constant 
!* c  = 2.99792458E8 m s-1       = speed of light in vacuum 
!* G  = 6.673E-11    m3 kg-1 s-2 = Netwonian constant of gravitation
!* sb = 5.67040E-8   W m-2 K-4   = Stefan-Boltzmann constant
!*_________________________________________________________________
!* (1) From NIST Reference on Constants, Units, and Uncertainty
!* http://physics.nist.gov/cuu/index.html Oct. 2001.
!* (2) These constants are not assigned to variable names;  in other 
!* words this is not Fortran code, but only a text table for quick 
!* reference.  To use, you must declare a variable name/type and
!* assign the value to that variable. Or assign as parameter (see
!* example for pi above).

end module tuvx_constants
