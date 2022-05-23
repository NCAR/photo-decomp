! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
module micm_photolysis_wavelength_grid

  use musica_constants, only : musica_dk, musica_ik

  implicit none

  type photolysis_wavelength_grid_t
    !> number of wavelength grid cells
    integer(musica_ik) :: nwave
    !> extra terrestial flux (photons/cm^2)
    real(musica_dk), allocatable :: etf(:)
    !> wavelength grid cell centers (nm)
    real(musica_dk), allocatable :: wcenter(:)
    !> wavelength grid cell edges (nm)
    real(musica_dk), allocatable :: wedge(:)
  end type photolysis_wavelength_grid_t

  type(photolysis_wavelength_grid_t) :: wavelength_grid

contains

  function wavelength_grid_initialize(filespec) result(retcode)

    use nc4fortran, only : netcdf_file 

    integer(musica_ik)              :: retcode
    character(len=*), intent(in)    :: filespec

    integer(musica_ik), parameter :: noErr = 0_musica_ik
    character(len=*), parameter   :: Iam = 'wavelength_grid_initialize: '
    integer(musica_ik), allocatable :: dims(:)
    type(netcdf_file)  :: ncObj

    call ncObj%initialize(filespec, ierr=retcode, status='old', action='r')
    if( retcode /= noErr ) then
      write(*,*) Iam,'retcode from initialize = ',retcode
      stop 'FileOpenError'
    endif

    call ncObj%shape( 'etf', dims )
    wavelength_grid%nwave = dims(1)
    associate( nwave => dims(1) )
    allocate(wavelength_grid%etf(nwave),wavelength_grid%wcenter(nwave),wavelength_grid%wedge(nwave+1))
    call ncObj%read( 'etf', wavelength_grid%etf )
    call ncObj%read( 'wc', wavelength_grid%wcenter )
    call ncObj%read( 'wl', wavelength_grid%wedge )
    end associate

    call ncObj%finalize()
    
  end function wavelength_grid_initialize

end module micm_photolysis_wavelength_grid
