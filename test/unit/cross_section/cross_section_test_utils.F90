module tuvx_test_cross_section_utils

  implicit none
  public

contains

  subroutine check_values( results, expected_results, relative_tolerance )

    use musica_assert,                 only : assert, almost_equal
    use musica_constants,              only : dk => musica_dk
    use tuvx_util,                     only : inter2

    real(kind=dk), intent(in) :: results(:,:)
    real(kind=dk), intent(in) :: expected_results(:,:)
    real(kind=dk), intent(in) :: relative_tolerance

    integer :: i_level, i_wavelength

    call assert( 577098581, &
      size( results, dim = 1 ) == size( expected_results, dim = 1) )
    call assert( 696108875, &
      size( results, dim = 2 ) == size( expected_results, dim = 2) )
    do i_wavelength = 1, size( results, dim = 2 )
      do i_level = 2, size( results, dim = 1 )
        call assert( 179372912, almost_equal(                                 &
          results( i_level, i_wavelength ),                                   &
          expected_results( i_level, i_wavelength ),                          &
          relative_tolerance))
      end do
    end do

  end subroutine check_values

end module tuvx_test_cross_section_utils