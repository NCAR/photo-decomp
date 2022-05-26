! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the photo_grid module

!> Test module for the grid_t type
program test_grid

  implicit none

  call test_grid_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Tests of the grid_t type
  subroutine test_grid_t( )

    use musica_config,    only : config_t
    use musica_string,    only : string_t
    use musica_assert,    only : assert, almost_equal
    use musica_constants, only : ik => musica_ik, dk => musica_dk
    use micm_grid_warehouse, only : grid_warehouse_t
    use micm_1d_grid,    only : abs_1d_grid_t

    !> local variables
    character(len=*), parameter :: Iam = 'test_grid: '
    character(len=*), parameter :: config_flsp = 'test/data/grid.tst.config.json'
    type(config_t) :: grid_tst_config
    type(grid_warehouse_t), pointer :: thewarehouse
    type(string_t) :: Handle
    class(abs_1d_grid_t), pointer   :: aGrid
    integer :: csv_grid_cells = 156
    real(dk) :: csv_midpoints(156)
    real(dk) :: csv_deltas(156)
    integer :: i

    csv_midpoints=[120.7000,121.6500,122.1000,122.7000,123.4500,124.2000,125.0000,125.8000,126.6000,127.8000,129.0000,129.8500,&
                   131.1500,133.5000,136.0000,141.0000,150.0000,160.0000,167.5000,172.7000,176.2000,177.8000,179.4000,181.0000,&
                   182.6500,184.3500,186.0500,187.8000,189.6000,191.4000,193.2500,195.1500,197.0500,199.0000,201.0000,203.0500,&
                   205.1500,207.2665,209.4295,211.6460,213.9100,216.2225,218.5855,221.0010,223.4705,225.9960,228.5790,231.2215,&
                   233.9260,236.6945,239.5295,242.4330,245.4080,248.4570,251.5825,254.7875,258.0750,261.4490,264.9125,268.4685,&
                   272.1215,275.8755,279.7340,283.7020,287.7845,291.9865,296.3090,300.5000,303.0000,304.0000,305.0000,306.0000,&
                   307.0000,308.0000,309.0000,310.0000,311.0000,312.0000,313.0000,314.0000,316.0000,320.0000,325.0000,330.0000,&
                   335.0000,340.0000,345.0000,350.0000,355.0000,360.0000,365.0000,370.0000,375.0000,380.0000,385.0000,390.0000,&
                   395.0000,400.0000,405.0000,410.0000,415.0000,420.0000,425.0000,430.0000,435.0000,440.0000,445.0000,450.0000,&
                   455.0000,460.0000,465.0000,470.0000,475.0000,480.0000,485.0000,490.0000,495.0000,500.0000,505.0000,510.0000,&
                   515.0000,520.0000,525.0000,530.0000,535.0000,540.0000,545.0000,550.0000,555.0000,560.0000,565.0000,570.0000,&
                   575.0000,580.0000,585.0000,590.0000,595.0000,600.0000,605.0000,610.0000,615.0000,620.0000,625.0000,630.0000,&
                   635.0000,640.0000,644.8000,651.0500,660.0000,670.0000,680.0000,690.0000,700.0000,710.0000,720.0000,730.0000 ]
    
    csv_deltas = [1.400000,0.500000,0.400000,0.800000,0.700000,0.800000,0.800000,0.800000,0.800000,1.600000,0.800000,0.900000,&
                  1.700000,3.000000,2.000000,8.000000,10.00000,10.00000,5.000000,5.400000,1.600000,1.600000,1.600000,1.600000,&
                  1.700000,1.700000,1.700000,1.800000,1.800000,1.800000,1.900000,1.900000,1.900000,2.000000,2.000000,2.100000,&
                  2.100000,2.133000,2.193000,2.240000,2.288000,2.337000,2.389000,2.442000,2.497000,2.554000,2.612000,2.673000,&
                  2.736000,2.801000,2.869000,2.938000,3.012000,3.086000,3.165000,3.245000,3.330000,3.418000,3.509000,3.603000,&
                  3.703000,3.805000,3.912000,4.024000,4.141000,4.263000,4.382000,4.000000,1.000000,1.000000,1.000000,1.000000,&
                  1.000000,1.000000,1.000000,1.000000,1.000000,1.000000,1.000000,1.000000,3.000000,5.000000,5.000000,5.000000,&
                  5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,&
                  5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,&
                  5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,&
                  5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,&
                  5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,5.000000,&
                  5.000000,5.000000,4.600000,7.900000,10.00000,10.00000,10.00000,10.00000,10.00000,10.00000,10.00000,10.00000 ]

    write(*,*) Iam // 'entering'

    call grid_tst_config%from_file( config_flsp )
    thewarehouse => grid_warehouse_t( grid_tst_config )

    Handle = 'Vertical Z'
    aGrid => thewarehouse%get_grid( Handle )
    call assert( 412238768, aGrid%ncells_ .eq. 120_ik )
    call assert( 412238769, all( aGrid%delta_ .eq. 1._dk ) )

    write(*,*) ' '
    write(*,*) Iam // 'Grid = ',aGrid%handle_
    write(*,*) 'There are ',aGrid%ncells_,' grid cells'
    write(*,*) 'Grid edges'
    write(*,'(1p10g15.7)') aGrid%edge_
    write(*,*) 'Grid midpoints'
    write(*,'(1p10g15.7)') aGrid%mid_
    write(*,*) 'Grid deltas'
    write(*,'(1p10g15.7)') aGrid%delta_

    deallocate( aGrid )

    Handle = 'Photolysis, wavelength'
    aGrid => thewarehouse%get_grid( Handle )
    call assert( 412238769, all( aGrid%delta_ > 0._dk ) )

    write(*,*) ' '
    write(*,*) Iam // 'Grid = ',aGrid%handle_
    call assert( 412238770, aGrid%ncells_ .eq. csv_grid_cells )

    write(*,*) 'Grid edges'
    call assert( 412238770, all( aGrid%edge_ .ne. 0 ) )
    call assert( 412238771, all( aGrid%edge_ .gt. 0 ) )

    write(*,*) 'Grid midpoints'
    call assert( 412238772, size( aGrid%mid_ ) .eq. csv_grid_cells )
    do i = 1, size(csv_midpoints)
      call assert( 412238773, &
        almost_equal( csv_midpoints( i ), aGrid%mid_(i), 0.1d-5 ) )
    end do

    write(*,*) 'Grid deltas'
    call assert( 412238774, size( aGrid%delta_ ) .eq. csv_grid_cells )

    do i = 1, size(csv_deltas)
      call assert( 412238775, &
        almost_equal( csv_deltas( i ), aGrid%delta_(i), 0.1d-5 ) )
    end do

    deallocate( aGrid )

    Handle = 'Time, hrs'
    aGrid => thewarehouse%get_grid( Handle )
    call assert( 412238769, all( aGrid%delta_ > 0._dk ) )

    write(*,*) ' '
    write(*,*) Iam // 'Grid = ',aGrid%handle_
    write(*,*) 'There are ',aGrid%ncells_,' grid cells'
    write(*,*) 'Grid edges'
    write(*,'(1p10g15.7)') aGrid%edge_
    write(*,*) 'Grid midpoints'
    write(*,'(1p10g15.7)') aGrid%mid_
    write(*,*) 'Grid deltas'
    write(*,'(1p10g15.7)') aGrid%delta_

    deallocate( aGrid )

    write(*,*) Iam // 'leaving'

  end subroutine test_grid_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_grid
