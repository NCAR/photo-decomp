! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
module tuvx_profile_extraterrestrial_flux

  use musica_constants,  only : dk => musica_dk, ik => musica_ik, lk => musica_lk
  use tuvx_Profile,      only : profile_t

  implicit none

  public :: etflfromCsvFile_t

  type, extends(profile_t) :: etflfromCsvFile_t
  contains
    final     :: finalize
  end type etflfromCsvFile_t

  !> Constructor
  interface etflfromCsvFile_t
    module procedure constructor
  end interface etflfromCsvFile_t

contains
  !> Initialize grid
  function constructor( profile_config, gridWareHouse ) result ( this )
      
    use musica_config, only : config_t
    use musica_string, only : string_t
    use musica_assert, only : die_msg
    use tuvx_util,   only : addpnt
    use tuvx_constants,    only : hc, deltax
    use tuvx_grid,  only : grid_t
    use tuvx_grid_warehouse,  only : grid_warehouse_t
    use tuvx_diagnostic_util,         only : diagout
    use tuvx_interpolate

    !> arguments
    type(etflfromCsvFile_t), pointer :: this
    type(config_t),           intent(inout) :: profile_config
    type(grid_warehouse_t),   intent(inout) :: gridWareHouse

    !> Local variables
    character(len=*), parameter :: Iam = 'etflfromCsvFile Profile initialize: '

    integer(ik), parameter :: Ok = 0_ik
    integer(ik), parameter :: inUnit = 20_ik
    integer(ik), parameter :: iZERO = 0_ik
    integer(dk), parameter :: rONE  = 1.0_dk
    real(dk),    parameter :: bin_edge(0:4) = (/ rZERO,150.01_dk,200.07_dk,1000.99_dk,real(huge(rZERO),dk) /)
    character(len=*), parameter :: comment = '#!$%*'
    class(grid_t), pointer :: lambdaGrid
 
    integer(ik) :: istat
    integer(ik) :: fileNdx, nFiles, ndx, nBins, nLines, Line
    real(dk)    :: zd, Value
    real(dk), allocatable :: inputGrid(:), inputData(:), tmpinputGrid(:)
    real(dk), allocatable :: interpolatedEtfl(:)
    logical(lk) :: found
    character(len=132)                 :: InputLine, trimInputLine
    character(len=512)                 :: IoMsg
    type(string_t)                     :: Handle
    type(string_t)                     :: defaultInterpolator
    type(string_t), allocatable        :: Filespec(:), Interpolator(:)
    class(abs_interpolator_t), pointer :: theInterpolator

    write(*,*) Iam // 'entering'

    allocate( this )

    defaultInterpolator = 'interp2'

    !> Get the configuration settings
    call profile_config%get( 'Filespec', Filespec, Iam )
    call profile_config%get( 'Handle', this%handle_, Iam, default = 'None' )
    call profile_config%get( 'Interpolator', Interpolator, Iam, found=found )
    nFiles = size(Filespec)
    if( .not. found ) then
      allocate( Interpolator(nFiles) )
      Interpolator = defaultInterpolator
    endif

    write(*,*) Iam // 'there are ',nFiles,' input files'
    write(*,*) Iam // 'profile handle = ',this%handle_%to_char()

    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    nBins = lambdaGrid%ncells_
    write(*,*) Iam // 'there are ',nBins,' wavelength bins'

file_loop: &
    do fileNdx = iONE,nFiles
      if(Interpolator(fileNdx) == "") Interpolator(fileNdx) = defaultInterpolator
      !> Does input grid file exist?
      inquire( file=Filespec(fileNdx)%to_char(), exist=found )
      file_exists: if( found ) then
        open(unit=inUnit,file=Filespec(fileNdx)%to_char(),iostat=istat,iomsg=IoMsg)
        if( istat == Ok ) then
          write(*,*) Iam // 'opening, reading file ',Filespec(fileNdx)%to_char()
          !> Determine number of lines in file
          nLines = iZERO
          do
            read(inUnit,'(a)',iostat=istat) InputLine
            if( istat == Ok ) then
              trimInputLine = adjustl(InputLine)
              if( verify( trimInputLine(1:1),comment ) /= 0 ) then
                nLines = nLines + iONE
                cycle
              endif
            else
              rewind(unit=inUnit)
              exit
            endif
          enddo
          !> Skip the header
          do
            read(inUnit,'(a)',iostat=istat) InputLine
            if( istat /= Ok ) then
              exit
            else
              trimInputLine = adjustl(InputLine)
              if( verify( trimInputLine(1:1),comment ) /= 0 ) then
                exit
              endif
            endif
          enddo
          if( istat == Ok ) then
            write(*,*) Iam // 'skipped header for file ',Filespec(fileNdx)%to_char()
            allocate( inputGrid(nLines) )
            allocate( inputData(nLines) )
            !> Read the data
            Line = iONE
            do
              trimInputLine = adjustl(InputLine)
              if( verify( trimInputLine(1:1),comment ) /= 0 ) then
                read(InputLine,*,iostat=istat,iomsg=IoMsg) inputGrid(Line),inputData(Line)
                if( istat /= Ok ) then
                  write(*,*) Iam // trim(IoMsg)
                  call die_msg( 560768229, "Invalid data format in " // Filespec(fileNdx)%to_char() )
                endif
                Line = Line + iONE
              endif
              read(inUnit,'(a)',iostat=istat) InputLine
              if( istat /= Ok ) then
                exit
              endif
            enddo
          else
            call die_msg( 560768227, "Error reading " // Filespec(fileNdx)%to_char() )
          endif
        else
          write(*,*) Iam // trim(IoMsg)
          call die_msg( 560768231, "Error opening " // Filespec(fileNdx)%to_char() )
        endif
      else file_exists
        call die_msg( 560768215, "File " // Filespec(fileNdx)%to_char() // " not found" )
      endif file_exists

      close(unit=inUnit)

      write(*,*) Iam // 'read data for file ',Filespec(fileNdx)%to_char()
      write(*,*) Iam // 'interpolator for file ',Filespec(fileNdx)%to_char(),' = ',Interpolator(fileNdx)%to_char()

    !> special handling for neckel.flx
      if( index(Filespec(fileNdx)%to_char(),'neckel.flx') /= 0 ) then
        allocate( tmpinputGrid,source=inputGrid )
        where( inputGrid < 630._dk )
          tmpinputGrid = inputGrid - 0.5_dk
        elsewhere( inputGrid >= 630._dk .and. inputGrid < 870._dk )
          tmpinputGrid = inputGrid - rONE
        elsewhere( inputGrid >= 870._dk)
          tmpinputGrid = inputGrid - 2.5_dk
        endwhere
        inputData = 1.e13_dk*hc*inputData/inputGrid
        inputGrid = tmpinputGrid
        inputGrid = [inputGrid,inputGrid(size(inputGrid)) + 2.5_dk]
        inputData = [inputData,rZERO]
        deallocate( tmpinputGrid )
      else
    !> extend inputGrid,inputData to cover model photolysis grid
        call addpnt( x=inputGrid,y=inputData,xnew=(rONE-deltax)*inputGrid(1),ynew=rZERO )
        call addpnt( x=inputGrid,y=inputData,xnew=rZERO,ynew=rZERO )
        call addpnt( x=inputGrid,y=inputData,xnew=(rONE+deltax)*inputGrid(size(inputGrid)),ynew=rZERO )
        call addpnt( x=inputGrid,y=inputData,xnew=1.e38_dk,ynew=rZERO )
      endif
    !> assign interpolator for this dataset
      select case( Interpolator(fileNdx)%to_char() )
        case( 'interp1' )
          allocate( interp1_t :: theInterpolator )
        case( 'interp2' )
          allocate( interp2_t :: theInterpolator )
        case( 'interp3' )
          allocate( interp3_t :: theInterpolator )
        case( 'interp4' )
          allocate( interp4_t :: theInterpolator )
        case default
          call die_msg( 560768275, "interpolator " // Interpolator(fileNdx)%to_char() // " not a valid selection" )
      end select

    !> interpolate from source to model wavelength grid
      interpolatedEtfl = theInterpolator%interpolate( lambdaGrid%edge_, inputGrid, inputData, FoldIn=0 )
      if( .not. allocated( this%mid_val_ ) ) then
        allocate( this%mid_val_,mold=interpolatedEtfl )
        this%mid_val_ = rZERO
      endif

    !> assign interpolated source to model etfl
      where( bin_edge(fileNdx-1) <= lambdaGrid%edge_(:nBins) .and. lambdaGrid%edge_(:nBins) < bin_edge(fileNdx) )
         this%mid_val_ = interpolatedEtfl
      endwhere

    !> test diagnostics
      if( index(Filespec(fileNdx)%to_char(),'susim') /= 0 ) then
        call diagout( 'susim.inputGrid.new', inputGrid )
        call diagout( 'susim.inputData.new', inputData )
        call diagout( 'susim.interpolated.new', interpolatedEtfl )
        call diagout( 'susim.etfl.new', this%mid_val_ )
      elseif( index(Filespec(fileNdx)%to_char(),'atlas') /= 0 ) then
        call diagout( 'atlas.inputGrid.new', inputGrid )
        call diagout( 'atlas.inputData.new', inputData )
        call diagout( 'atlas.interpolated.new', interpolatedEtfl )
        call diagout( 'atlas.etfl.new', this%mid_val_ )
      elseif( index(Filespec(fileNdx)%to_char(),'neckel') /= 0 ) then
        call diagout( 'neckel.inputGrid.new', inputGrid )
        call diagout( 'neckel.inputData.new', inputData )
        call diagout( 'neckel.interpolated.new', interpolatedEtfl )
        call diagout( 'neckel.etfl.new', this%mid_val_ )
      elseif( index(Filespec(fileNdx)%to_char(),'sao2010') /= 0 ) then
        call diagout( 'sao2010.inputGrid.new', inputGrid )
        call diagout( 'sao2010.inputData.new', inputData )
        call diagout( 'sao2010.interpolated.new', interpolatedEtfl )
        call diagout( 'sao2010.etfl.new', this%mid_val_ )
      endif

      deallocate( inputGrid,inputData )
      deallocate( theInterpolator )

    enddo file_loop

    deallocate( lambdaGrid )

    !> test diagnostics
    call diagout( 'etfl.new', this%mid_val_ )

    write(*,*) Iam // 'exiting'

  end function constructor

  subroutine finalize( this )

  type(etflfromCsvFile_t), intent(inout) :: this

  if( allocated( this%edge_val_ ) ) deallocate( this%edge_val_ )
  if( allocated( this%mid_val_ ) )  deallocate( this%mid_val_ )
  if( allocated( this%delta_val_ ) ) deallocate( this%delta_val_ )
  if( allocated( this%layer_dens_ ) ) deallocate( this%layer_dens_ )

  end subroutine finalize

end module tuvx_profile_extraterrestrial_flux
