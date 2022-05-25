! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
module tuvx_diagnostic_util

   use musica_constants, only : dk => musica_dk

   implicit none

   interface diagout
     module procedure :: diagnostic_1d
     module procedure :: diagnostic_1d_dk
     module procedure :: diagnostic_2d
     module procedure :: diagnostic_2d_dk
   end interface diagout

   contains

   subroutine diagnostic_1d( filename, variable )
   
   character(len=*), intent(in) :: filename
   real, intent(in)             :: variable(:)

   integer :: ios

   write(*,*) 'diagnostic_1d: entering'

   open(unit=44,file='OUTPUTS/'//filename,form='unformatted',iostat=ios)
   if( ios /= 0 ) then
     write(*,*) 'diagnostic_1d: failed to open ',filename,'; error = ',ios
     stop 'OpnErr'
   endif
   write(unit=44,iostat=ios) variable
   if( ios /= 0 ) then
     write(*,*) 'diagnostic_1d: failed to write ',filename,'; error = ',ios
     stop 'OpnErr'
   endif

   close(unit=44)

   write(*,*) 'diagnostic_1d: exiting'

   end subroutine diagnostic_1d

   subroutine diagnostic_1d_dk( filename, variable )
   
   character(len=*), intent(in) :: filename
   real(dk), intent(in)         :: variable(:)

   character(len=*), parameter  :: Iam = 'diagnostic_1d_dk: '

   integer :: ios
   character(len=256) :: iomsg

   write(*,*) Iam // 'entering'

   open(unit=44,file='OUTPUTS/'//filename,form='unformatted',iostat=ios,iomsg=iomsg)
   if( ios /= 0 ) then
     write(*,*) 'diagnostic_1d: failed to open ',filename,'; error = ',ios
     write(*,*) trim(iomsg)
     stop 'OpnErr'
   endif
   write(unit=44,iostat=ios,iomsg=iomsg) variable
   if( ios /= 0 ) then
     write(*,*) 'diagnostic_1d: failed to write ',filename,'; error = ',ios
     write(*,*) trim(iomsg)
     stop 'WriteErr'
   endif

   close(unit=44)

   write(*,*) Iam // 'exiting'

   end subroutine diagnostic_1d_dk

   subroutine diagnostic_2d( filename, variable )
   
   character(len=*), intent(in) :: filename
   real, intent(in)             :: variable(:,:)

   integer :: ios

   open(unit=44,file='OUTPUTS/'//filename,form='unformatted',iostat=ios)
   if( ios /= 0 ) then
     write(*,*) 'diagnostic_2d: failed to open ',filename,'; error = ',ios
     stop 'OpnErr'
   endif
   write(unit=44,iostat=ios) variable
   if( ios /= 0 ) then
     write(*,*) 'diagnostic_2d: failed to write ',filename,'; error = ',ios
     stop 'OpnErr'
   endif

   close(unit=44)

   end subroutine diagnostic_2d

   subroutine diagnostic_2d_dk( filename, variable )
   
   character(len=*), intent(in) :: filename
   real(dk), intent(in)         :: variable(:,:)

   integer :: ios
   character(len=512) :: iomsg

   open(unit=44,file='OUTPUTS/'//filename,form='unformatted',iostat=ios,iomsg=iomsg)
   if( ios /= 0 ) then
     write(*,*) 'diagnostic_2d_dk: failed to open ',filename,'; error = ',ios
     write(*,*) trim(iomsg)
     call execute_command_line('pwd')
     stop 'OpnErr'
   endif
   write(unit=44,iostat=ios,iomsg=iomsg) variable
   if( ios /= 0 ) then
     write(*,*) 'diagnostic_2d_dk: failed to write ',filename,'; error = ',ios
     write(*,*) trim(iomsg)
     stop 'WriteErr'
   endif

   close(unit=44)

   end subroutine diagnostic_2d_dk

end module tuvx_diagnostic_util
