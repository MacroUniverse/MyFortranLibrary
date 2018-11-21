! Using the Leapfrog Method to solve the one dimensional TDSE
    
program functiontest
use mymodule
integer :: ii,Nx,Nt
real(8) :: x1, x2, dt, t_end
real(8),allocatable :: x(:), t(:), temp1(:), temp2(:)
complex(8), allocatable :: psi(:)

x1 = 0d0; x2 = pi         ! Wave Function Boundaries
Nx = 500                    ! Wave Function Grid Point #
t_end = 0.2d0             ! Ending Time
Nt = 10;                   ! Time Grid #
dt = t_end/(Nt-1);        ! Time Step Length

allocate(t(Nt), x(Nx), temp1(Nx), temp2(Nx), psi(Nx))

! Grid Points
x = linspace(x1,x2,Nx); t = linspace(0d0,t_end,Nt);
!print*, 'x is'; print*, x;
!print*, 't is'; print*, t;

! Initial Value Setup
call vdSin(Nx,x,temp1)
do ii = 1,Nx; psi(ii) = cmplx(temp1(ii),0,8); end do

!!!!!!!!!!!!!!!!!!!!! Solving TDSE in Naive Way !!!!!!!!!!!!!!!!!!
open (11, file = 'settings.txt',status = 'new')
open (12, file = 'RealWF.txt', status = 'new')
open (13, file = 'ImagWF.txt', status = 'new')
write(11, '(1ES24.15)') x1,x2,real(Nx,8),t_end,real(Nt,8)
dx2 = ((x2-x1)/(Nx-1))**2

! Write Initial Wave Function
write(12,'(500ES24.15)') real(psi); write(13,'(500ES24.15)') imag(psi)

! Time Evolution
do ii = 2,Nt
    psi(2:Nx-1) = psi(2:Nx-1) + &
        ((psi(3:Nx) + psi(1:Nx-2) - 2*psi(2:Nx-1))/dx2 *(0,0.5d0))*dt ! V = 0
    write(12,'(500ES24.15)') real(psi); write(13,'(500ES24.15)') Imag(psi)
end do
close (1)

end program functiontest

subroutine f1
    print*, 'f1 is called'
end subroutine f1

