c Using the leapfrog method to solve the TDSE

c Rpsi(ix,it): Real part of Psi, t(it)=dt*(it-1)
c Ipsi(ix,it): Imaginary part Psi, t(it)=dt*(it-1/2)


      program leapfrog
      implicit none
      
c      use mymodule
      integer*4 Nx,Nt,ii,jj
      real*8 pi,x1,x2,dx,t_end,dt,dt2dx2 !dt/(2*dx^2)
      real*8, allocatable ::  x(:),Rpsi(:,:),Ipsi(:,:)
      parameter(pi=3.14159265358979323d0)
      print*,'pi=',pi
      x1=0; x2=pi; Nx=2001; dx=(x2-x1)/(Nx-1)
      t_end=0.002; Nt=4001; dt=t_end/(Nt-1)
      dt2dx2=dt/(2*dx**2)
      allocate(x(Nx),Rpsi(Nx,Nt),Ipsi(Nx,Nt))
      
c  Set up Initial vale
      call linspace(x1,x2,Nx, x)
      Rpsi(:,1)=dsin(x)

c-----------------The Main Iteration----------------

      do jj=1,Nt-1; do ii=2,Nx-1 ! psi(ii,jj), ii=>position, jj=>time
          Rpsi(ii,jj+1)=Rpsi(ii,jj) -dt2dx2*(Ipsi(ii+1,jj)+Ipsi(ii-1,jj)
     &                  -2*Ipsi(ii,jj)) ! -dt*V(ii)*Rpsi(ii,jj) , which is 0

          Ipsi(ii,jj+1)=Ipsi(ii,jj) +dt2dx2*(Rpsi(ii+1,jj+1)
     &                  +Rpsi(ii-1,jj+1) -2*Rpsi(ii,jj+1)) ! +dt*V(ii)*Ipsi(ii,jj+1)
      enddo; enddo
c-----------------Write File
      open(101,file='Rpsi.txt',status='new')
      open(102,file='Ipsi.txt',status='new')
      open(103,file='NxNtx1x2.txt',status='new')
      write(103,*) Nx,Nt,x1,x2
      do jj=1,Nt; do ii=1,Nx
          write(101,*) Rpsi(ii,jj)
          write(102,*) Ipsi(ii,jj)
      enddo; enddo
      close(101);close(102)


      deallocate(x,Rpsi,Ipsi)
      print*, 'this is the end of the program'
      end program leapfrog


c Linspace----------------------------------
      subroutine linspace(x1,x2,Nx, x)
      integer*4 ii,Nx
      real*8 x1,x2,dx,x(Nx)
      dx=(x2-x1)/(Nx-1)
      do ii=1,Nx
          x(ii)=ii-1
      end do
      x=x*dx+x1
      end subroutine linspace