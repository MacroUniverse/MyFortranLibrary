!Program to test openMP with gfortran
program openMP
  integer, parameter :: n = 8E4
  integer:: i,j,k
  real(kind=8), dimension(n)::  A, B
  do i=1,n
    A(i) = i
  enddo
!$OMP PARALLEL DO
  do k=1,n
  do j=1,n
    do i=2,n
      B(i) = B(i) + (A(i) + A(i-1)) / 2.0
    enddo
  enddo
  enddo
!$OMP END PARALLEL DO
end program openMP
