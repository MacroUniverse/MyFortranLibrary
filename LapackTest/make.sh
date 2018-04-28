rm *.o *.x *.mod
gfortran -c lapack.f90
gfortran *.o  -l lapack -l blas
mv a.out lapack.x
./lapack.x
