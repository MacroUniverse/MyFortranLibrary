rm *.o *.x *.mod
gfortran -fopenmp test.f90
#gfortran test.f90
mv a.out test.x
