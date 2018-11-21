module mymodule
    real(8) :: pi = 3.141592653589793d0,   &
               e  = 2.718281828459045d0
               
    contains
        ! This is the linspace function in Matlab
        function linspace(x1,x2,Nx)
            real(8) :: x1, x2, linspace(Nx)
            linspace(1:Nx) = (/(ii,ii=0,Nx-1)/)*(x2-x1)/(Nx-1) + x1
        end function linspace
end module mymodule
    