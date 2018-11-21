module mymodule
implicit none 

   real(16) :: pi = 3.1415926535897932384626433832795029q0 ,&
                         &e  = 2.7182818284590452353602874713526625q0 ,&
                         &c  = 299792458 ,&

    
function linspace(x1,x2,N)
    integer :: N, i
    real(8) :: x1, x2, linspace(N)
    real(8) :: dx
    dx = (x2-x1)/(N-1)
    linspace = (/ ( (dx*i), i=0,N ) /)
    print '(5ES22.5)', linspace
    end

