! Play with function and subroutine
    
program test
implicit none
    integer :: M(-1:1,2:3) = 0, i, a(8) = 0, b(8) = (/0,1,2,3,4,5,6,7/)
    real :: c(4) = (/1,2,3,4/)
    logical :: logic(4) = (/1,1,0,1/)
    
    interface
        subroutine printMat(Mat)
            integer :: Mat(:,:)
        end subroutine printMat
        function testfun(a)
            real a
        end function testfun
    end interface
    
    M = reshape( (/5,9,6,1000,8,12/), (/3,2/) )
    print*, 'lbound1', lbound(M)
    print*, 'ubound1', ubound(M)
    print*, 'pack', pack(M,.true.)

    a = pack(M,.true.,b)   ! test
    print*, 'test pack function'
    print '(10I12)', a

    print*, 'test end'
    call printMat(M)
    print*, 'logic = ', logic
    print*, 'c(1,2,4) = ', pack(c, logic)
    print*, 'testfun test'
    print*, testfun(5.)
end program test

subroutine printMat(Mat)
    integer :: Mat(:,:)
    integer :: ii, shapeM(2)
    shapeM = shape(Mat)
    do ii = 1,shapeM(1)
        print*, Mat(ii,:)
    end do
    end subroutine printMat
    
function testfun(a) result(t2)
    real t2
    t2 = a*2
    end function testfun
    


    
    
    
    

