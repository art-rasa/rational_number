! 
! Simple test program for the rational.number module.
! 
program rational_number_test
    use rational_number
    implicit none
    
    !type(rationalnumber_t) :: rNum1, rNum2, rNum3
    !integer :: tmp
    integer :: selection
    
    print *, '-----program rational_number_test-----'
    print *, 'Select feature to test:'
    print *, '  1: testRatNumSimplify()'
    print *, '  2: testRatNumInitFromReal()'
    print *, '  3: testRatNumMultiply()'
    print *, '  4: ()'
    print *, '  5: ()'
    print *, '  6: ()'
    print *, '  0: test all'
    read(*, '(i3)') selection
    
    select case (selection)
    case (1)
        call testRatNumSimplify()
    
    case (2)
        call testRatNumInitFromReal()
        
    case (3)
        call testRatNumMultiply()
        
    case (0)
        call testRatNumSimplify()
        call testRatNumInitFromReal()
        call testRatNumMultiply()
    end select
    
contains
    
    subroutine testRatNumSimplify()
        type(rationalnumber_t) :: rNum1, rNum2
        
        rNum1 = ratNumInit(1, 1)
        rNum2 = ratNumInit(1, 1)
        
        print *, ':::: testRatNumSimplify() ::::'
        print *, ' '
        
        call getRatNum(rNum1)
        call getRatNum(rNum2)
        
        print *, ' '
        print *, 'Original rational numbers:'
        print '(5x,a)', 'r1 = ' // ratNumToStr(rNum1) // &
                       ', r2 = ' // ratNumToStr(rNum2)
        print *, 'Simplified rational numbers:'
        rNum1 = ratNumSimplify(rNum1)
        rNum2 = ratNumSimplify(rNum2)
        print '(5x,a)', 'r1 = ' // ratNumToStr(rNum1) // &
                       ', r2 = ' // ratNumToStr(rNum2)
        
        print *, ':::: End of test ::::'
    end subroutine
    
    subroutine testRatNumInitFromReal()
        print *, ':::: testRatNumInitFromReal() ::::'
        print *, ' '
        
        call testRatNumInitFromReal_driver(12.34)
        call testRatNumInitFromReal_driver(-15357.63)
        call testRatNumInitFromReal_driver(-160000.02)
        call testRatNumInitFromReal_driver(445.69937)
        call testRatNumInitFromReal_driver(545871.616)
        call testRatNumInitFromReal_driver(313.96153)
        call testRatNumInitFromReal_driver(-86.622)
        call testRatNumInitFromReal_driver(8.53645)
        
        print *, ':::: End of test ::::'
    end subroutine
    
    subroutine testRatNumMultiply()
        print *, ':::: testRatNumMultiply() ::::'
        print *, ' '

        call testRatNumMultiply_driver_int(2, 6, 3, 7)
        call testRatNumMultiply_driver_int(2142, 653, -2733, 7361)
        call testRatNumMultiply_driver_int(-2, 1, -1, 1)
        call testRatNumMultiply_driver_int(-54, 1, -1, 632)
        call testRatNumMultiply_driver_int(-632, 1, -1, 632)
        call testRatNumMultiply_driver_real(-1.05, -2.6)
        
        print *, ':::: End of test ::::'
    end subroutine
    
    ! 
    ! Test driver procedures
    ! 
    
    subroutine testRatNumInitFromReal_driver(r)
        real, intent(in) :: r
        type(rationalnumber_t) :: rNum1, rNum2
        
        rNum1 = ratNumInitFromReal(r)
        rNum2 = ratNumSimplify(rNum1)
        print '(1x,a,f0.10,a)', 'testing "', r, '"'
        print '(5x,a)', 'Rational number is: ' // ratNumToStr(rNum1)
        print '(5x,a)', 'Simplified number is: ' // ratNumToStr(rNum2)
        print *, ' '
    end subroutine
    
    subroutine testRatNumMultiply_driver_int(num1, den1, num2, den2)
        integer, intent(in) :: num1, den1, num2, den2
        type(rationalnumber_t) :: rNum1, rNum2, res
        
        rNum1 = ratNumInit(num1, den1)
        rNum2 = ratNumInit(num2, den2)
        res = ratNumMultiply(rNum1, rNum2)
        print '(5x,a)', 'testing "' // ratNumToStr(rNum1) &
                           // ' * ' // ratNumToStr(rNum2) // '"'
        print '(5x,a)', ' = ' // ratNumToStr(res)
        print *, ' '
    end subroutine
    
    subroutine testRatNumMultiply_driver_real(a, b)
        real, intent(in) :: a, b
        type(rationalnumber_t) :: rNum1, rNum2, res
        
        rNum1 = ratNumInitFromReal(a)
        rNum2 = ratNumInitFromReal(b)
        res = ratNumMultiply(rNum1, rNum2, .true.)
        print '(5x,a)', 'testing "' // ratNumToStr(rNum1) &
                           // ' * ' // ratNumToStr(rNum2) // '"'
        print '(5x,a)', ' = ' // ratNumToStr(res)
        print *, ' '
    end subroutine
    
    ! 
    ! Helper procedures
    ! 
    
    subroutine getRatNum(r)
        type(rationalnumber_t), intent(inout) :: r
        integer :: tmp
        
        print *, 'Enter a rational number:'
        write(*,'(a)',advance='no') 'Enter numerator: '
        read(*,fmt='(i10)') tmp
        call ratNumSetNumerator(r, tmp)
        write(*,'(a)',advance='no') 'Enter denominator: '
        read(*,fmt='(i10)') tmp
        call ratNumSetDenominator(r, tmp)
    end subroutine
    
end program

































