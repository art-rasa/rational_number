! 
! Simple test program for the rational.number module.
! 
program rational_number_test
    use rational_number
    implicit none
    
    type(rationalnumber_t) :: rNum1, rNum2, rNum3
    !integer :: tmp
    integer :: selection
    
    print *, '-----program rational_number_test-----'
    print *, 'Select feature to test:'
    print *, '  1: testRatNumSimplify()'
    print *, '  2: testRatNumInitFromReal()'
    print *, '  3: ()'
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
        
    case (0)
        call testRatNumSimplify()
        call testRatNumInitFromReal()
    end select
    
contains
    
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
    
    subroutine testRatNumSimplify()
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
        real :: r
        
        print *, ':::: testRatNumInitFromReal() ::::'
        print *, ' '
        
        r = 12.34
        rNum1 = ratNumInitFromReal(r)
        rNum2 = ratNumSimplify(rNum1)
        
        print *, ' '
        print '(5x,a,f0.10)', 'Real number is: ', r
        print '(5x,a)', 'Rational number is: ' // ratNumToStr(rNum1)
        print '(5x,a)', 'Simplified number is: ' // ratNumToStr(rNum2)
        
        print *, ':::: End of test ::::'
    end subroutine
    
end program

































