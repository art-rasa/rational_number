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
        print *, 'testing "12.34"'
        print '(5x,a,f0.10)', 'Real number is: ', r
        print '(5x,a)', 'Rational number is: ' // ratNumToStr(rNum1)
        print '(5x,a)', 'Simplified number is: ' // ratNumToStr(rNum2)
        print *, ' '
        
        r = -15357.63
        rNum1 = ratNumInitFromReal(r)
        rNum2 = ratNumSimplify(rNum1)
        print *, 'testing "-15357.63"'
        print '(5x,a,f0.10)', 'Real number is: ', r
        print '(5x,a)', 'Rational number is: ' // ratNumToStr(rNum1)
        print '(5x,a)', 'Simplified number is: ' // ratNumToStr(rNum2)
        print *, ' '
        
        r = -160000.002
        rNum1 = ratNumInitFromReal(r)
        rNum2 = ratNumSimplify(rNum1)
        print *, 'testing "-160000.002"'
        print '(5x,a,f0.10)', 'Real number is: ', r
        print '(5x,a)', 'Rational number is: ' // ratNumToStr(rNum1)
        print '(5x,a)', 'Simplified number is: ' // ratNumToStr(rNum2)
        print *, ' '
        
        r = -160000.02
        rNum1 = ratNumInitFromReal(r)
        rNum2 = ratNumSimplify(rNum1)
        print *, 'testing "-160000.02"'
        print '(5x,a,f0.10)', 'Real number is: ', r
        print '(5x,a)', 'Rational number is: ' // ratNumToStr(rNum1)
        print '(5x,a)', 'Simplified number is: ' // ratNumToStr(rNum2)
        print *, ' '
        
        r = -160000.2
        rNum1 = ratNumInitFromReal(r)
        rNum2 = ratNumSimplify(rNum1)
        print *, 'testing "-160000.2"'
        print '(5x,a,f0.10)', 'Real number is: ', r
        print '(5x,a)', 'Rational number is: ' // ratNumToStr(rNum1)
        print '(5x,a)', 'Simplified number is: ' // ratNumToStr(rNum2)
        print *, ' '
        
        r = 445.69937
        rNum1 = ratNumInitFromReal(r)
        rNum2 = ratNumSimplify(rNum1)
        print *, 'testing "445.69937"'
        print '(5x,a,f0.10)', 'Real number is: ', r
        print '(5x,a)', 'Rational number is: ' // ratNumToStr(rNum1)
        print '(5x,a)', 'Simplified number is: ' // ratNumToStr(rNum2)
        print *, ' '
        
        r = 545871.616
        rNum1 = ratNumInitFromReal(r)
        rNum2 = ratNumSimplify(rNum1)
        print *, 'testing "545871.616"'
        print '(5x,a,f0.10)', 'Real number is: ', r
        print '(5x,a)', 'Rational number is: ' // ratNumToStr(rNum1)
        print '(5x,a)', 'Simplified number is: ' // ratNumToStr(rNum2)
        print *, ' '
        
        r = 313.96153
        rNum1 = ratNumInitFromReal(r)
        rNum2 = ratNumSimplify(rNum1)
        print *, 'testing "313.96153"'
        print '(5x,a,f0.10)', 'Real number is: ', r
        print '(5x,a)', 'Rational number is: ' // ratNumToStr(rNum1)
        print '(5x,a)', 'Simplified number is: ' // ratNumToStr(rNum2)
        print *, ' '
        
        r = -86.622
        rNum1 = ratNumInitFromReal(r)
        rNum2 = ratNumSimplify(rNum1)
        print *, 'testing "-86.622"'
        print '(5x,a,f0.10)', 'Real number is: ', r
        print '(5x,a)', 'Rational number is: ' // ratNumToStr(rNum1)
        print '(5x,a)', 'Simplified number is: ' // ratNumToStr(rNum2)
        print *, ' '
        
        r = 8.53645
        rNum1 = ratNumInitFromReal(r)
        rNum2 = ratNumSimplify(rNum1)
        print *, 'testing "8.53645"'
        print '(5x,a,f0.10)', 'Real number is: ', r
        print '(5x,a)', 'Rational number is: ' // ratNumToStr(rNum1)
        print '(5x,a)', 'Simplified number is: ' // ratNumToStr(rNum2)
        print *, ' '
        
        print *, ':::: End of test ::::'
    end subroutine
    
    subroutine testRatNumMultiply()
        type(rationalnumber_t) :: res
        
        print *, ':::: testRatNumMultiply() ::::'
        print *, ' '
        rNum1 = ratNumInit(2, 6)
        rNum2 = ratNumInit(3, 7)
        res = ratNumMultiply(rNum1, rNum2)
        print '(5x,a)', 'testing "' // ratNumToStr(rNum1) // ' * ' // ratNumToStr(rNum2) // '"'
        print '(5x,a)', ' = ' // ratNumToStr(res)
        print *, ' '
        
        rNum1 = ratNumInit(2, 6)
        rNum2 = ratNumInit(3, 7)
        res = ratNumMultiply(rNum1, rNum2, .true.)
        print '(5x,a)', 'testing "' // ratNumToStr(rNum1) // ' * ' // ratNumToStr(rNum2) // '"'
        print '(5x,a)', ' = ' // ratNumToStr(res)
        print *, ' '
        
        rNum1 = ratNumInit(2142, 653)
        rNum2 = ratNumInit(-2733, 7361)
        res = ratNumMultiply(rNum1, rNum2, .false.)
        print '(5x,a)', 'testing "' // ratNumToStr(rNum1) // ' * ' // ratNumToStr(rNum2) // '"'
        print '(5x,a)', ' = ' // ratNumToStr(res)
        print *, ' '
        
        rNum1 = ratNumInit(2142, 653)
        rNum2 = ratNumInit(-2733, 7361)
        res = ratNumMultiply(rNum1, rNum2, .true.)
        print '(5x,a)', 'testing "' // ratNumToStr(rNum1) // ' * ' // ratNumToStr(rNum2) // '"'
        print '(5x,a)', ' = ' // ratNumToStr(res)
        print *, ' '
        
        rNum1 = ratNumInit(-2, 1)
        rNum2 = ratNumInit(-1, 1)
        res = ratNumMultiply(rNum1, rNum2, .false.)
        print '(5x,a)', 'testing "' // ratNumToStr(rNum1) // ' * ' // ratNumToStr(rNum2) // '"'
        print '(5x,a)', ' = ' // ratNumToStr(res)
        print *, ' '
        
        rNum1 = ratNumInit(-54, 1)
        rNum2 = ratNumInit(-1, 632)
        res = ratNumMultiply(rNum1, rNum2, .true.)
        print '(5x,a)', 'testing "' // ratNumToStr(rNum1) // ' * ' // ratNumToStr(rNum2) // '"'
        print '(5x,a)', ' = ' // ratNumToStr(res)
        print *, ' '
        
        rNum1 = ratNumInit(-632, 1)
        rNum2 = ratNumInit(-1, 632)
        res = ratNumMultiply(rNum1, rNum2, .true.)
        print '(5x,a)', 'testing "' // ratNumToStr(rNum1) // ' * ' // ratNumToStr(rNum2) // '"'
        print '(5x,a)', ' = ' // ratNumToStr(res)
        print *, ' '
        
        rNum1 = ratNumInit(-11, 764)
        rNum2 = ratNumInitFromReal(43.637)
        res = ratNumMultiply(rNum1, rNum2, .true.)
        print '(5x,a)', 'testing "' // ratNumToStr(rNum1) // ' * ' // ratNumToStr(rNum2) // '"'
        print '(5x,a)', ' = ' // ratNumToStr(res)
        print *, ' '
        
        rNum1 = ratNumInitFromReal(-1.05)
        rNum2 = ratNumInitFromReal(-2.6)
        res = ratNumMultiply(rNum1, rNum2, .true.)
        print '(5x,a)', 'testing "' // ratNumToStr(rNum1) // ' * ' // ratNumToStr(rNum2) // '"'
        print '(5x,a)', ' = ' // ratNumToStr(res)
        print *, ' '
        
        print *, ':::: End of test ::::'
    end subroutine
    
end program

































