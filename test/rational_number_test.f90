! 
! Simple test program for the rational.number module.
! 
program rational_number_test
    use rational_number
    implicit none
    
    type(rationalnumber_t) :: rNum1, rNum2, rNum3
    integer :: tmp
    
    rNum1 = ratNumInit(1, 1)
    rNum2 = ratNumInit(1, 1)
    
    print *, ':::: testing rational_number module ::::'
    print *, ' '
    
    print *, 'Rational number 1'
    write(*,'(a)',advance='no') 'Enter numerator: '
    read(*,fmt='(i10)') tmp
    call ratNumSetNumerator(rNum1, tmp)
    write(*,'(a)',advance='no') 'Enter denominator: '
    read(*,fmt='(i10)') tmp
    call ratNumSetDenominator(rNum1, tmp)
    
    print *, ' '
    print *, 'Rational number 2'
    write(*,'(a)',advance='no') 'Enter numerator: '
    read(*,fmt='(i10)') tmp
    call ratNumSetNumerator(rNum2, tmp)
    write(*,'(a)',advance='no') 'Enter denominator: '
    read(*,fmt='(i10)') tmp
    call ratNumSetDenominator(rNum2, tmp)
    
    print *, ' '
    print *, 'Original rational numbers:'
    print '(5x,a)', 'r1 = ' // ratNumToStr(rNum1) // &
                   ', r2 = ' // ratNumToStr(rNum2)
    print *, 'Simplified rational numbers:'
    rNum1 = ratNumSimplify(rNum1)
    rNum2 = ratNumSimplify(rNum2)
    print '(5x,a)', 'r1 = ' // ratNumToStr(rNum1) // &
                   ', r2 = ' // ratNumToStr(rNum2)
    
    rNum3 = ratNumAdd(rNum1, rNum2)
    
    print *, ' '
    print *, 'Addition r1 + r2:'
    print '(5x,a)', ratNumToStr(rNum1) // ' + ' // &
                    ratNumToStr(rNum2) // ' = ' // &
                    ratNumToStr(rNum3)
    
    rNum3 = ratNumSimplify(rNum3)
    
    print *, 'Simplified addition r1 + r2:'
    print '(5x,a)', ratNumToStr(rNum1) // ' + ' // &
                    ratNumToStr(rNum2) // ' = ' // &
                    ratNumToStr(rNum3)
    
    print *, 'As Real numbers:'
    print *, ratNumToReal(rNum1), '+', ratNumToReal(rNum2), '=', &
             ratNumToReal(rNum3)
    
    rNum3 = ratNumSubstract(rNum1, rNum2)
    
    print *, ' '
    print *, 'Substraction r1 - r2: '
    print '(5x,a)', ratNumToStr(rNum1) // ' - ' // &
                    ratNumToStr(rNum2) // ' = ' // &
                    ratNumToStr(rNum3)
    
    rNum3 = ratNumSimplify(rNum3)
    
    print *, 'Simplified substraction r1 - r2: '
    print '(5x,a)', ratNumToStr(rNum1) // ' - ' // &
                    ratNumToStr(rNum2) // ' = ' // &
                    ratNumToStr(rNum3)
    
    print *, 'As Real numbers:'
    print *, ratNumToReal(rNum1), '-', ratNumToReal(rNum2), '=', &
             ratNumToReal(rNum3)
    
    print *, ' '
    print *, ':::: End of test ::::'
    
end program

































