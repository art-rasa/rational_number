! 
! Simple test program for the rational.number module.
! 
program rational_number_test
    use rational_number
    implicit none
    
    type(rationalnumber_t) :: rNum1, rNum2, rNum3
    integer :: tmp
    character(:), allocatable :: rNum1_str, rNum2_str, rNum3_str
    
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
    
    rNum3 = ratNumAdd(rNum1, rNum2)
    rNum1_str = ratNumToStr(rNum1)
    rNum2_str = ratNumToStr(rNum2)
    rNum3_str = ratNumToStr(rNum3)
    
    print *, ' '
    print *, 'Addition r1 + r2:'
    print '(5x,a)', trim(rNum1_str) // ' + ' // trim(rNum2_str) // ' = ' // trim(rNum3_str)
    deallocate(rNum1_str)
    deallocate(rNum2_str)
    deallocate(rNum3_str)
    
    rNum1 = ratNumSimplify(rNum1)
    rNum2 = ratNumSimplify(rNum2)
    rNum3 = ratNumAdd(rNum1, rNum2)
    rNum3 = ratNumSimplify(rNum3)
    
    rNum1_str = ratNumToStr(rNum1)
    rNum2_str = ratNumToStr(rNum2)
    rNum3_str = ratNumToStr(rNum3)
    print *, ' '
    print *, 'Simplified addition r1 + r2:'
    print '(5x,a)', trim(rNum1_str) // ' + ' // trim(rNum2_str) // ' = ' // trim(rNum3_str)
    deallocate(rNum1_str)
    deallocate(rNum2_str)
    deallocate(rNum3_str)
    
    print *, ' '
    print *, 'As Real numbers:'
    print *, ratNumToReal(rNum1), '+', ratNumToReal(rNum2), '=', ratNumToReal(rNum3)
    
    print *, ' '
    print *, ':::: End of test ::::'
    
end program

































