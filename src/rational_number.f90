module rational_number
    use math , only: gcDenom, realNumDecimals, realWholePart, realFracPart
    implicit none
    
    type rationalnumber_t
        integer :: numerator
        integer :: denominator
    end type
    
contains
    
    !  
    !  name: ratNumInit
    !  @param num Integer numerator value.
    !  @param den Integer denominator value. Must not be 0.
    !  @return Rational number initialized with given parameters.
    !          Input parameters will not be modified.
    !  
    pure function ratNumInit(num, den) result(rNum)
        integer, intent(in) :: num
        integer, intent(in) :: den
        type(rationalnumber_t) :: rNum
        integer :: num_temp
        integer :: den_temp
        
        num_temp = num
        den_temp = den
        
        if (den_temp < 0) then
            den_temp = abs(den_temp)
            num_temp = 0 - num_temp
        end if
        
        if (den_temp > 0) then
            rNum % numerator = num_temp
            rNum % denominator = den_temp
        end if
    end function
    
    !  
    !  name: ratNumInitFromReal
    !  desc: Returns a rational approximation of "realNum".
    !  @param realNum real variable.
    !  @return Rational representation of the real number 'realNum'.
    !  
    function ratNumInitFromReal(realNum) result(rNum)
        real, intent(in) :: realNum
        type(rationalnumber_t) :: rNum
        integer :: numerator
        integer :: denominator
        integer :: n_dec
        integer :: expo
        real :: temp
        
        n_dec = realNumDecimals(realNum)
        expo = 10 ** n_dec
        
        numerator = realWholePart(realNum) * expo
        numerator = numerator + realFracPart(realNum)
        denominator = expo
        
        rNum = ratNumInit(numerator, denominator)
    end function
    
    !  
    !  name: ratNumToStr
    !  desc: Returns a string representation of "rNum".
    !  @param rNum rationalnumber_t variable.
    !  @return String representation of the rational number 'rNum'.
    !  
    function ratNumToStr(rNum) result(res)
        type(rationalnumber_t), intent(in) :: rNum
        character(len=:), allocatable :: res
        character(len=20) :: tmp_a
        character(len=20) :: tmp_b
        
        write(tmp_a, '(i0)') rNum % numerator
        
        if ( (rNum % denominator /= 1) .and. (rNum % numerator /= 0) ) then
            write(tmp_b, '(i0)') rNum % denominator
            res = trim(tmp_a) // '/' // trim(tmp_b)
        else
            res = trim(tmp_a)
        end if
    end function
    
    !  
    !  name: ratNumEqualize
    !  desc: Make r1's and r2's denominators equal.
    !  @param r1 rationalnumber_t variable. Will be modified.
    !  @param r2 rationalnumber_t variable. Will be modified.
    !  
    subroutine ratNumEqualize(r1, r2) 
        type(rationalnumber_t), intent(inout) :: r1, r2
        integer :: r1_num, r2_num, r1_den, r2_den
        
        if (r1 % denominator /= r2 % denominator) then
            r1_den = r1 % denominator * r2 % denominator
            r2_den = r1 % denominator * r2 % denominator
            r1_num = r1 % numerator * r2 % denominator
            r2_num = r2 % numerator * r1 % denominator
            
            r1 % numerator = r1_num
            r1 % denominator = r1_den
            
            r2 % numerator = r2_num
            r2 % denominator = r2_den
        end if
        
    end subroutine
    
    !  
    !  name: ratNumSimplify
    !  desc: Computes a simplified rational number from its input parameter.
    !  @param r: Rational number to be simplified. Will not be modified.
    !  @return Simplified rational number.
    !  
    pure function ratNumSimplify(rNum) result(res)
        type(rationalnumber_t), intent(in) :: rNum
        integer :: gcd, num, den
        type(rationalnumber_t) :: res
        
        num = rNum % numerator
        den = rNum % denominator
        
        gcd = gcDenom(num, den)
        
        if (gcd > 1) then
            num = num / gcd
            den = den / gcd
        end if
        
        res = ratNumInit(num, den)
    end function
    
    !  
    !  name: ratNumAdd
    !  desc: Add two rational numbers.
    !  @param r1: Rational number.
    !  @param r2: Rational number.
    !  @param simplify: Optional parameter.
    !  @return The sum of two rational numbers.
    !  
    function ratNumAdd(r1, r2, simplify) result(res)
        type(rationalnumber_t), intent(in) :: r1, r2
        logical, intent(in), optional :: simplify
        type(rationalnumber_t) :: res
        type(rationalnumber_t) :: r1_eq, r2_eq
        
        r1_eq = r1
        r2_eq = r2
        
        if ( present(simplify) ) then
            if (simplify) then
                r1_eq = ratNumSimplify(r1_eq)
                r2_eq = ratNumSimplify(r2_eq)
            end if
        end if
        
        call ratNumEqualize(r1_eq, r2_eq)
        
        res = ratNumInit(r1_eq % numerator + r2_eq % numerator, r1_eq % denominator)
        
        if ( present(simplify) ) then
            if (simplify) then
                res = ratNumSimplify(res)
            end if
        end if
    end function
    
    !  
    !  name: ratNumSubstract
    !  desc: Substracts two rational numbers.
    !  @param r1: Rational number.
    !  @param r2: Rational number.
    !  @param simplify: Optional parameter.
    !  @return: The difference (r1-r2) between two rational numbers.
    !  
    function ratNumSubstract(r1, r2, simplify) result(res)
        type(rationalnumber_t), intent(in) :: r1, r2
        logical, intent(in), optional :: simplify
        type(rationalnumber_t) :: res
        type(rationalnumber_t) :: r2_tmp
        integer :: new_num
        
        r2_tmp = r2
        new_num = ratNumGetNumerator(r2_tmp)
        call ratNumSetNumerator(r2_tmp, -new_num)
        
        res = ratNumAdd(r1, r2_tmp, simplify)
    end function
    
    !  
    !  name: ratNumMultiply
    !  desc: Multiplies two rational numbers.
    !  @param r1: Rational number.
    !  @param r2: Rational number.
    !  @param simplify: Optional parameter.
    !  @return: The multiplication (r1 * r2) of two rational numbers.
    !  
    function ratNumMultiply(r1, r2, simplify) result(res)
        type(rationalnumber_t), intent(in) :: r1, r2
        logical, intent(in), optional :: simplify
        type(rationalnumber_t) :: res
        integer :: num, den
        type(rationalnumber_t) :: r1_eq, r2_eq
        
        r1_eq = r1
        r2_eq = r2
        
        if (present(simplify)) then
            if (simplify) then
                r1_eq = ratNumSimplify(r1_eq)
                r2_eq = ratNumSimplify(r2_eq)
            end if
        end if
        
        num = ratNumGetNumerator(r1_eq) * ratNumGetNumerator(r2_eq)
        den = ratNumGetDenominator(r1_eq) * ratNumGetDenominator(r2_eq)
        res = ratNumInit(num, den)
        
        if (present(simplify)) then
            if (simplify) then
                res = ratNumSimplify(res)
            end if
        end if
    end function
    
    !  
    !  name: ratNumSetNumerator
    !  desc: Modify the numerator.
    !  @param r: Rational number. Will be modified.
    !  @param n: Integer value to set.
    !  
    subroutine ratNumSetNumerator(r, n)
        type(rationalnumber_t), intent(inout) :: r
        integer, intent(in) :: n
        
        r % numerator = n
    end subroutine
    
    !  
    !  name: ratNumGetNumerator
    !  desc: Returns the numerator.
    !  @param r: Rational number. Will not be modified.
    !  @return: Numerator value as an integer.
    !  
    function ratNumGetNumerator(r) result(num)
        type(rationalnumber_t), intent(in) :: r
        integer :: num
        
        num = r % numerator
    end function
    
    !  
    !  name: ratNumSetDenominator
    !  desc: Modify the denominator.
    !  @param r: Rational number. Will be modified.
    !  @param d: Integer value to set. Must not be zero.
    !  
    subroutine ratNumSetDenominator(r, d)
        type(rationalnumber_t), intent(inout) :: r
        integer, intent(in) :: d
        integer :: num
        
        if (d /= 0) then
            if (d > 0) then
                r % denominator = d
            
            else
                num = r % numerator
                r % denominator = abs(d)
                call ratNumSetNumerator(r, -num)
            end if
        end if
    end subroutine
    
    !  
    !  name: ratNumGetDenominator
    !  desc: Returns the denominator.
    !  @param r: Rational number. Will not be modified.
    !  @return: Denominator value as an integer.
    !  
    function ratNumGetDenominator(r) result(den)
        type(rationalnumber_t), intent(in) :: r
        integer :: den
        
        den = r % denominator
    end function
    
    !  
    !  name: ratNumToReal
    !  desc: Convert the rational number to a real number.
    !  @param r: Rational number. 
    !  @return real number.
    !  
    function ratNumToReal(r) result(f)
        type(rationalnumber_t) , intent(in) :: r
        real :: f
        f = 0.0
        if (r % denominator > 0.0) then
            f = real(r % numerator) / real(r % denominator)
        end if
    end function
    
end module
































