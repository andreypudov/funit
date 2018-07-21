!
! A unit testing library for Fortran
!
! The MIT License
!
! Copyright 2011-2018 Andrey Pudov
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the 'Software'), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.
!

pure function arrayEquals_character(expected, actual) result(value)
    character, dimension(:), intent(in) :: expected
    character, dimension(:), intent(in) :: actual

    logical value

    if (size(expected) /= size(actual)) then
        value = .false.
        return
    end if

    value = all(expected == actual)
end function

pure function arrayEquals_complex(expected, actual, delta) result(value)
    complex, dimension(:), intent(in) :: expected
    complex, dimension(:), intent(in) :: actual
    real,                  intent(in) :: delta

    logical value

    if (size(expected) /= size(actual)) then
        value = .false.
        return
    end if

    value = (all(abs(real(expected) - real(actual)) < delta) .and. &
             all(abs(aimag(expected) - aimag(actual)) < delta))
end function

pure function arrayEquals_double_precision(expected, actual, delta) result(value)
    double precision, dimension(:), intent(in) :: expected
    double precision, dimension(:), intent(in) :: actual
    double precision,               intent(in) :: delta

    logical value

    if (size(expected) /= size(actual)) then
        value = .false.
        return
    end if

    value = all(dabs(expected - actual) < delta)
end function

pure function arrayEquals_integer(expected, actual) result(value)
    integer, dimension(:), intent(in) :: expected
    integer, dimension(:), intent(in) :: actual

    logical value

    if (size(expected) /= size(actual)) then
        value = .false.
        return
    end if

    value = all(expected == actual)
end function

pure function arrayEquals_logical(expected, actual) result(value)
    logical, dimension(:), intent(in) :: expected
    logical, dimension(:), intent(in) :: actual

    logical value

    if (size(expected) /= size(actual)) then
        value = .false.
        return
    end if

    value = all(expected .eqv. actual)
end function

pure function arrayEquals_real(expected, actual, delta) result(value)
    real, dimension(:), intent(in) :: expected
    real, dimension(:), intent(in) :: actual
    real,               intent(in) :: delta

    logical value

    if (size(expected) /= size(actual)) then
        value = .false.
        return
    end if

    value = all(abs(expected - actual) < delta)
end function