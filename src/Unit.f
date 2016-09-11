!
! A unit testing library for Fortran
!
! The MIT License
!
! Copyright 2011-2016 Andrey Pudov
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

module Unit

    implicit none
    private

    type, public :: Asserts
    private
    contains
        procedure, nopass, private :: arrayEquals_character
        procedure, nopass, private :: arrayEquals_complex
        procedure, nopass, private :: arrayEquals_double_precision
        procedure, nopass, private :: arrayEquals_integer
        procedure, nopass, private :: arrayEquals_logical
        procedure, nopass, private :: arrayEquals_real

        procedure, nopass, private :: equals_character
        procedure, nopass, private :: equals_complex
        procedure, nopass, private :: equals_double_precision
        procedure, nopass, private :: equals_integer
        procedure, nopass, private :: equals_logical
        procedure, nopass, private :: equals_real

        procedure, nopass, public :: false

        procedure, nopass, private :: notNull_character
        procedure, nopass, private :: notNull_complex
        procedure, nopass, private :: notNull_double_precision
        procedure, nopass, private :: notNull_integer
        procedure, nopass, private :: notNull_logical
        procedure, nopass, private :: notNull_real

        procedure, nopass, private :: notSame_character
        procedure, nopass, private :: notSame_complex
        procedure, nopass, private :: notSame_double_precision
        procedure, nopass, private :: notSame_integer
        procedure, nopass, private :: notSame_logical
        procedure, nopass, private :: notSame_real

        procedure, nopass, private :: null_character
        procedure, nopass, private :: null_complex
        procedure, nopass, private :: null_double_precision
        procedure, nopass, private :: null_integer
        procedure, nopass, private :: null_logical
        procedure, nopass, private :: null_real

        procedure, nopass, private :: same_character
        procedure, nopass, private :: same_complex
        procedure, nopass, private :: same_double_precision
        procedure, nopass, private :: same_integer
        procedure, nopass, private :: same_logical
        procedure, nopass, private :: same_real

        procedure, nopass, public :: true
        procedure, nopass, public :: fail

        generic :: arrayEquals => arrayEquals_character, &
                arrayEquals_complex, &
                arrayEquals_double_precision, &
                arrayEquals_integer, &
                arrayEquals_logical, &
                arrayEquals_real

        generic :: equals => equals_character, &
                equals_complex, &
                equals_double_precision, &
                    equals_integer, &
                equals_logical, &
                equals_real

        generic :: notNull => notNull_character, &
                notNull_complex, &
                notNull_double_precision, &
                notNull_integer, &
                notNull_logical, &
                notNull_real

        generic :: notSame => notSame_character, &
                notSame_complex, &
                notSame_double_precision, &
                notSame_integer, &
                notSame_logical, &
                notSame_real

        generic :: null => null_character, &
                null_complex, &
                null_double_precision, &
                null_integer, &
                null_logical, &
                null_real

        generic :: same => notSame_character, &
                same_complex, &
                same_double_precision, &
                same_integer, &
                same_logical, &
                same_real
    end type

    type, private :: UnitProcedureEntry
        procedure(UnitProcedure), pointer, nopass  :: procedure
        type(UnitProcedureEntry), pointer          :: next
    end type

    type, private :: UnitCaseEntry
        class(UnitCase), pointer     :: case
        type(UnitCaseEntry), pointer :: next
    end type

    type, public :: UnitCase
    private
        type(UnitProcedureEntry), pointer :: list => null()
        type(UnitProcedureEntry), pointer :: last => null()
    contains
        procedure, pass, public :: init  => init_case
        procedure, pass, public :: clean => clean_case

        procedure, pass, public :: add => add_case
        procedure, pass, public :: run => run_case
    end type

    type, public :: UnitSuite
    private
        type(UnitCaseEntry), pointer :: list => null()
        type(UnitCaseEntry), pointer :: last => null()
    contains
        procedure, pass, public :: init  => init_suite
        procedure, pass, public :: clean => clean_suite

        procedure, pass, public :: add => add_suite
        procedure, pass, public :: run => run_suite
    end type

    interface
        !
        ! ArrayEquals - asserts that two arrays are equal.
        !
        module subroutine arrayEquals_character(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            character, dimension(:), intent(in) :: expected
            character, dimension(:), intent(in) :: actual
        end subroutine

        module subroutine arrayEquals_complex(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            complex, dimension(:), intent(in) :: expected
            complex, dimension(:), intent(in) :: actual
            real,                  intent(in) :: delta
        end subroutine

        module subroutine arrayEquals_double_precision(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            double precision, dimension(:), intent(in) :: expected
            double precision, dimension(:), intent(in) :: actual
            double precision, intent(in)               :: delta
        end subroutine

        module subroutine arrayEquals_integer(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            integer, dimension(:), intent(in) :: expected
            integer, dimension(:), intent(in) :: actual
        end subroutine

        module subroutine arrayEquals_logical(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            logical, dimension(:), intent(in) :: expected
            logical, dimension(:), intent(in) :: actual
        end subroutine

        module subroutine arrayEquals_real(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            real, dimension(:), intent(in) :: expected
            real, dimension(:), intent(in) :: actual
            real,               intent(in) :: delta
        end subroutine

        !
        ! Equals - asserts that two values are equal.
        !
        module subroutine equals_character(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            character, intent(in) :: expected
            character, intent(in) :: actual
        end subroutine

        module subroutine equals_complex(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            complex, intent(in) :: expected
            complex, intent(in) :: actual
            real,    intent(in) :: delta
        end subroutine

        module subroutine equals_double_precision(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            double precision, intent(in) :: expected
            double precision, intent(in) :: actual
            double precision, intent(in) :: delta
        end subroutine

        module subroutine equals_integer(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            integer, intent(in) :: expected
            integer, intent(in) :: actual
        end subroutine

        module subroutine equals_logical(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            logical, intent(in) :: expected
            logical, intent(in) :: actual
        end subroutine

        module subroutine equals_real(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            real, intent(in) :: expected
            real, intent(in) :: actual
            real, intent(in) :: delta
        end subroutine

        !
        ! False - asserts that a condition is false.
        !
        module subroutine false(condition, message)
            character(len=*), optional, intent(in) :: message
            logical, intent(in) :: condition
        end subroutine

        !
        ! NutNull - asserts that a pointer isn't null (associated).
        !
        module subroutine notNull_character(pointer, message)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: pointer
        end subroutine

        module subroutine notNull_complex(pointer, message)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: pointer
        end subroutine

        module subroutine notNull_double_precision(pointer, message)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: pointer
        end subroutine

        module subroutine notNull_integer(pointer, message)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: pointer
        end subroutine

        module subroutine notNull_logical(pointer, message)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: pointer
        end subroutine

        module subroutine notNull_real(pointer, message)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: pointer
        end subroutine

        !
        ! NotSame - asserts that two pointers do not refer to the same target.
        !
        module subroutine notSame_character(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: unexpected
            character,        pointer,  intent(in) :: actual
        end subroutine

        module subroutine notSame_complex(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: unexpected
            complex,          pointer, intent(in)  :: actual
        end subroutine

        module subroutine notSame_double_precision(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: unexpected
            double precision, pointer,  intent(in) :: actual
        end subroutine

        module subroutine notSame_integer(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: unexpected
            integer,          pointer,  intent(in) :: actual
        end subroutine

        module subroutine notSame_logical(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: unexpected
            logical,          pointer,  intent(in) :: actual
        end subroutine

        module subroutine notSame_real(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: unexpected
            real,             pointer,  intent(in) :: actual
        end subroutine

        !
        ! Null - asserts that a pointer is null (not associated).
        !
        module subroutine null_character(pointer, message)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: pointer
        end subroutine

        module subroutine null_complex(pointer, message)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: pointer
        end subroutine

        module subroutine null_double_precision(pointer, message)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: pointer
        end subroutine

        module subroutine null_integer(pointer, message)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: pointer
        end subroutine

        module subroutine null_logical(pointer, message)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: pointer
        end subroutine

        module subroutine null_real(pointer, message)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: pointer
        end subroutine

        !
        ! Same - asserts that two pointers refer to the same target.
        !
        module subroutine same_character(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: unexpected
            character,        pointer,  intent(in) :: actual
        end subroutine

        module subroutine same_complex(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: unexpected
            complex,          pointer, intent(in)  :: actual
        end subroutine

        module subroutine same_double_precision(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: unexpected
            double precision, pointer,  intent(in) :: actual
        end subroutine

        module subroutine same_integer(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: unexpected
            integer,          pointer,  intent(in) :: actual
        end subroutine

        module subroutine same_logical(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: unexpected
            logical,          pointer,  intent(in) :: actual
        end subroutine

        module subroutine same_real(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: unexpected
            real,             pointer,  intent(in) :: actual
        end subroutine

        !
        ! True - asserts that a condition is true.
        !
        module subroutine true(condition, message)
            character(len=*), optional, intent(in) :: message
            logical, intent(in) :: condition
        end subroutine

        !
        ! Fail - fails a test with the given message.
        !
        module subroutine fail(message)
            character(len=*), optional, intent(in) :: message
        end subroutine
    end interface

    abstract interface
        subroutine UnitProcedure(self)
            import UnitCase
            class(UnitCase), intent(in out) :: self
        end subroutine
    end interface

    interface
        module subroutine init_case(self)
            class(UnitCase), intent(in out) :: self
        end subroutine

        module subroutine clean_case(self)
            class(UnitCase), intent(in out) :: self
        end subroutine

        module subroutine add_case(self, procedure)
            class(UnitCase), intent(in out)               :: self
            procedure(UnitProcedure), pointer, intent(in) :: procedure
        end subroutine

        module subroutine run_case(self)
            class(UnitCase), intent(in out) :: self
        end subroutine
    end interface

    interface
        module subroutine init_suite(self)
            class(UnitSuite), intent(in out) :: self
        end subroutine

        module subroutine clean_suite(self)
            class(UnitSuite), intent(in out) :: self
        end subroutine

        module subroutine add_suite(self, case)
            class(UnitSuite), intent(in out)     :: self
            class(UnitCase), pointer, intent(in) :: case
        end subroutine

        module subroutine run_suite(self)
            class(UnitSuite), intent(in) :: self
        end subroutine
    end interface
end module
