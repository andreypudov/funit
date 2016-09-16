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

    use Logger

    implicit none
    private

    type, public :: Asserts
    private
    contains
        procedure, nopass, private :: arrayEquals_assert_character
        procedure, nopass, private :: arrayEquals_assert_complex
        procedure, nopass, private :: arrayEquals_assert_double_precision
        procedure, nopass, private :: arrayEquals_assert_integer
        procedure, nopass, private :: arrayEquals_assert_logical
        procedure, nopass, private :: arrayEquals_assert_real

        procedure, nopass, private :: equals_assert_character
        procedure, nopass, private :: equals_assert_complex
        procedure, nopass, private :: equals_assert_double_precision
        procedure, nopass, private :: equals_assert_integer
        procedure, nopass, private :: equals_assert_logical
        procedure, nopass, private :: equals_assert_real

        procedure, nopass, public :: false => false_assert

        procedure, nopass, private :: notNull_assert_character
        procedure, nopass, private :: notNull_assert_complex
        procedure, nopass, private :: notNull_assert_double_precision
        procedure, nopass, private :: notNull_assert_integer
        procedure, nopass, private :: notNull_assert_logical
        procedure, nopass, private :: notNull_assert_real

        procedure, nopass, private :: notSame_assert_character
        procedure, nopass, private :: notSame_assert_complex
        procedure, nopass, private :: notSame_assert_double_precision
        procedure, nopass, private :: notSame_assert_integer
        procedure, nopass, private :: notSame_assert_logical
        procedure, nopass, private :: notSame_assert_real

        procedure, nopass, private :: null_assert_character
        procedure, nopass, private :: null_assert_complex
        procedure, nopass, private :: null_assert_double_precision
        procedure, nopass, private :: null_assert_integer
        procedure, nopass, private :: null_assert_logical
        procedure, nopass, private :: null_assert_real

        procedure, nopass, private :: same_assert_character
        procedure, nopass, private :: same_assert_complex
        procedure, nopass, private :: same_assert_double_precision
        procedure, nopass, private :: same_assert_integer
        procedure, nopass, private :: same_assert_logical
        procedure, nopass, private :: same_assert_real

        procedure, nopass, public :: true => true_assert
        procedure, nopass, public :: fail => fail_assert

        generic :: arrayEquals => arrayEquals_assert_character, &
                arrayEquals_assert_complex, &
                arrayEquals_assert_double_precision, &
                arrayEquals_assert_integer, &
                arrayEquals_assert_logical, &
                arrayEquals_assert_real

        generic :: equals => equals_assert_character, &
                equals_assert_complex, &
                equals_assert_double_precision, &
                equals_assert_integer, &
                equals_assert_logical, &
                equals_assert_real

        generic :: notNull => notNull_assert_character, &
                notNull_assert_complex, &
                notNull_assert_double_precision, &
                notNull_assert_integer, &
                notNull_assert_logical, &
                notNull_assert_real

        generic :: notSame => notSame_assert_character, &
                notSame_assert_complex, &
                notSame_assert_double_precision, &
                notSame_assert_integer, &
                notSame_assert_logical, &
                notSame_assert_real

        generic :: null => null_assert_character, &
                null_assert_complex, &
                null_assert_double_precision, &
                null_assert_integer, &
                null_assert_logical, &
                null_assert_real

        generic :: same => notSame_assert_character, &
                same_assert_complex, &
                same_assert_double_precision, &
                same_assert_integer, &
                same_assert_logical, &
                same_assert_real
    end type

    type, public :: Expects
    private
    contains
        procedure, nopass, private :: arrayEquals_expect_character
        procedure, nopass, private :: arrayEquals_expect_complex
        procedure, nopass, private :: arrayEquals_expect_double_precision
        procedure, nopass, private :: arrayEquals_expect_integer
        procedure, nopass, private :: arrayEquals_expect_logical
        procedure, nopass, private :: arrayEquals_expect_real

        procedure, nopass, private :: equals_expect_character
        procedure, nopass, private :: equals_expect_complex
        procedure, nopass, private :: equals_expect_double_precision
        procedure, nopass, private :: equals_expect_integer
        procedure, nopass, private :: equals_expect_logical
        procedure, nopass, private :: equals_expect_real

        procedure, nopass, public :: false => false_expect

        procedure, nopass, private :: notNull_expect_character
        procedure, nopass, private :: notNull_expect_complex
        procedure, nopass, private :: notNull_expect_double_precision
        procedure, nopass, private :: notNull_expect_integer
        procedure, nopass, private :: notNull_expect_logical
        procedure, nopass, private :: notNull_expect_real

        procedure, nopass, private :: notSame_expect_character
        procedure, nopass, private :: notSame_expect_complex
        procedure, nopass, private :: notSame_expect_double_precision
        procedure, nopass, private :: notSame_expect_integer
        procedure, nopass, private :: notSame_expect_logical
        procedure, nopass, private :: notSame_expect_real

        procedure, nopass, private :: null_expect_character
        procedure, nopass, private :: null_expect_complex
        procedure, nopass, private :: null_expect_double_precision
        procedure, nopass, private :: null_expect_integer
        procedure, nopass, private :: null_expect_logical
        procedure, nopass, private :: null_expect_real

        procedure, nopass, private :: same_expect_character
        procedure, nopass, private :: same_expect_complex
        procedure, nopass, private :: same_expect_double_precision
        procedure, nopass, private :: same_expect_integer
        procedure, nopass, private :: same_expect_logical
        procedure, nopass, private :: same_expect_real

        procedure, nopass, public :: true => true_expect
        procedure, nopass, public :: fail => fail_expect

        generic :: arrayEquals => arrayEquals_expect_character, &
                arrayEquals_expect_complex, &
                arrayEquals_expect_double_precision, &
                arrayEquals_expect_integer, &
                arrayEquals_expect_logical, &
                arrayEquals_expect_real

        generic :: equals => equals_expect_character, &
                equals_expect_complex, &
                equals_expect_double_precision, &
                equals_expect_integer, &
                equals_expect_logical, &
                equals_expect_real

        generic :: notNull => notNull_expect_character, &
                notNull_expect_complex, &
                notNull_expect_double_precision, &
                notNull_expect_integer, &
                notNull_expect_logical, &
                notNull_expect_real

        generic :: notSame => notSame_expect_character, &
                notSame_expect_complex, &
                notSame_expect_double_precision, &
                notSame_expect_integer, &
                notSame_expect_logical, &
                notSame_expect_real

        generic :: null => null_expect_character, &
                null_expect_complex, &
                null_expect_double_precision, &
                null_expect_integer, &
                null_expect_logical, &
                null_expect_real

        generic :: same => notSame_expect_character, &
                same_expect_complex, &
                same_expect_double_precision, &
                same_expect_integer, &
                same_expect_logical, &
                same_expect_real
    end type

    type, public :: UnitCase
    private
        type(UnitProcedureEntry), pointer :: list => null()
        type(UnitProcedureEntry), pointer :: last => null()
        character(len=:),         pointer :: name => null()
    contains
        procedure, pass, public :: init  => init_case
        procedure, pass, public :: clean => clean_case

        procedure, pass, public :: add => add_case
        procedure, pass, public :: run => run_case
    end type

    type, public :: UnitSuite
    private
        type(UnitCaseEntry), pointer :: list   => null()
        type(UnitCaseEntry), pointer :: last   => null()
        class(UnitLogger),   pointer :: logger => null()
        character(len=:),    pointer :: name   => null()
    contains
        procedure, pass, public :: init  => init_suite
        procedure, pass, public :: clean => clean_suite

        procedure, pass, public :: add => add_suite
        procedure, pass, public :: run => run_suite
    end type

    type, private :: UnitContext
    private
    contains
        procedure, nopass, public :: getContext => getContext_context
        procedure, pass,   public :: clean      => clean_context

        procedure, pass,   public :: getLogger  => getLogger_context
    end type

    type, private :: UnitProcedureEntry
        procedure(UnitProcedure), pointer, nopass  :: procedure => null()
        type(UnitProcedureEntry), pointer          :: next      => null()
        character(len=:),         pointer          :: name      => null()
    end type

    type, private :: UnitCaseEntry
        class(UnitCase),     pointer :: case => null()
        type(UnitCaseEntry), pointer :: next => null()
    end type

    interface
        !
        ! ArrayEquals - asserts that two arrays are equal.
        !
        module subroutine arrayEquals_assert_character(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            character, dimension(:), intent(in) :: expected
            character, dimension(:), intent(in) :: actual
        end subroutine

        module subroutine arrayEquals_assert_complex(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            complex, dimension(:), intent(in) :: expected
            complex, dimension(:), intent(in) :: actual
            real,                  intent(in) :: delta
        end subroutine

        module subroutine arrayEquals_assert_double_precision(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            double precision, dimension(:), intent(in) :: expected
            double precision, dimension(:), intent(in) :: actual
            double precision, intent(in)               :: delta
        end subroutine

        module subroutine arrayEquals_assert_integer(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            integer, dimension(:), intent(in) :: expected
            integer, dimension(:), intent(in) :: actual
        end subroutine

        module subroutine arrayEquals_assert_logical(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            logical, dimension(:), intent(in) :: expected
            logical, dimension(:), intent(in) :: actual
        end subroutine

        module subroutine arrayEquals_assert_real(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            real, dimension(:), intent(in) :: expected
            real, dimension(:), intent(in) :: actual
            real,               intent(in) :: delta
        end subroutine

        !
        ! Equals - asserts that two values are equal.
        !
        module subroutine equals_assert_character(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            character, intent(in) :: expected
            character, intent(in) :: actual
        end subroutine

        module subroutine equals_assert_complex(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            complex, intent(in) :: expected
            complex, intent(in) :: actual
            real,    intent(in) :: delta
        end subroutine

        module subroutine equals_assert_double_precision(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            double precision, intent(in) :: expected
            double precision, intent(in) :: actual
            double precision, intent(in) :: delta
        end subroutine

        module subroutine equals_assert_integer(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            integer, intent(in) :: expected
            integer, intent(in) :: actual
        end subroutine

        module subroutine equals_assert_logical(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            logical, intent(in) :: expected
            logical, intent(in) :: actual
        end subroutine

        module subroutine equals_assert_real(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            real, intent(in) :: expected
            real, intent(in) :: actual
            real, intent(in) :: delta
        end subroutine

        !
        ! False - asserts that a condition is false.
        !
        module subroutine false_assert(condition, message)
            character(len=*), optional, intent(in) :: message
            logical, intent(in) :: condition
        end subroutine

        !
        ! NutNull - asserts that a pointer isn't null (associated).
        !
        module subroutine notNull_assert_character(pointer, message)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: pointer
        end subroutine

        module subroutine notNull_assert_complex(pointer, message)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: pointer
        end subroutine

        module subroutine notNull_assert_double_precision(pointer, message)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: pointer
        end subroutine

        module subroutine notNull_assert_integer(pointer, message)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: pointer
        end subroutine

        module subroutine notNull_assert_logical(pointer, message)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: pointer
        end subroutine

        module subroutine notNull_assert_real(pointer, message)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: pointer
        end subroutine

        !
        ! NotSame - asserts that two pointers do not refer to the same target.
        !
        module subroutine notSame_assert_character(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: unexpected
            character,        pointer,  intent(in) :: actual
        end subroutine

        module subroutine notSame_assert_complex(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: unexpected
            complex,          pointer, intent(in)  :: actual
        end subroutine

        module subroutine notSame_assert_double_precision(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: unexpected
            double precision, pointer,  intent(in) :: actual
        end subroutine

        module subroutine notSame_assert_integer(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: unexpected
            integer,          pointer,  intent(in) :: actual
        end subroutine

        module subroutine notSame_assert_logical(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: unexpected
            logical,          pointer,  intent(in) :: actual
        end subroutine

        module subroutine notSame_assert_real(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: unexpected
            real,             pointer,  intent(in) :: actual
        end subroutine

        !
        ! Null - asserts that a pointer is null (not associated).
        !
        module subroutine null_assert_character(pointer, message)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: pointer
        end subroutine

        module subroutine null_assert_complex(pointer, message)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: pointer
        end subroutine

        module subroutine null_assert_double_precision(pointer, message)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: pointer
        end subroutine

        module subroutine null_assert_integer(pointer, message)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: pointer
        end subroutine

        module subroutine null_assert_logical(pointer, message)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: pointer
        end subroutine

        module subroutine null_assert_real(pointer, message)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: pointer
        end subroutine

        !
        ! Same - asserts that two pointers refer to the same target.
        !
        module subroutine same_assert_character(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: unexpected
            character,        pointer,  intent(in) :: actual
        end subroutine

        module subroutine same_assert_complex(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: unexpected
            complex,          pointer, intent(in)  :: actual
        end subroutine

        module subroutine same_assert_double_precision(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: unexpected
            double precision, pointer,  intent(in) :: actual
        end subroutine

        module subroutine same_assert_integer(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: unexpected
            integer,          pointer,  intent(in) :: actual
        end subroutine

        module subroutine same_assert_logical(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: unexpected
            logical,          pointer,  intent(in) :: actual
        end subroutine

        module subroutine same_assert_real(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: unexpected
            real,             pointer,  intent(in) :: actual
        end subroutine

        !
        ! True - asserts that a condition is true.
        !
        module subroutine true_assert(condition, message)
            character(len=*), optional, intent(in) :: message
            logical, intent(in) :: condition
        end subroutine

        !
        ! Fail - fails a test with the given message.
        !
        module subroutine fail_assert(message)
            character(len=*), optional, intent(in) :: message
        end subroutine
    end interface

    interface
        !
        ! ArrayEquals - asserts that two arrays are equal.
        !
        module subroutine arrayEquals_expect_character(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            character, dimension(:), intent(in) :: expected
            character, dimension(:), intent(in) :: actual
        end subroutine

        module subroutine arrayEquals_expect_complex(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            complex, dimension(:), intent(in) :: expected
            complex, dimension(:), intent(in) :: actual
            real,                  intent(in) :: delta
        end subroutine

        module subroutine arrayEquals_expect_double_precision(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            double precision, dimension(:), intent(in) :: expected
            double precision, dimension(:), intent(in) :: actual
            double precision, intent(in)               :: delta
        end subroutine

        module subroutine arrayEquals_expect_integer(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            integer, dimension(:), intent(in) :: expected
            integer, dimension(:), intent(in) :: actual
        end subroutine

        module subroutine arrayEquals_expect_logical(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            logical, dimension(:), intent(in) :: expected
            logical, dimension(:), intent(in) :: actual
        end subroutine

        module subroutine arrayEquals_expect_real(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            real, dimension(:), intent(in) :: expected
            real, dimension(:), intent(in) :: actual
            real,               intent(in) :: delta
        end subroutine

        !
        ! Equals - asserts that two values are equal.
        !
        module subroutine equals_expect_character(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            character, intent(in) :: expected
            character, intent(in) :: actual
        end subroutine

        module subroutine equals_expect_complex(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            complex, intent(in) :: expected
            complex, intent(in) :: actual
            real,    intent(in) :: delta
        end subroutine

        module subroutine equals_expect_double_precision(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            double precision, intent(in) :: expected
            double precision, intent(in) :: actual
            double precision, intent(in) :: delta
        end subroutine

        module subroutine equals_expect_integer(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            integer, intent(in) :: expected
            integer, intent(in) :: actual
        end subroutine

        module subroutine equals_expect_logical(expected, actual, message)
            character(len=*), optional, intent(in) :: message

            logical, intent(in) :: expected
            logical, intent(in) :: actual
        end subroutine

        module subroutine equals_expect_real(expected, actual, delta, message)
            character(len=*), optional, intent(in) :: message

            real, intent(in) :: expected
            real, intent(in) :: actual
            real, intent(in) :: delta
        end subroutine

        !
        ! False - asserts that a condition is false.
        !
        module subroutine false_expect(condition, message)
            character(len=*), optional, intent(in) :: message
            logical, intent(in) :: condition
        end subroutine

        !
        ! NutNull - asserts that a pointer isn't null (associated).
        !
        module subroutine notNull_expect_character(pointer, message)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: pointer
        end subroutine

        module subroutine notNull_expect_complex(pointer, message)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: pointer
        end subroutine

        module subroutine notNull_expect_double_precision(pointer, message)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: pointer
        end subroutine

        module subroutine notNull_expect_integer(pointer, message)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: pointer
        end subroutine

        module subroutine notNull_expect_logical(pointer, message)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: pointer
        end subroutine

        module subroutine notNull_expect_real(pointer, message)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: pointer
        end subroutine

        !
        ! NotSame - asserts that two pointers do not refer to the same target.
        !
        module subroutine notSame_expect_character(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: unexpected
            character,        pointer,  intent(in) :: actual
        end subroutine

        module subroutine notSame_expect_complex(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: unexpected
            complex,          pointer, intent(in)  :: actual
        end subroutine

        module subroutine notSame_expect_double_precision(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: unexpected
            double precision, pointer,  intent(in) :: actual
        end subroutine

        module subroutine notSame_expect_integer(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: unexpected
            integer,          pointer,  intent(in) :: actual
        end subroutine

        module subroutine notSame_expect_logical(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: unexpected
            logical,          pointer,  intent(in) :: actual
        end subroutine

        module subroutine notSame_expect_real(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: unexpected
            real,             pointer,  intent(in) :: actual
        end subroutine

        !
        ! Null - asserts that a pointer is null (not associated).
        !
        module subroutine null_expect_character(pointer, message)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: pointer
        end subroutine

        module subroutine null_expect_complex(pointer, message)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: pointer
        end subroutine

        module subroutine null_expect_double_precision(pointer, message)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: pointer
        end subroutine

        module subroutine null_expect_integer(pointer, message)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: pointer
        end subroutine

        module subroutine null_expect_logical(pointer, message)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: pointer
        end subroutine

        module subroutine null_expect_real(pointer, message)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: pointer
        end subroutine

        !
        ! Same - asserts that two pointers refer to the same target.
        !
        module subroutine same_expect_character(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            character,        pointer,  intent(in) :: unexpected
            character,        pointer,  intent(in) :: actual
        end subroutine

        module subroutine same_expect_complex(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            complex,          pointer, intent(in)  :: unexpected
            complex,          pointer, intent(in)  :: actual
        end subroutine

        module subroutine same_expect_double_precision(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            double precision, pointer,  intent(in) :: unexpected
            double precision, pointer,  intent(in) :: actual
        end subroutine

        module subroutine same_expect_integer(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            integer,          pointer,  intent(in) :: unexpected
            integer,          pointer,  intent(in) :: actual
        end subroutine

        module subroutine same_expect_logical(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            logical,          pointer,  intent(in) :: unexpected
            logical,          pointer,  intent(in) :: actual
        end subroutine

        module subroutine same_expect_real(unexpected, actual, message)
            character(len=*), optional, intent(in) :: message
            real,             pointer,  intent(in) :: unexpected
            real,             pointer,  intent(in) :: actual
        end subroutine

        !
        ! True - asserts that a condition is true.
        !
        module subroutine true_expect(condition, message)
            character(len=*), optional, intent(in) :: message
            logical, intent(in) :: condition
        end subroutine

        !
        ! Fail - fails a test with the given message.
        !
        module subroutine fail_expect(message)
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
        module subroutine init_case(self, name)
            class(UnitCase), intent(in out)        :: self
            character(len=*), optional, intent(in) :: name
        end subroutine

        module subroutine clean_case(self)
            class(UnitCase), intent(in out) :: self
        end subroutine

        module subroutine add_case(self, procedure, name)
            class(UnitCase), intent(in out)               :: self
            procedure(UnitProcedure), pointer, intent(in) :: procedure
            character(len=*),        optional, intent(in) :: name
        end subroutine

        module subroutine run_case(self)
            class(UnitCase), intent(in out) :: self
        end subroutine
    end interface

    interface
        module subroutine init_suite(self, name)
            class(UnitSuite), intent(in out)       :: self
            character(len=*), optional, intent(in) :: name
        end subroutine

        module subroutine clean_suite(self)
            class(UnitSuite), intent(in out) :: self
        end subroutine

        module subroutine add_suite(self, case)
            class(UnitSuite), intent(in out)     :: self
            class(UnitCase), pointer, intent(in) :: case
        end subroutine

        module subroutine run_suite(self, resume)
            class(UnitSuite), intent(in)  :: self
            logical, optional, intent(in) :: resume
        end subroutine
    end interface

    interface
        module function getContext_context() result(instance)
            class(UnitContext), pointer :: instance
        end function

        module subroutine clean_context(self)
            class(UnitContext), intent(in out) :: self
        end subroutine

        module function getLogger_context(self) result(value)
            class(UnitContext), intent(in out) :: self
            class(UnitLogger), pointer         :: value
        end function
    end interface
end module
