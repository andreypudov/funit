!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (FUnit) NotNullAsserts

    use Conditions

    implicit none

    character(len=*), parameter :: default = 'Not null condition'

contains
    module subroutine notNull_assert_character(pointer, message)
        character(len=*), optional, intent(in) :: message
        character,        pointer,  intent(in) :: pointer

        if (.not. notNull(pointer)) then
            call fail_assert(message, default)
        end if
    end subroutine

    module subroutine notNull_assert_complex(pointer, message)
        character(len=*), optional, intent(in) :: message
        complex,          pointer, intent(in)  :: pointer

        if (.not. notNull(pointer)) then
            call fail_assert(message, default)
        end if
    end subroutine

    module subroutine notNull_assert_double_precision(pointer, message)
        character(len=*), optional, intent(in) :: message
        double precision, pointer,  intent(in) :: pointer

        if (.not. notNull(pointer)) then
            call fail_assert(message, default)
        end if
    end subroutine

    module subroutine notNull_assert_integer(pointer, message)
        character(len=*), optional, intent(in) :: message
        integer,          pointer,  intent(in) :: pointer

        if (.not. notNull(pointer)) then
            call fail_assert(message, default)
        end if
    end subroutine

    module subroutine notNull_assert_logical(pointer, message)
        character(len=*), optional, intent(in) :: message
        logical,          pointer,  intent(in) :: pointer

        if (.not. notNull(pointer)) then
            call fail_assert(message, default)
        end if
    end subroutine

    module subroutine notNull_assert_real(pointer, message)
        character(len=*), optional, intent(in) :: message
        real,             pointer,  intent(in) :: pointer

        if (.not. notNull(pointer)) then
            call fail_assert(message, default)
        end if
    end subroutine
end submodule
