! 
!  Copyright (C) 2008. PathScale, LLC. All Rights Reserved.
!
#if defined(PRIVATE_IMPLEMENTED)	/* TBD */
# define PRIVATE_MEMBER private
#else
# define PRIVATE_MEMBER
#endif /* PRIVATE_IMPLEMENTED */
#if defined(TARG_IA32) || (_MIPS_SZPTR==32)
# define LONG_KIND 4
# define PTR_KIND 4 /* Integer capable of holding address */
# define LONG_DOUBLE_KIND -1 /* Fortran has no 12-byte real */
# define LONG_DOUBLE_COMPLEX_KIND -1 /* Fortran has no 12-byte real */
#elif defined(TARG_X8664) || (_MIPS_SZPTR==64)
# define LONG_KIND 8
# define PTR_KIND 8 /* Integer capable of holding address */
# define LONG_DOUBLE_KIND -1 /* Fortran has no 16-byte real */
# define LONG_DOUBLE_COMPLEX_KIND -1 /* Fortran has no 16-byte real */
#else
# error "check kind= constants for this target"
#endif /* defined(TARG_IA32) */
#define ANYTYPE integer

module iso_c_binding

/* For backward compatibility, skip this module if the pathf90 used to compile
 * it can't implement BIND(C) and the extensions to !dir$ ignore_tkr needed to
 * make the module work correctly */
#if ! defined(NOBIND)
  public

  ! Fortran "kind=" values corresponding to C data types 
  integer, parameter :: c_int = 4
  integer, parameter :: c_short = 2
  integer, parameter :: c_long = LONG_KIND
  integer, parameter :: c_long_long = 8
  integer, parameter :: c_signed_char = 1
  integer, parameter :: c_size_t = LONG_KIND
  integer, parameter :: c_int8_t = 1
  integer, parameter :: c_int16_t = 2
  integer, parameter :: c_int32_t = 4
  integer, parameter :: c_int64_t = 8
  integer, parameter :: c_int_least8_t = 1
  integer, parameter :: c_int_least16_t = 2
  integer, parameter :: c_int_least32_t = 4
  integer, parameter :: c_int_least64_t = 8
  integer, parameter :: c_int_fast8_t = 1
  integer, parameter :: c_int_fast16_t = LONG_KIND
  integer, parameter :: c_int_fast32_t = LONG_KIND
  integer, parameter :: c_int_fast64_t = 8
  integer, parameter :: c_intmax_t = 8
  integer, parameter :: c_intptr_t = LONG_KIND
  integer, parameter :: c_float = 4
  integer, parameter :: c_double = 8
  integer, parameter :: c_long_double = LONG_DOUBLE_KIND
  integer, parameter :: c_float_complex = 4
  integer, parameter :: c_double_complex = 8
  integer, parameter :: c_long_double_complex = LONG_DOUBLE_COMPLEX_KIND
  integer, parameter :: c_bool = 1
  integer, parameter :: c_char = 1

  character(len=1, kind=c_char), parameter :: c_null_char = char(0)
  character(len=1, kind=c_char), parameter :: c_alert = char(7)
  character(len=1, kind=c_char), parameter :: c_backspace = char(8)
  character(len=1, kind=c_char), parameter :: c_form_feed = char(12)
  character(len=1, kind=c_char), parameter :: c_new_line = char(10)
  character(len=1, kind=c_char), parameter :: c_carriage_return = char(13)
  character(len=1, kind=c_char), parameter :: c_horizontal_tab = char(9)
  character(len=1, kind=c_char), parameter :: c_vertical_tab = char(11)

  type, bind(c) :: c_ptr
    PRIVATE_MEMBER
    integer(kind=PTR_KIND) :: px
  end type c_ptr

  type, bind(c) :: c_funptr
    PRIVATE_MEMBER
    integer(kind=PTR_KIND) :: px
  end type c_funptr

  type(c_ptr), parameter :: c_null_ptr = c_ptr(0)
  type(c_funptr), parameter :: c_null_funptr = c_funptr(0)

  ! 15.1.2.1 c_associated

  private :: c_associated_ptr, c_associated_funptr
  interface c_associated

    logical function c_associated_ptr(c_ptr_1, c_ptr_2)
      import :: c_ptr
      type(c_ptr), intent(in), value :: c_ptr_1
      type(c_ptr), intent(in), optional :: c_ptr_2
    end function c_associated_ptr

    logical function c_associated_funptr(c_ptr_1, c_ptr_2)
      import :: c_funptr
      type(c_funptr), intent(in), value :: c_ptr_1
      type(c_funptr), intent(in), optional :: c_ptr_2
    end function c_associated_funptr

  end interface c_associated

  ! 15.1.2.2 c_f_pointer
  ! Requires type checking in front end code in s_intrin.c

  interface c_f_pointer
    subroutine c_f_pointera(cptr, fptr, shape)
      import :: c_ptr
      type(c_ptr), intent(in) :: cptr
!dir$ ignore_tkr fptr
      ANYTYPE, pointer, dimension(:,:,:,:,:,:,:), intent(out) :: fptr
      integer, dimension(*), intent(in) :: shape
    end subroutine c_f_pointera
    subroutine c_f_pointers(cptr, fptr)
      import :: c_ptr
      type(c_ptr), intent(in) :: cptr
!dir$ ignore_tkr fptr
      ANYTYPE, pointer, intent(out) :: fptr
    end subroutine c_f_pointers
  end interface c_f_pointer

  ! 15.1.2.3 c_f_procpointer not implementable till we have procedure pointers

  ! 15.1.2.4 c_funloc not completely implementable till we have procedure
  ! pointers; requires type checking in front end code in s_intrin.c

  interface c_funloc
    type(c_funptr) function c_funloc(x)
      import :: c_funptr
      external :: x
!dir$ ignore_tkr x
    end function c_funloc
  end interface c_funloc

  ! 15.1.2.5 c_loc requires type checking in front end code in s_intrin.c

  interface c_loc
    type(c_ptr) function c_loc(x)
      import c_ptr
!dir$ ignore_tkr x
      ANYTYPE :: x
    end function c_loc
  end interface c_loc

!TBD contains

!TBD  subroutine c_f_procpointer(cptr, fptr)
!TBD    type(c_funptr), intent(in) :: cptr
!TBD    procedure(), pointer, intent(out) :: fptr
!TBD    fptr => cptr%px;
!TBD  end subroutine c_f_procpointer

#endif /* NOBIND */
end module iso_c_binding
