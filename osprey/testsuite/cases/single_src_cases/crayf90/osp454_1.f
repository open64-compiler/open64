        PROGRAM t454
        TYPE TYPE_454
        COMPLEX :: T454_CPX
        INTEGER :: T454_INT
        END TYPE TYPE_454
        TYPE(TYPE_454), PARAMETER :: A = TYPE_454( (6.3,8.1),96721)
        COMPLEX :: C
        C = A%T454_CPX
        WRITE(6, "(2F6.3)") C
        END PROGRAM t454
!{ dg-output "6.300 8.100" }
