#aux_source_directory(${LIBCMPLERS} WGEN_SRC_FOUND)


SET(CFILES
    array_alloc.c
    make_depend.c
)


foreach(ONE_C_FILE IN LISTS CFILES)
  list(APPEND LIB_CMPLR_SRC ${LIB_CMPLR_DIR}/${ONE_C_FILE})
endforeach()





