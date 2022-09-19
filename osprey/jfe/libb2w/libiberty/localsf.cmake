SET(IBERTY_C_SRC
    alloca.c
    argv.c
    choose-temp.c
    concat.c
    cp-demangle.c
    cplus-dem.c
    d-demangle.c
    dyn-string.c
    fdmatch.c
    fibheap.c
    floatformat.c
    fnmatch.c
    getopt.c
    getopt1.c
    getpwd.c
    getruntime.c
    hex.c
    hashtab.c
    lbasename.c
    mkstemps.c
    md5.c
    objalloc.c
    obstack.c
    partition.c
    pexecute.c
    physmem.c
    rust-demangle.c
    safe-ctype.c
    spaces.c
    splay-tree.c
    strerror.c
    strsignal.c
    xatexit.c
    xexit.c
    xmalloc.c
    xmemdup.c
    xstrdup.c
    xstrerror.c
)

SET(IBERTY_INC_SRC
    ansidecl.h
    demangle.h
    dyn-string.h
    fibheap.h
    floatformat.h
    hashtab.h
    libiberty.h
    objalloc.h
    partition.h
    safe-ctype.h
    sort.h
    splay-tree.h
    ternary.h
)

foreach(ONE_C_FILE IN LISTS IBERTY_C_SRC)
  list(APPEND IBERTY_SRC ${LIBIBERTY_DIR}/${ONE_C_FILE})
endforeach()


foreach(ONE_C_FILE IN LISTS IBERTY_INC_SRC)
#  list(APPEND IBERTY_SRC ${LIBIBERTY_INC_DIR}/${ONE_C_FILE})
endforeach()
