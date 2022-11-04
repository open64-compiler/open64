ARGS=""
GCC_ARGS=""
for i in $*
do
        case $1 in
                --srcdir=*)
                        srcdir=`expr "x$1" : 'x[^=]*=\(.*\)'`
                        ;;
                --prefix=*)
                        prefix=`expr "x$1" : 'x[^=]*=\(.*\)'`
                        ;;
                *)
                        ARGS="$ARGS $1"
        esac
        shift 1
done

if [ "$BUILD_PRODUCT" = "XCALCC" ]
then
        export CC="riscv64-linux-gnu-gcc -mabi=lp64d"
else
        export CC="$GCC_CONFIGURE_COMPILER"
fi
export CFLAGS="$GCC_CONFIGURE_CFLAGS"

if [ "`uname -m | sed -e s/i.86/i386/`" = "i386" ]
then
        export FLAGS_FOR_TARGET="-m32"
fi

if [ "$BUILD_PRODUCT" = "MASTIFF" ]
then
        export prefix=$prefix/compat/gcc-4
else
        export prefix=$prefix/open64-gcc-4.2.0
fi

$srcdir/configure  --prefix=$prefix --disable-gas --disable-gprof --disable-gold --disable-libstdcxx --disable-bootstrap --disable-libada --disable-libssp --disable-checking --enable-threads=posix --enable-plugins --enable-targets=all --srcdir=$srcdir --host=$GCC_CONFIGURE_HOST
