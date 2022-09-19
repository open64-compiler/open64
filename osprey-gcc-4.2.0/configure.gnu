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
                --disable-multilib)
                        GCC_ARGS="$GCC_ARGS $1"
                        ;;
                *)
                        ARGS="$ARGS $1"
        esac
        shift 1
done

export CC="$GCC_CONFIGURE_COMPILER"
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

$srcdir/configure  --prefix=$prefix --with-gnu-as --with-gnu-ld --enable-languages=c,c++ --disable-bootstrap --disable-libmudflap --disable-libssp --disable-checking --enable-threads=posix --enable-tls --with-system-zlib --enable-__cxa_atexit $GCC_ARGS --srcdir=$srcdir --host=$GCC_CONFIGURE_HOST --target=$GCC_CONFIGURE_TARG
