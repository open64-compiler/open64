ARGS=""
for i in $*
do
        case $1 in
                --srcdir=*)
                        srcdir=`expr "x$1" : 'x[^=]*=\(.*\)'`
                        ;;
                --prefix=*)
                        prefix=`expr "x$1" : 'x[^=]*=\(.*\)'`
                        ;;
#                CC=*)
#                        compiler=`expr "x$1" : 'x[^=]*=\(.*\)'`
#                        ;;
                *)
                        ARGS="$ARGS $1"
        esac
        shift 1
done

export CC="$CYGNUS_CONFIGURE_COMPILER $CYGNUS_CONFIGURE_CFLAGS"
$srcdir/configure  --prefix=$prefix --srcdir=$srcdir --host=$CYGNUS_CONFIGURE_HOST --target=$CYGNUS_CONFIGURE_TARG
