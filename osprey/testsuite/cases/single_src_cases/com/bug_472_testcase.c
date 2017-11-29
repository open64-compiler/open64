//CFLAGS: -O3 -c
//This is the testcase of bug #472, which is fixed in Rev 2108

const unsigned long int zuf_randmax = 16777216;
typedef struct   {
        unsigned long int u[607];
}
zuf_state_t;
inline unsigned long int zuf_get (void *vstate) {
        long int i, j, k, l, m;
        double x, y;
        long int ii, jj;
        zuf_state_t *state = (zuf_state_t *) vstate;
        for (ii = 0; ii < 607; ++ii) {
                for (jj = 1; jj <= 24; ++jj) {
                        m = i * j % 179 * k % 179;
                        if (l * m % 64 >= 32) {
                                x += y;
                        }
                }
                state->u[ii] = (unsigned long int) (x * zuf_randmax);
        }
}

