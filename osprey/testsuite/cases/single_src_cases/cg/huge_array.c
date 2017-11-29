//It's the test case for BUG #306
//OBJ
#define	NX	1024
#define	NY	512
#define	NZ	512

void evolve(double u0[NZ][NY][NX], double u1[NZ][NY][NX]);
void checksum(double u1[NZ][NY][NX]);

int main(int argc, char **argv) {
    static double u0[NZ][NY][NX];
    static double u1[NZ][NY][NX];
    static double u2[NZ][NY][NX];
    
    evolve(u0, u1);
    checksum(u2);
}
