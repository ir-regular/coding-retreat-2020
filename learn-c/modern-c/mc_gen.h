#ifndef MC_GEN
#define MC_GEN 1

unsigned *mc_gen_rand_unsigned(size_t len);
int *mc_gen_rand_int(size_t len);
double *mc_gen_rand_double(size_t len);

void mc_init_rand_unsigned(size_t len, unsigned list[len]);
void mc_init_rand_int(size_t len, int list[len]);
void mc_init_rand_double(size_t len, double list[len]);

#endif