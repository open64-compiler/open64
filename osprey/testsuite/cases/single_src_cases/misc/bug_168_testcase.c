//CMD: $(CXX) -L /usr/lib $(SOURCE) -o $(TARGET) -lm

#include <math.h>

typedef struct
{
  float           x_pos;
  float           y_pos;
  float           rotation_percent;
} visible_host_t;

void f(float radius, unsigned long n_hosts)
{
  unsigned long   c;
  float                           face_theta;
  visible_host_t  *vh;

  vh->x_pos = radius * sin((float)(c - 0.5) * face_theta);
  vh->y_pos = radius * cos((float)(c - 0.5) * face_theta);
}
int main()
{
  return 0;
}
