class rvectort {
public:
  float x, y, z;
};
struct createwayinfot {
  rvectort finishp;
};
struct wayinfot {
  rvectort *wayarp;
};
class wayobj {
  createwayinfot createwayinfo;
  void gety(float);
  rvectort getpoint();
  bool createway(const rvectort &, const rvectort &, bool, wayinfot &);
};
bool wayobj::createway(const rvectort &startp, const rvectort &finishp,
                       bool flcorrect, wayinfot &wayinfo) {
  gety(finishp.z);
  if (flcorrect)
    createwayinfo.finishp = getpoint();
  wayinfo.wayarp[0] = startp;
}
