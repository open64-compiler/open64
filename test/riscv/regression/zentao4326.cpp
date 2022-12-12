class Speedometer {
  double current_simtime;
  void addEvent(double);
};
void Speedometer::addEvent(double t) { current_simtime = t; }
