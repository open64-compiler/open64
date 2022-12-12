*aftermath_genmove_under_control;
aftermath_genmove_distance;
aftermath_genmove() {
  int pos;
  for (; pos; pos++)
    if (aftermath_genmove_distance)
      aftermath_genmove_under_control[pos] = 0;
    else
      aftermath_genmove_under_control[pos] = 1;
}
