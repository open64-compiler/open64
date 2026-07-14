/*
 * Copyright (C) 2026 Open64 Project
 */

//-*-c++-*-

#include "torch2whirl_driver.h"

#include <iostream>

int
main (int argc, char **argv)
{
    T2W_DRIVER driver(std::cout, std::cerr);

    return static_cast<int>(driver.Run (argc, argv));
}
