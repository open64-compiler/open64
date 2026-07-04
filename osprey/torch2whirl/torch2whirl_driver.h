/*
 * Copyright (C) 2026 Open64 Project
 */

//-*-c++-*-

#ifndef torch2whirl_driver_INCLUDED
#define torch2whirl_driver_INCLUDED "torch2whirl_driver.h"

#include <iosfwd>
#include <optional>
#include <string>

enum class T2W_EXIT_CODE : int {
    SUCCESS = 0,
    NOT_IMPLEMENTED = 1,
    USAGE_ERROR = 2
};

class T2W_DRIVER {
public:
    T2W_DRIVER (std::ostream &out, std::ostream &err);

    T2W_EXIT_CODE Run (int argc, char **argv);

private:
    struct OPTIONS {
        std::string input_path;
        std::string output_path;
        bool show_help = false;
        bool show_version = false;
    };

    std::optional<OPTIONS> Parse_Options (int argc, char **argv);
    T2W_EXIT_CODE Run_Conversion (const OPTIONS &options);
    void Print_Usage (std::ostream &stream) const;
    void Print_Version () const;

    std::ostream &_out;
    std::ostream &_err;
};

#endif /* torch2whirl_driver_INCLUDED */
