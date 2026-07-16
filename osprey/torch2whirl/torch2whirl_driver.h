/*
 * Copyright (C) 2026 Open64 Project
 */

//-*-c++-*-

#ifndef torch2whirl_driver_INCLUDED
#define torch2whirl_driver_INCLUDED "torch2whirl_driver.h"

#include <iosfwd>
#include <optional>
#include <string>
#include <vector>

enum class T2W_EXIT_CODE : int {
    SUCCESS = 0,
    CONVERSION_ERROR = 1,
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
        std::string entry = "forward";
        std::string backend = "mock";
        std::string model_factory = "create_model";
        std::vector<std::string> sample_inputs;
        bool single_pu = false;
        bool show_help = false;
        bool show_version = false;
    };

    std::optional<OPTIONS> Parse_Options (int argc, char **argv);
    T2W_EXIT_CODE Run_Conversion (const OPTIONS &options);
    T2W_EXIT_CODE Run_Python_Cli (const OPTIONS &options);
    bool Parse_Value_Option (int argc, char **argv, int *index,
                             const char *option_name, std::string *value);
    std::string Build_Pythonpath () const;
    void Print_Usage (std::ostream &stream) const;
    void Print_Version () const;

    std::ostream &_out;
    std::ostream &_err;
};

#endif /* torch2whirl_driver_INCLUDED */
