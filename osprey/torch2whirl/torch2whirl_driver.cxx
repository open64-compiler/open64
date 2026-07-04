/*
 * Copyright (C) 2026 Open64 Project
 */

//-*-c++-*-

#include "torch2whirl_driver.h"

#include <iostream>
#include <string_view>

static bool
T2W_Option_Is (std::string_view arg, std::string_view option)
{
    return arg == option;
}

T2W_DRIVER::T2W_DRIVER (std::ostream &out, std::ostream &err) :
    _out(out),
    _err(err)
{
}

T2W_EXIT_CODE
T2W_DRIVER::Run (int argc, char **argv)
{
    std::optional<OPTIONS> options = Parse_Options (argc, argv);

    if (!options.has_value())
        return T2W_EXIT_CODE::USAGE_ERROR;

    if (options->show_help) {
        Print_Usage (_out);
        return T2W_EXIT_CODE::SUCCESS;
    }

    if (options->show_version) {
        Print_Version ();
        return T2W_EXIT_CODE::SUCCESS;
    }

    return Run_Conversion (*options);
}

std::optional<T2W_DRIVER::OPTIONS>
T2W_DRIVER::Parse_Options (int argc, char **argv)
{
    OPTIONS options;

    for (int i = 1; i < argc; ++i) {
        std::string_view arg(argv[i] == nullptr ? "" : argv[i]);

        if (T2W_Option_Is (arg, "--help") || T2W_Option_Is (arg, "-h")) {
            options.show_help = true;
            return options;
        }

        if (T2W_Option_Is (arg, "--version")) {
            options.show_version = true;
            return options;
        }

        if (T2W_Option_Is (arg, "-o")) {
            if (i + 1 >= argc) {
                _err << "torch2whirl: -o requires a path\n";
                return std::nullopt;
            }
            options.output_path = argv[++i];
            continue;
        }

        if (!arg.empty() && arg.front() == '-') {
            _err << "torch2whirl: unknown option: " << arg << '\n';
            Print_Usage (_err);
            return std::nullopt;
        }

        if (!options.input_path.empty()) {
            _err << "torch2whirl: multiple input files are not supported yet\n";
            return std::nullopt;
        }
        options.input_path = std::string(arg);
    }

    return options;
}

T2W_EXIT_CODE
T2W_DRIVER::Run_Conversion (const OPTIONS &options)
{
    if (options.input_path.empty()) {
        _err << "torch2whirl: missing input Python file\n";
        return T2W_EXIT_CODE::USAGE_ERROR;
    }

    if (options.output_path.empty()) {
        _err << "torch2whirl: missing output WHIRL path\n";
        return T2W_EXIT_CODE::USAGE_ERROR;
    }

    /*
     * Placeholder for the next staged slice:
     *   Python capture -> native DSL builder -> binary WHIRL image writer.
     */
    _err << "torch2whirl: conversion is not implemented yet "
         << "(input=" << options.input_path
         << " output=" << options.output_path << ")\n";
    return T2W_EXIT_CODE::NOT_IMPLEMENTED;
}

void
T2W_DRIVER::Print_Usage (std::ostream &stream) const
{
    stream << "usage: torch2whirl [--help] [--version] -o <output.whirl> "
           << "<input.py>\n";
}

void
T2W_DRIVER::Print_Version () const
{
    _out << "torch2whirl 0.1\n";
}
