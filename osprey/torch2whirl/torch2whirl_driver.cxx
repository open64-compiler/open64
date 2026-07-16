/*
 * Copyright (C) 2026 Open64 Project
 */

//-*-c++-*-

#include "torch2whirl_driver.h"

#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string_view>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#ifndef TORCH2WHIRL_PYTHON
#define TORCH2WHIRL_PYTHON "python3"
#endif

#ifndef TORCH2WHIRL_PYTHONPATH
#define TORCH2WHIRL_PYTHONPATH ""
#endif

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

        if (T2W_Option_Is (arg, "--single-pu")) {
            options.single_pu = true;
            continue;
        }

        if (T2W_Option_Is (arg, "-o") || T2W_Option_Is (arg, "--output")) {
            if (!Parse_Value_Option (argc, argv, &i, argv[i],
                                     &options.output_path))
                return std::nullopt;
            continue;
        }

        if (T2W_Option_Is (arg, "--entry")) {
            if (!Parse_Value_Option (argc, argv, &i, "--entry",
                                     &options.entry))
                return std::nullopt;
            continue;
        }

        if (T2W_Option_Is (arg, "--sample-input")) {
            std::string sample_input;
            if (!Parse_Value_Option (argc, argv, &i, "--sample-input",
                                     &sample_input))
                return std::nullopt;
            options.sample_inputs.push_back (sample_input);
            continue;
        }

        if (T2W_Option_Is (arg, "--backend")) {
            if (!Parse_Value_Option (argc, argv, &i, "--backend",
                                     &options.backend))
                return std::nullopt;
            continue;
        }

        if (T2W_Option_Is (arg, "--model-factory")) {
            if (!Parse_Value_Option (argc, argv, &i, "--model-factory",
                                     &options.model_factory))
                return std::nullopt;
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

    if (options.sample_inputs.empty()) {
        _err << "torch2whirl: missing --sample-input\n";
        return T2W_EXIT_CODE::USAGE_ERROR;
    }

    return Run_Python_Cli (options);
}

T2W_EXIT_CODE
T2W_DRIVER::Run_Python_Cli (const OPTIONS &options)
{
    std::vector<std::string> args;
    args.push_back (TORCH2WHIRL_PYTHON);
    args.push_back ("-m");
    args.push_back ("open64_dsc.cli");
    args.push_back (options.input_path);
    args.push_back ("--entry");
    args.push_back (options.entry);
    args.push_back ("--model-factory");
    args.push_back (options.model_factory);
    args.push_back ("--backend");
    args.push_back (options.backend);
    if (options.single_pu)
        args.push_back ("--single-pu");
    for (const std::string &sample_input : options.sample_inputs) {
        args.push_back ("--sample-input");
        args.push_back (sample_input);
    }
    args.push_back ("-o");
    args.push_back (options.output_path);

    std::vector<char *> argv;
    for (std::string &arg : args)
        argv.push_back (arg.data());
    argv.push_back (nullptr);

    const std::string pythonpath = Build_Pythonpath ();
    pid_t pid = fork ();
    if (pid < 0) {
        _err << "torch2whirl: failed to launch Python frontend: "
             << std::strerror (errno) << '\n';
        return T2W_EXIT_CODE::CONVERSION_ERROR;
    }

    if (pid == 0) {
        if (!pythonpath.empty())
            setenv ("PYTHONPATH", pythonpath.c_str(), 1);
        execvp (args[0].c_str(), argv.data());
        std::cerr << "torch2whirl: failed to exec " << args[0]
                  << ": " << std::strerror (errno) << '\n';
        _exit (127);
    }

    int status = 0;
    if (waitpid (pid, &status, 0) < 0) {
        _err << "torch2whirl: failed to wait for Python frontend: "
             << std::strerror (errno) << '\n';
        return T2W_EXIT_CODE::CONVERSION_ERROR;
    }

    if (WIFEXITED (status)) {
        int exit_status = WEXITSTATUS (status);
        if (exit_status == 0)
            return T2W_EXIT_CODE::SUCCESS;
        _err << "torch2whirl: Python frontend exited with status "
             << exit_status << '\n';
        return T2W_EXIT_CODE::CONVERSION_ERROR;
    }

    if (WIFSIGNALED (status)) {
        _err << "torch2whirl: Python frontend terminated by signal "
             << WTERMSIG (status) << '\n';
    } else {
        _err << "torch2whirl: Python frontend did not exit normally\n";
    }
    return T2W_EXIT_CODE::CONVERSION_ERROR;
}

bool
T2W_DRIVER::Parse_Value_Option (int argc, char **argv, int *index,
                                const char *option_name, std::string *value)
{
    if (index == nullptr || value == nullptr)
        return false;

    if (*index + 1 >= argc) {
        _err << "torch2whirl: " << option_name << " requires a value\n";
        return false;
    }

    *value = argv[++(*index)];
    if (value->empty()) {
        _err << "torch2whirl: " << option_name << " value must not be empty\n";
        return false;
    }
    return true;
}

std::string
T2W_DRIVER::Build_Pythonpath () const
{
    std::string pythonpath = TORCH2WHIRL_PYTHONPATH;
    const char *existing = std::getenv ("PYTHONPATH");
    if (existing != nullptr && existing[0] != '\0') {
        if (!pythonpath.empty())
            pythonpath += ":";
        pythonpath += existing;
    }
    return pythonpath;
}

void
T2W_DRIVER::Print_Usage (std::ostream &stream) const
{
    stream << "usage: torch2whirl [--help] [--version] "
           << "[--entry <name>] [--model-factory <name>] "
           << "[--single-pu] "
           << "[--backend mock|native] --sample-input shape:<dims> "
           << "-o|--output <output.whirl> <input.py>\n";
}

void
T2W_DRIVER::Print_Version () const
{
    _out << "torch2whirl 0.1\n";
}
