#!/usr/bin/env python3
# encoding: utf-8

#  Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

# Module Name: oclint_meta.py
# generate oclint meta json file

# std library
import json
import getopt
import sys
import os
import subprocess
import traceback

builtin_name = "BUILTIN"
builtin_disp_name = "Xcalibyte"
rulesets = [ "libcertc64.a", "libcertj.a" ]

def usage(prog):
    """
    print usage
    """
    print(prog + ": generate meta json file for Xcalibyte Vulnerability Static Analyzer (xvsa)")
    print("Usage: " + prog + " -h | -o <output_file>")
    print("  -h: print this usage")
    print("  -o <output_file>: write meta json info into <output_file>")


def append_rules(rs, r):
    """
    append r to rs
    """
    all_keys = map(lambda x : x["code"], rs)
    rs.extend(filter(lambda x : not x["code"] in all_keys, r))

def append_ruleset(rs_list, rs):
    """
    append rs to rs_list
    """
    for rsi in rs_list:
        if rsi["display_name"].upper() == builtin_name:
            rsi["display_name"] = builtin_disp_name;
        elif rsi["name"] == rs["name"]:
            append_rules(rsi["rules"], rs["rules"])
            return
        pass
    rs_list.append(rs)

# main
if __name__ == "__main__":
    # parse parameters
    prog_name = os.path.basename(sys.argv[0])
    script_dir = os.path.dirname(os.path.abspath(__file__))
    output_file = None
    try:
        options, args = getopt.getopt(sys.argv[1:], "ho:")
        for name, value in options:
            if name == "-h":
                usage(prog_name)
                sys.exit()
            elif name == "-o":
                output_file = value
    except getopt.GetoptError as e:
        sys.stderr.write("Error: " + str(e) + ". Please run `" + prog_name + " -h' for more details.\n")
        sys.exit(1)

    # check if output_file is specified
    if output_file is None:
        usage(prog_name)
        sys.exit(1)

    xvsa_path = os.path.join(script_dir, "xvsa")
    # check if xvsa is in the same directory
    if not os.path.isfile(xvsa_path):
        sys.stderr.write("Error: `" + script_dir + "' doesn't contain full xvsa binaries.\n")
        sys.exit(2)

    # get metamgr file path
    mm_pipe = subprocess.run([xvsa_path, "--print-prog-name=metamgr"],
                             stdout = subprocess.PIPE, stderr = subprocess.DEVNULL)
    if mm_pipe.returncode != 0:
        sys.stderr.write("Error: failed to run " + xvsa_path + " --print-prog-name=metamgr.\n")
        sys.exit(2)
    mm_path = mm_pipe.stdout.decode().strip()
    lib_path = os.path.dirname(mm_path)

    # check if script_dir contains full oclint binaries
    if not os.path.isfile(mm_path):
        sys.stderr.write("Error: not find metamgr at `" + mm_path + "'.\n")
        sys.exit(2)

    output_dir = os.path.dirname(output_file)
    if output_dir == "":
        output_dir = "."
    if not os.path.isdir(output_dir):
        sys.stderr.write("Error: `" + output_dir + "' doesn't exist.\n")
        sys.exit(2)

    try:
        rs_pipe = subprocess.run([mm_path, "builtin"], stdout = subprocess.PIPE, stderr = subprocess.DEVNULL)
        if rs_pipe.returncode != 0:
            sys.stderr.write("Error: failed to run `" + xvsa_path + " builtin'.\n")
            sys.exit(2)


        efj = json.loads(rs_pipe.stdout.decode())

        for rs_name in rulesets:
            rs_pipe = subprocess.run([mm_path, os.path.join(lib_path, rs_name)], stdout = subprocess.PIPE, stderr = subprocess.DEVNULL)
            if rs_pipe.returncode == 0:
                rfj = json.loads(rs_pipe.stdout.decode())
                append_ruleset(efj["rulesets"], rfj)
            else:
                sys.stderr.write("Error: failed to run `" + xvsa_path + " " + rs_name + "'.\n")
             
        with open(output_file, "w") as ofo:
            json.dump(efj, ofo,
                      default=lambda o: o.__dict__, indent=1, sort_keys=True)
    except Exception as e:
        traceback.print_exc()
        sys.exit(4)

    sys.exit(0)
